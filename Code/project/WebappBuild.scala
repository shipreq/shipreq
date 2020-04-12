import sbt.{project => _, _}, Keys._
import org.scalajs.core.tools.io.{FileVirtualJSFile, VirtualJSFile}
import org.scalajs.sbtplugin.{ScalaJSPlugin, Stage}
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{crossProject => _, CrossType => _, _}
import org.scalajs.sbtplugin.ScalaJSPluginInternal.stageKeys
import sbtcrossproject.CrossPlugin.autoImport.{crossProject => _, _}
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import sbtdocker.DockerPlugin, DockerPlugin.autoImport._
import Common._
import Dependencies._
import LibDependency.JVM
import ShipReqBuild._
import TaskmanBuild._

/** The user-facing app.
  */
object WebappBuild {

  object Frontend {
    val mode = if (releaseMode) "prod" else "dev"
    val dist = s"../frontend/dist/$mode"
    val local = s"$dist/local"
    val scala = s"$dist/scala"
    val serve = s"$dist/serve"

    def manifestPath(name: String) = Def.setting {
      val lines = IO.readLines(file(s"${baseDirectory.value}/$scala/AssetManifest.scala"))
      val List(line) = lines.filter(_.contains(s" $name ="))
      "(?<=\"/)(.+)(?=\")".r.findFirstIn(line).get
    }
    def scalaJsPath(name: String) = Def.setting {
      manifestPath(s"webappClient${name}Js").value
    }
    def scalaJsPathPublic  = scalaJsPath("Public")
    def scalaJsPathHome    = scalaJsPath("Home")
    def scalaJsPathProject = scalaJsPath("Project")
    def scalaJsPathWw      = scalaJsPath("Ww")
  }

  lazy val webapp =
    project("webapp")
      .configure(Common.jvmSettings)
      .aggregate(
        webappMacroJvm, webappBaseJvm, webappBaseMemberJvm, webappServerLogicJvm, webappBaseTestJvm,
        webappMacroJs , webappBaseJs , webappBaseMemberJs , webappServerLogicJs , webappBaseTestJs ,
        webappClientPublicJvm, webappClientPublicJs,
        webappClientLoaders,
        webappClientHome,
        webappClientWwApi, webappClientWw, webappClientProject,
        webappSsrJvm, webappSsrJs,
        webappServer)
      .settings(
        jsSizesFast := jsSizesTask(Stage.FastOpt).value,
        jsSizesFull := jsSizesTask(Stage.FullOpt).value,
        addCommandAlias("jsSizes", ";jsSizesFast;jsSizesFull"))

  lazy val webappMacroJvm = webappMacro.jvm
  lazy val webappMacroJs  = webappMacro.js
  lazy val webappMacro =
    crossProject("webapp-macro")
      .configureBoth(
        Common.macroModuleSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(baseUtil)
      .depsForBoth(
        boopickle ++ Monocle.core ++
        providedScope(Scala.library) ++
        testScope(μTest))
      .configureJvm(_.dependsOn(baseDb))
      .depsForJvm(postgresql)

  lazy val webappBaseJvm = webappBase.jvm
  lazy val webappBaseJs  = webappBase.js
  lazy val webappBase =
    crossProject("webapp-base")
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(baseUtil, webappMacro)
      .depsForBoth(Monocle.macros ++ Nyaya.prop ++ boopickle)
      .depsForJs(React.most ++ scalajsDom)
      .settings(unmanagedSourceDirectories in Compile += baseDirectory.value / ".." / Frontend.scala)

  lazy val webappBaseMemberJvm = webappBaseMember.jvm
  lazy val webappBaseMemberJs  = webappBaseMember.js
  lazy val webappBaseMember =
    crossProject("webapp-base-member")
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(webappBase)
      .depsForBoth(shapeless ++ parboiled ++ (Circe.main % Provided))
      .depsForJs(ScalaCSS.react)

  lazy val webappBaseTestJvm = webappBaseTest.jvm
  lazy val webappBaseTestJs  = webappBaseTest.js
  lazy val webappBaseTest =
    crossProject("webapp-base-test")
      .configureBoth(Common.testModuleSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(UsePhantomJs))
      .dependsOn(baseTest, webappBaseMember)
      .depsForBoth(μTest ++ Nyaya.test ++ Circe.main)
      .depsForJs(
        React.test ++ ScalaCSS.react ++
        TestState.nyaya ++ TestState.domZipperSizzle ++ TestState.scalajsReact)
      .jsSettings(jsDependencies in Test += ProvidedJS / "webapp-base-test.js")

  /** Settings for client SPA projects.
    *
    * ScalaCss is deliberately missing because it's too heavy for the public SPA.
    */
  private lazy val clientSpa: Project => Project =
    _.enablePlugins(ScalaJSPlugin)
      .configure(Common.jsSettings(UsePhantomJs))
      .dependsOn(webappBaseJs, webappBaseTestJs % Test, webappServerLogicJs % Test)
      .settings(jsDependencies in Test += ProvidedJS / "webapp-client-test.js")

  lazy val webappClientPublicJvm = webappClientPublic.jvm
  lazy val webappClientPublicJs  = webappClientPublic.js
  lazy val webappClientPublic =
    crossProject("webapp-client-public")
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(UsePhantomJs))
      .dependsOn(webappBase, webappBaseTest % Test)
      .jsSettings(jsDependencies in Test += ProvidedJS / "webapp-client-test.js")

  lazy val webappClientLoaders =
    project("webapp-client-loaders")
      .enablePlugins(ScalaJSPlugin)
      .configure(Common.jsSettings(NoTests))
      .dependsOn(webappBaseMemberJs)

  lazy val webappClientHome =
    project("webapp-client-home")
      .configure(clientSpa)
      .configure(Common.jsSettings(UseNode)) // PhantomJS crashes
      .dependsOn(webappClientLoaders)
      .depsForJs(ScalaCSS.react)

  lazy val webappClientWwApi =
    project("webapp-client-ww-api")
      .enablePlugins(ScalaJSPlugin)
      .configure(Common.jsSettings(UsePhantomJs))
      .dependsOn(webappBaseMemberJs)
      .depsForJs(
        boopickle ++ scalajsDom ++
        testScope(μTest))

  lazy val webappClientWw =
    project("webapp-client-ww")
      .enablePlugins(ScalaJSPlugin)
      .configure(Common.jsSettings(UsePhantomJs))
      .dependsOn(webappClientWwApi, webappBaseTestJs % Test)
      .depsForJs(
        boopickle ++ scalajsDom ++
        testScope(μTest))
      .settings(
        scalaJSUseMainModuleInitializer := true,
        mainClass in Compile := Some("shipreq.webapp.client.ww.Main"))

  lazy val webappClientProject =
    project("webapp-client-project")
      .configure(clientSpa)
      .dependsOn(webappClientWwApi, webappClientLoaders)
      .depsForJs(ScalaCSS.react ++ scalajsDom ++ shapeless ++ Nyaya.prop ++ parboiled ++ React.scalaz)

  lazy val webappSsr =
    crossProject("webapp-ssr")
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(webappBaseMember, webappClientPublic, baseTest % Test)
      .depsForBoth(ScalaGraal.extBoopickle ++ testScope(μTest))

  lazy val webappSsrJvm = webappSsr.jvm
    .deps(ScalaGraal.util ++ ScalaGraal.extPrometheus ++ scalaXml)
    .settings(unmanagedResources in Compile += Def.taskDyn {
      val stage = (scalaJSStage in Compile in webappSsrJs).value
      val task = stageKeys(stage)
      Def.task((task in Compile in webappSsrJs).value.data)
    }.value)

  lazy val webappSsrJs = webappSsr.js
    .dependsOn(webappClientLoaders)
    .settings(
      emitSourceMaps := false,
      artifactPath in (Compile, fastOptJS) := (crossTarget.value / "webapp-ssr.js"),
      artifactPath in (Compile, fullOptJS) := (crossTarget.value / "webapp-ssr.js"))

  lazy val webappServerLogicJvm = webappServerLogic.jvm
  lazy val webappServerLogicJs  = webappServerLogic.js
  lazy val webappServerLogic =
    crossProject("webapp-server-logic")
      .configureJvm(
        Common.jvmSettings,
        _.dependsOn(taskmanApiLogic, webappClientPublicJvm, webappSsrJvm))
      .configureJs(Common.jsSettings(UsePhantomJs))
      .dependsOn(webappBaseMember)
      .dependsOn(baseTest % Test, webappBaseTest % Test)
      .depsForJvm(scaffeine ++ Circe.main ++ commonsText)
      .depsForBoth(testScope(μTest ++ Nyaya.test))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val webappServer =
    project("webapp-server").configure(Server.definition)

  object Server {
    import com.earldouglas.xwp._
    import ContainerPlugin.start
    import ContainerPlugin.autoImport._
    import JettyPlugin    .autoImport._
    import WebappPlugin   .autoImport._

//    lazy val copyClientJs = taskKey[Unit]("Copies required webapp client resources.")

    lazy val DockerDeps = config("dockerdeps")

    def assetSettings: Project => Project =
      _.settings(
        webappPostProcess := {
          implicit val log = streams.value.log

          val baseDirectoryValue     = baseDirectory.value
          val jsWebappClientPublicJs = (scalaJSLinkedFile in Compile in webappClientPublicJs).value
          val jsWebappClientHome     = (scalaJSLinkedFile in Compile in webappClientHome    ).value
          val jsWebappClientProject  = (scalaJSLinkedFile in Compile in webappClientProject ).value
          val jsWebappClientWw       = (scalaJSLinkedFile in Compile in webappClientWw      ).value
          val pathScalaJsPathPublic  = Frontend.scalaJsPathPublic .value
          val pathScalaJsPathHome    = Frontend.scalaJsPathHome   .value
          val pathScalaJsPathProject = Frontend.scalaJsPathProject.value
          val pathScalaJsPathWw      = Frontend.scalaJsPathWw     .value
          (target: File) => {

            // Copy Scala.JS output
            def copyScalaJs(jsf: VirtualJSFile, to: String): Unit =
              jsf match {
                case f: FileVirtualJSFile =>
                  fileSync(f.file, target / to, mandatory = true)
                case other =>
                  sys.error("Unsupported virtual file type: " + other)
              }
            copyScalaJs(jsWebappClientPublicJs, pathScalaJsPathPublic )
            copyScalaJs(jsWebappClientHome    , pathScalaJsPathHome   )
            copyScalaJs(jsWebappClientProject , pathScalaJsPathProject)
            copyScalaJs(jsWebappClientWw      , pathScalaJsPathWw     )

            // Copy frontend assets
            val assetSrc = baseDirectoryValue / Frontend.serve
            log.info(s"Copying ${assetSrc.getCanonicalPath} → ${target.absolutePath}")
            IO.copyDirectory(assetSrc, target, overwrite = true)
          }
        }
      )

    def testSettings = (_: Project)
      .configure(DockerEnv.test.required)
      .dependsOn(webappBaseTestJvm % Test)
      .settings(inConfig(Test)(Seq(
        fork                         := true,
        javaOptions                  += "-Drun.mode=test",
        javaOptions                  += s"-Dshipreq.assets=${(baseDirectory.value / Frontend.serve).absolutePath}",
        unmanagedResourceDirectories += baseDirectory.value / Frontend.serve,
        unmanagedResourceDirectories += baseDirectory.value / "src/main/webapp",
        parallelExecution            := false) // Due to LiveTest
      ): _*)

    def consoleCmds = "def initLift() = {val b = new bootstrap.liftweb.Boot; b.configureLift; b}"

    def dockerSettings = (_: Project)
      .enablePlugins(DockerPlugin)
      .configs(DockerDeps)
      .configure(Docker.settingsFor("webapp"))
      .deps(LibJetty.distTarGz % DockerDeps)
      .settings(
        cleanFiles += baseDirectory.value / "target",
        classpathTypes in DockerDeps += "tar.gz", // for jetty-distribution
        webappWebInfClasses := false)

    /** Does the following on the `up` command:
      *
      * - Starts up a subset of `envs/dev/docker-compose.yml` (see [[DockerEnv.dev]] for exact services)
      * - Adds `envs/dev/webapp` to the runtime classpath
      * - Loads the env specified in docker-compose into system properties
      * - Overrides certain env values to use external hosts and ports
      */
    def connectToDockerDevEnv: Project => Project =
      _.configure(DockerEnv.dev.commands)
        .settings(
          containerArgs in Jetty ++= "--classes" :: (DockerEnv.dev.resDir("webapp", baseDirectory.value) / "resources").absolutePath :: Nil,
          javaOptions   in Jetty ++= DockerEnv.dev.javaOptions("webapp", baseDirectory.value),
          start         in Jetty  := (start in Jetty).dependsOn(DockerEnv.dev.devEnvStart).value)

    def webappCmdAliases: Project => Project = {
      val w = "webapp-server"
      addCommandAliases(
        "js" -> s"$w/webappPrepare",               // compile JavaScript
        "up" -> s";$w/jetty:stop ;$w/jetty:start", // webapp Up
        "d"  -> s"$w/jetty:stop")                  // webapp Down
    }

    def definition: Project => Project = _
      .enablePlugins(JettyPlugin, WarPlugin, DockerPlugin)
      .dependsOn(baseDb, baseOps, taskmanApi, webappServerLogicJvm)
      .deps(
        scalaz ++ Lift.webkit ++  scalaXml ++ SLF4J.jcl ++ commonsText ++ Nyaya.gen ++ Logback.withPlugins ++ JJWT.all ++
        Prometheus.client ++ Prometheus.hotspot ++ Prometheus.servlet ++ Prometheus.logback ++ redisson ++
        providedScope(LibJetty.javaxServletApi ++ LibJetty.javaxWebsocketApi) ++
        testScope(μTest ++ Lift.testkit ++ commonsIo) ++
        (LibJetty.webapp % Test))
      .configure(
        Common.jvmSettings,
        webappCmdAliases,
        assetSettings,
        testSettings,
        connectToDockerDevEnv,
        dockerSettings)
      .settings(
        scalacOptions -= "-Yno-generic-signatures", // Without this, snippets break. LiveTest confirms.
        containerLibs in Jetty := LibJetty.devRun(JVM),
        javaOptions in Jetty ++= List(
          "-XX:+UseJVMCINativeLibrary",
          // "-XX:+BootstrapJVMCI",
          //"-XX:-TieredCompilation",
          //"-XX:+EagerJVMCI",
          // "-agentpath:/opt/jprofiler10/bin/linux-x64/libjprofilerti.so=port=8849,nowait",
          "-Xmx1g",
          "-XX:+UseG1GC"), // TODO use everywhere then including tests
        initialCommands += consoleCmds,
        fullClasspath in console in Compile += file("src/main/webapp")) // So templates can be loaded from console
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val jsSizesFast = TaskKey[Unit]("jsSizesFast", "Print JS sizes (using fastOptJS).")
  val jsSizesFull = TaskKey[Unit]("jsSizesFull", "Print JS sizes (using fullOptJS).")

  def jsSizesTask(stage: Stage) = Def.task[Unit] {
    def report(name: String, f: Attributed[File]): Unit =
      printf("%,11d → %,9d - %s\n", f.data.length, gzipLength(f.data), name)
    val header = s"JS $stage Sizes"
    println()
    println(header)
    println("=" * header.length)
    report((moduleName in webappClientPublicJs).value, (stageKeys(stage) in Compile in webappClientPublicJs).value)
    report((moduleName in webappClientHome    ).value, (stageKeys(stage) in Compile in webappClientHome    ).value)
    report((moduleName in webappClientProject ).value, (stageKeys(stage) in Compile in webappClientProject ).value)
    report((moduleName in webappClientWw      ).value, (stageKeys(stage) in Compile in webappClientWw      ).value)
    println()
  }

}
