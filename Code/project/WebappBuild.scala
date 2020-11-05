import sbt._
import sbt.Keys._
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin.autoImport._
import org.scalajs.sbtplugin.{ScalaJSPlugin, Stage}
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport._
import sbtdocker.DockerPlugin
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import Common._
import Dependencies._
import LibDependency.JVM
import ShipReqBuild._
import TaskmanBuild._

/** The user-facing app.
  */
object WebappBuild {

  lazy val genLastValueMemoBoilerplate = TaskKey[File]("genLastValueMemoBoilerplate")

  object Frontend {
    val mode = if (releaseMode) "prod" else "dev"
    val dist = s"../frontend/dist/$mode"
    val local = s"$dist/local"
    val scala = s"$dist/scala"
    val serve = s"$dist/serve"

    def manifestPath(name: String) = Def.setting {
      val lines = IO.readLines(file(s"${baseDirectory.value}/$scala/AbstractAssetManifest.scala"))
      val List(line) = lines.filter(_.contains(s" $name ="))
      "(?<=\"/)(.+)(?=\")".r.findFirstIn(line).get
    }
    def scalaJsDevPath(name: String) = s"/j/$name.js"
    def scalaJsDevPathPublic  = scalaJsDevPath("public")
    def scalaJsDevPathHome    = scalaJsDevPath("home")
    def scalaJsDevPathProject = scalaJsDevPath("project")
    def scalaJsDevPathWw      = scalaJsDevPath("ww")
  }

  lazy val webapp =
    project
      .configure(Common.jvmSettings)
      .aggregate(
        webappMacroJvm, webappMacroJs,
        webappBaseJvm, webappBaseJs,
        webappBaseTestJvm, webappBaseTestJs,
        webappMemberJvm, webappMemberJs,
        webappMemberTestJvm, webappMemberTestJs,
        webappServerLogicJvm, webappServerLogicJs,
        webappSampleDataJvm, webappSampleDataJs,
        webappClientPublicJvm, webappClientPublicJs,
        webappClientLoaders,
        webappClientHome,
        webappClientWwApi, webappClientWw,
        webappClientProject,
        webappSsrJvm, webappSsrJs,
        webappServer)
      .settings(
        jsSizesFast := jsSizesTask(Stage.FastOpt).value,
        jsSizesFull := jsSizesTask(Stage.FullOpt).value)

  lazy val webappMacroJvm = webappMacro.jvm
  lazy val webappMacroJs  = webappMacro.js
  lazy val webappMacro =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-macro"))
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
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-base"))
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(baseUtil)
      .depsForBoth(Monocle.macros ++ Nyaya.prop ++ boopickle)
      .depsForJs(React.most ++ scalajsDom)
      .settings(
        unmanagedSourceDirectories in Compile += baseDirectory.value / ".." / Frontend.scala)
      .jsSettings(
        genLastValueMemoBoilerplate := GenLastValueMemoBoilerplate(sourceDirectory.value / "main" / "scala"))

  lazy val webappBaseTestJvm = webappBaseTest.jvm
  lazy val webappBaseTestJs  = webappBaseTest.js
  lazy val webappBaseTest =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-base-test"))
      .configureBoth(Common.testModuleSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(_.enablePlugins(JSDependenciesPlugin), Common.jsSettings(UsePhantomJs))
      .dependsOn(baseTest, webappBase)
      .depsForBoth(μTest ++ Nyaya.test)
      .depsForJs(
        React.test ++ ScalaCSS.react ++
        TestState.nyaya ++ TestState.domZipperSizzle ++ TestState.scalajsReact)
      .jsSettings(
        parallelExecution := false, // I don't know why this is needed
        jsDependencies in Test += ProvidedJS / "webapp-base-test.js")

  lazy val webappMemberJvm = webappMember.jvm
  lazy val webappMemberJs  = webappMember.js
  lazy val webappMember =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-member"))
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(webappBase, webappMacro)
      .depsForBoth(shapeless ++ parboiled)
      .depsForBoth(Circe.main % Provided) // Provided because for now, want to ensure JSON stuff isn't part of frontend
      .depsForJs(ScalaCSS.react)

  lazy val webappMemberTestJvm = webappMemberTest.jvm
  lazy val webappMemberTestJs  = webappMemberTest.js
  lazy val webappMemberTest =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-member-test"))
      .configureBoth(Common.testModuleSettings)
      .configureJvm(Common.jvmSettings)
      .configureJvm(_.dependsOn(webappSampleDataJvm))
      .configureJs(_.enablePlugins(JSDependenciesPlugin), Common.jsSettings(UsePhantomJs))
      .dependsOn(webappBaseTest, webappMember)
      .depsForBoth(Circe.main)
      .jsSettings(
        parallelExecution := false, // I don't know why this is needed
        jsDependencies in Test += ProvidedJS / "webapp-member-test.js")

  lazy val webappSampleDataJvm = webappSampleData.jvm
  lazy val webappSampleDataJs  = webappSampleData.js
  lazy val webappSampleData =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-sampledata"))
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(webappMember)
      .depsForBoth(Circe.main ++ Microlibs.testUtil)

  /** Settings for client SPA projects.
    *
    * ScalaCss is deliberately missing because it's too heavy for the public SPA.
    */
  private lazy val memberSpa: Project => Project =
    _.enablePlugins(ScalaJSPlugin, JSDependenciesPlugin)
      .configure(Common.jsSettings(UsePhantomJs))
      .dependsOn(webappMemberJs, webappMemberTestJs % Test, webappServerLogicJs % Test)
      .settings(jsDependencies in Test += ProvidedJS / "webapp-client-test.js")

  lazy val webappClientPublicJvm = webappClientPublic.jvm
  lazy val webappClientPublicJs  = webappClientPublic.js
  lazy val webappClientPublic =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-client-public"))
      .configureJvm(Common.jvmSettings)
      .configureJs(_.enablePlugins(JSDependenciesPlugin), Common.jsSettings(UsePhantomJs))
      .dependsOn(webappBase, webappBaseTest % Test)
      .jsSettings(jsDependencies in Test += ProvidedJS / "webapp-client-test.js")

  lazy val webappClientLoaders =
    project
      .in(file("webapp-client-loaders"))
      .enablePlugins(ScalaJSPlugin)
      .configure(Common.jsSettings(NoTests))
      .dependsOn(webappMemberJs)

  lazy val webappClientHome =
    project
      .in(file("webapp-client-home"))
      .configure(memberSpa)
      .configure(Common.jsSettings(UseNode)) // PhantomJS crashes
      .dependsOn(webappClientLoaders)
      .depsForJs(ScalaCSS.react)

  lazy val webappClientWwApi =
    project
      .in(file("webapp-client-ww-api"))
      .enablePlugins(ScalaJSPlugin)
      .configure(Common.jsSettings(UsePhantomJs))
      .dependsOn(webappMemberJs)
      .depsForJs(
        boopickle ++ scalajsDom ++
        testScope(μTest))

  lazy val webappClientWw =
    project
      .in(file("webapp-client-ww"))
      .enablePlugins(ScalaJSPlugin)
      .configure(Common.jsSettings(UseNode))
      .dependsOn(webappClientWwApi, webappMemberTestJs % Test)
      .depsForJs(
        boopickle ++ scalajsDom ++
        testScope(μTest))
      .settings(
        scalacOptions in Compile -= "-Xno-forwarders", // https://github.com/scala-js/scala-js/issues/4030
        scalaJSUseMainModuleInitializer := true,
        mainClass in Compile := Some("shipreq.webapp.client.ww.Main"))

  lazy val webappClientProject =
    project
      .in(file("webapp-client-project"))
      .configure(memberSpa)
      .dependsOn(webappClientWwApi, webappClientLoaders)
      .depsForJs(ScalaCSS.react ++ scalajsDom ++ shapeless ++ Nyaya.prop ++ parboiled)

  lazy val webappSsr =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-ssr"))
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(webappMember, webappClientPublic, baseTest % Test)
      .depsForBoth(ScalaGraal.extBoopickle ++ testScope(μTest))

  lazy val webappSsrJvm = webappSsr.jvm
    .deps(ScalaGraal.coreJs ++ ScalaGraal.extPrometheus ++ scalaXml)
    .settings(unmanagedResources in Compile += Def.taskDyn {
      val stage = (scalaJSStage in Compile in webappSsrJs).value
      val task = stageKey(stage)
      Def.task((task in Compile in webappSsrJs).value.data)
    }.value)

  lazy val webappSsrJs = webappSsr.js
    .dependsOn(webappClientLoaders)
    .settings(
      scalaJSLinkerConfig ~= { _.withSourceMap(emitSourceMapsValue) },
      artifactPath in (Compile, fastOptJS) := (crossTarget.value / "webapp-ssr.js"),
      artifactPath in (Compile, fullOptJS) := (crossTarget.value / "webapp-ssr.js"))

  lazy val webappServerLogicJvm = webappServerLogic.jvm
  lazy val webappServerLogicJs  = webappServerLogic.js
  lazy val webappServerLogic =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-server-logic"))
      .configureJvm(
        Common.jvmSettings,
        _.dependsOn(taskmanApiLogic, webappClientPublicJvm, webappSsrJvm))
      .configureJs(Common.jsSettings(UsePhantomJs))
      .dependsOn(webappMember)
      .dependsOn(baseTest % Test, webappMemberTest % Test)
      .depsForJvm(scaffeine ++ commonsText)
      .depsForBoth(testScope(μTest ++ Nyaya.test))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val webappServer =
    project
      .in(file("webapp-server"))
      .configure(Server.definition)

  object Server {
    import com.earldouglas.xwp._
    import ContainerPlugin.start
    import ContainerPlugin.autoImport._
    import JettyPlugin    .autoImport._
    import WebappPlugin   .autoImport._

    lazy val DockerDeps = config("dockerdeps")

    def assetSettings: Project => Project =
      _.settings(
        javaOptions in Jetty ++= Seq(
          "-Dshipreq.scalajs.public="    + Frontend.scalaJsDevPathPublic,
          "-Dshipreq.scalajs.home="      + Frontend.scalaJsDevPathHome,
          "-Dshipreq.scalajs.project="   + Frontend.scalaJsDevPathProject,
          "-Dshipreq.scalajs.webWorker=" + Frontend.scalaJsDevPathWw
        ),
        webappPostProcess := {
          implicit val log = streams.value.log

          val baseDirectoryValue     = baseDirectory.value
          val jsWebappClientPublicJs = (scalaJSLinkedFile in Compile in webappClientPublicJs).value
          val jsWebappClientHome     = (scalaJSLinkedFile in Compile in webappClientHome    ).value
          val jsWebappClientProject  = (scalaJSLinkedFile in Compile in webappClientProject ).value
          val jsWebappClientWw       = (scalaJSLinkedFile in Compile in webappClientWw      ).value
          val pathScalaJsPathPublic  = Frontend.scalaJsDevPathPublic
          val pathScalaJsPathHome    = Frontend.scalaJsDevPathHome
          val pathScalaJsPathProject = Frontend.scalaJsDevPathProject
          val pathScalaJsPathWw      = Frontend.scalaJsDevPathWw
          (target: File) => {

            // Copy Scala.JS output
            def copyScalaJs(f: Attributed[File], to: String): Unit = {
              fileSync(f.data, target / to, mandatory = true)
              if (Common.emitSourceMapsValue) {
                val src = file(f.data.absolutePath + ".map")
                val tgt = target / to.replaceFirst("[^/]+$", src.getName)
                fileSync(src, tgt, mandatory = true)
              }
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

    def definition: Project => Project = _
      .enablePlugins(JettyPlugin, WarPlugin, DockerPlugin)
      .dependsOn(baseDb, baseOps, taskmanApi, webappServerLogicJvm)
      .dependsOn(webappMemberTestJvm % Test)
      .deps(
        scalaz ++ Lift.webkit ++  scalaXml ++ SLF4J.jcl ++ commonsText ++ Nyaya.gen ++ Logback.withPlugins ++ JJWT.all ++
        Prometheus.client ++ Prometheus.hotspot ++ Prometheus.servlet ++ Prometheus.logback ++ redisson ++
        LibJetty.http ++
        providedScope(LibJetty.javaxServletApi ++ LibJetty.javaxWebsocketApi ++ LibJetty.servlets) ++
        testScope(μTest ++ Lift.testkit ++ commonsIo) ++
        (LibJetty.webapp % Test))
      .configure(
        Common.jvmSettings,
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
          // jprofilerAgent(wait = false),
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
    report((moduleName in webappClientPublicJs).value, (stageKey(stage) in Compile in webappClientPublicJs).value)
    report((moduleName in webappClientHome    ).value, (stageKey(stage) in Compile in webappClientHome    ).value)
    report((moduleName in webappClientProject ).value, (stageKey(stage) in Compile in webappClientProject ).value)
    report((moduleName in webappClientWw      ).value, (stageKey(stage) in Compile in webappClientWw      ).value)
    println()
  }

}
