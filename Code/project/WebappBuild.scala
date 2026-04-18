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
import Dependencies.{Jetty => JettyDep, _}
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
        webappMacroJVM, webappMacroJS,
        webappBaseJVM, webappBaseJS,
        webappBaseTestJVM, webappBaseTestJS,
        webappMemberJVM, webappMemberJS,
        webappMemberTestJVM, webappMemberTestJS,
        webappServerLogicJVM, webappServerLogicJS,
        webappSampleDataJVM, webappSampleDataJS,
        webappClientPublicJVM, webappClientPublicJS,
        webappClientLoaders,
        webappClientHome,
        webappClientWwApi, webappClientWw,
        webappClientProject,
        webappSsrJVM, webappSsrJS,
        webappServer)
      .settings(
        jsSizesFast := jsSizesTask(Stage.FastOpt).value,
        jsSizesFull := jsSizesTask(Stage.FullOpt).value)

  lazy val webappMacroJVM = webappMacro.jvm
  lazy val webappMacroJS  = webappMacro.js
  lazy val webappMacro =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-macro"))
      .configureBoth(
        Common.macroModuleSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(baseUtil)
      .depsForBoth(
        boopickle ++ Microlibs.compileTime ++ Monocle.core ++
        providedScope(Scala.library) ++
        testScope(utest))
      .configureJvm(_.dependsOn(baseDb))
      .depsForJvm(postgresql)

  lazy val webappBaseJVM = webappBase.jvm
  lazy val webappBaseJS  = webappBase.js
  lazy val webappBase =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-base"))
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(baseUtil)
      .depsForBoth(Monocle.macros ++ Nyaya.prop ++ boopickle)
      .depsForJs(React.most ++ scalajsDom)
      .settings(
        Compile / unmanagedSourceDirectories += baseDirectory.value / ".." / Frontend.scala)
      .jsSettings(
        genLastValueMemoBoilerplate := GenLastValueMemoBoilerplate(sourceDirectory.value / "main" / "scala"))

  lazy val webappBaseTestJVM = webappBaseTest.jvm
  lazy val webappBaseTestJS  = webappBaseTest.js
  lazy val webappBaseTest =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-base-test"))
      .configureBoth(Common.testModuleSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(_.enablePlugins(JSDependenciesPlugin), Common.jsSettings(UseNode))
      .dependsOn(baseTest, webappBase)
      .depsForBoth(utest ++ Nyaya.test)
      .depsForJs(
        React.test ++ ScalaCSS.react ++
        TestState.nyaya ++ TestState.domZipperSizzle ++ TestState.scalajsReact)
      .jsSettings(
        parallelExecution := false, // I don't know why this is needed
        Test / jsDependencies += ProvidedJS / "webapp-base-test.js")

  lazy val webappMemberJVM = webappMember.jvm
  lazy val webappMemberJS  = webappMember.js
  lazy val webappMember =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-member"))
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(webappBase, webappMacro)
      .depsForBoth(shapeless ++ parboiled)
      .depsForBoth(Circe.main % Provided) // Provided because for now, want to ensure JSON stuff isn't part of frontend
      .depsForJs(ScalaCSS.react)

  lazy val webappMemberTestJVM = webappMemberTest.jvm
  lazy val webappMemberTestJS  = webappMemberTest.js
  lazy val webappMemberTest =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-member-test"))
      .configureBoth(Common.testModuleSettings)
      .configureJvm(Common.jvmSettings)
      .configureJvm(_.dependsOn(webappSampleDataJVM))
      .configureJs(_.enablePlugins(JSDependenciesPlugin), Common.jsSettings(UseNodeAdvanced))
      .dependsOn(webappBaseTest, webappMember)
      .depsForBoth(Circe.main)
      .depsForBoth(ScalaCSS.core % Test) // for NaturalOrdering
      .jsSettings(
        parallelExecution := false, // Faster
        Test / jsDependencies += ProvidedJS / "webapp-member-test.js")

  lazy val webappSampleDataJVM = webappSampleData.jvm
  lazy val webappSampleDataJS  = webappSampleData.js
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
  private def memberSpa: Project => Project =
    _.enablePlugins(ScalaJSPlugin, JSDependenciesPlugin)
      .configure(Common.jsSettings(UseNode))
      .dependsOn(webappMemberJS, webappMemberTestJS % Test, webappServerLogicJS % Test)
      .settings(Test / jsDependencies += ProvidedJS / "webapp-client-test.js")

  lazy val webappClientPublicJVM = webappClientPublic.jvm
  lazy val webappClientPublicJS  = webappClientPublic.js
  lazy val webappClientPublic =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-client-public"))
      .configureJvm(Common.jvmSettings)
      .configureJs(_.enablePlugins(JSDependenciesPlugin), Common.jsSettings(UseNode))
      .dependsOn(webappBase, webappBaseTest % Test)
      .jsSettings(Test / jsDependencies += ProvidedJS / "webapp-client-test.js")

  lazy val webappClientLoaders =
    project
      .in(file("webapp-client-loaders"))
      .enablePlugins(ScalaJSPlugin)
      .configure(Common.jsSettings(NoTests))
      .dependsOn(webappMemberJS)

  lazy val webappClientHome =
    project
      .in(file("webapp-client-home"))
      .configure(memberSpa)
      .dependsOn(webappClientLoaders)
      .depsForJs(ScalaCSS.react)

  lazy val webappClientWwApi =
    project
      .in(file("webapp-client-ww-api"))
      .enablePlugins(ScalaJSPlugin)
      .configure(Common.jsSettings(NoTests))
      .dependsOn(webappMemberJS)
      .depsForJs(
        boopickle ++ scalajsDom ++
        testScope(utest))

  lazy val webappClientWw =
    project
      .in(file("webapp-client-ww"))
      .enablePlugins(ScalaJSPlugin)
      .configure(Common.jsSettings(UseNode))
      .dependsOn(webappClientWwApi, webappMemberTestJS % Test)
      .depsForJs(
        boopickle ++ scalajsDom ++
        testScope(utest))
      .settings(
        Compile / scalacOptions -= "-Xno-forwarders", // https://github.com/scala-js/scala-js/issues/4030
        scalaJSUseMainModuleInitializer := true,
        Compile / mainClass := Some("shipreq.webapp.client.ww.Main"))

  object WebappClientProject {
    val parallelism = 4
  }

  lazy val webappClientProject =
    project
      .in(file("webapp-client-project"))
      .configure(memberSpa)
      .dependsOn(webappClientWwApi, webappClientLoaders)
      .depsForJs(ScalaCSS.react ++ scalajsDom ++ shapeless ++ Nyaya.prop ++ parboiled)
      .settings(Test / test / tags += CustomTags.WebappClientProjectTest -> 1)

  lazy val webappSsr =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-ssr"))
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(webappMember, webappClientPublic, baseTest % Test)
      .depsForBoth(ScalaGraal.extBoopickle ++ testScope(utest))

  lazy val webappSsrJVM = webappSsr.jvm
    .deps(ScalaGraal.coreJs ++ ScalaGraal.extPrometheus ++ scalaXml)
    .settings(Compile / unmanagedResources += Def.taskDyn {
      val stage = (webappSsrJS / Compile / scalaJSStage).value
      val task = stageKey(stage)
      Def.task((webappSsrJS / Compile / task).value.data)
    }.value)

  lazy val webappSsrJS = webappSsr.js
    .dependsOn(webappClientLoaders)
    .settings(
      scalaJSLinkerConfig ~= { _.withSourceMap(emitSourceMapsValue) },
      Compile / fastOptJS / artifactPath := (crossTarget.value / "webapp-ssr.js"),
      Compile / fullOptJS / artifactPath := (crossTarget.value / "webapp-ssr.js"),
    )

  lazy val webappServerLogicJVM = webappServerLogic.jvm
  lazy val webappServerLogicJS  = webappServerLogic.js
  lazy val webappServerLogic =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("webapp-server-logic"))
      .configureJvm(
        Common.jvmSettings,
        _.dependsOn(taskmanApiLogic, webappClientPublicJVM, webappSsrJVM))
      .configureJs(Common.jsSettings(UseNode))
      .dependsOn(webappMember)
      .dependsOn(baseTest % Test, webappMemberTest % Test)
      .depsForJvm(scaffeine ++ commonsText)
      .depsForBoth(testScope(utest ++ Nyaya.test))

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
        Jetty / javaOptions ++= Seq(
          "-Dshipreq.scalajs.public="    + Frontend.scalaJsDevPathPublic,
          "-Dshipreq.scalajs.home="      + Frontend.scalaJsDevPathHome,
          "-Dshipreq.scalajs.project="   + Frontend.scalaJsDevPathProject,
          "-Dshipreq.scalajs.webWorker=" + Frontend.scalaJsDevPathWw
        ),
        webappPostProcess := {
          implicit val log = streams.value.log

          val baseDirectoryValue     = baseDirectory.value
          val jsWebappClientPublicJs = (webappClientPublicJS / Compile / scalaJSLinkedFile).value
          val jsWebappClientHome     = (webappClientHome     / Compile / scalaJSLinkedFile).value
          val jsWebappClientProject  = (webappClientProject  / Compile / scalaJSLinkedFile).value
          val jsWebappClientWw       = (webappClientWw       / Compile / scalaJSLinkedFile).value
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
      .configure(DockerEnv.test)
      .dependsOn(webappBaseTestJVM % Test)
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
      .configure(DockerCfg.settingsFor("webapp"))
      .deps(JettyDep.distTarGz % DockerDeps)
      .settings(
        cleanFiles += baseDirectory.value / "target",
        DockerDeps / classpathTypes += "tar.gz", // for jetty-distribution
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
          Jetty / containerArgs ++= "--classes" :: (DockerEnv.dev.resDir("webapp", baseDirectory.value) / "resources").absolutePath :: Nil,
          Jetty / javaOptions   ++= DockerEnv.dev.javaOptions("webapp", baseDirectory.value),
          Jetty / start          := (Jetty / start).dependsOn(DockerEnv.dev.devEnvStart).value)

    def definition: Project => Project = _
      .enablePlugins(JettyPlugin, WarPlugin, DockerPlugin)
      .dependsOn(baseDb, baseOps, taskmanApi, webappServerLogicJVM)
      .dependsOn(webappMemberTestJVM % Test)
      .dependsOn(webappServerLogicJVM % "test->test")
      .deps(
        Lift.webkit ++  scalaXml ++ SLF4J.jcl ++ commonsText ++ Nyaya.gen ++ Logback.withPlugins ++ JJWT.all ++
        Prometheus.client ++ Prometheus.hotspot ++ Prometheus.servlet ++ Prometheus.logback ++ redisson ++
        JettyDep.http ++
        providedScope(JettyDep.javaxServletApi ++ JettyDep.javaxWebsocketApi ++ JettyDep.servlets) ++
        testScope(utest ++ Lift.testkit ++ commonsIo) ++
        (JettyDep.webapp % Test))
      .configure(
        Common.jvmSettings,
        assetSettings,
        testSettings,
        connectToDockerDevEnv,
        dockerSettings)
      .settings(
        scalacOptions -= "-Yno-generic-signatures", // Without this, snippets break. LiveTest confirms.
        Jetty / containerLibs := JettyDep.devRun(JVM),
        Jetty / javaOptions ++= List(
          "-XX:+UseJVMCINativeLibrary",
          // "-XX:+BootstrapJVMCI",
          //"-XX:-TieredCompilation",
          //"-XX:+EagerJVMCI",
          // jprofilerAgent(wait = false),
          "-Xmx1g",
          "-XX:+UseG1GC"), // TODO use everywhere then including tests
        initialCommands += consoleCmds,
        Compile / console / fullClasspath += file("src/main/webapp"), // So templates can be loaded from console
      )
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
    report((webappClientPublicJS / moduleName).value, (webappClientPublicJS / Compile / stageKey(stage)).value)
    report((webappClientHome     / moduleName).value, (webappClientHome     / Compile / stageKey(stage)).value)
    report((webappClientProject  / moduleName).value, (webappClientProject  / Compile / stageKey(stage)).value)
    report((webappClientWw       / moduleName).value, (webappClientWw       / Compile / stageKey(stage)).value)
    println()
  }

}
