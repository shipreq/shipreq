import sbt._
import Keys._
import Common.ExportsTestLib
import Common.Functions._
import Common.Values.releaseMode
import Deps._
import org.scalajs.sbtplugin.ScalaJSPlugin

object ShipReq extends Build {

  // Declare modules
  lazy val root = Root.project

  lazy val base         = Base.project
  lazy val baseDb       = Base.Db.project
  lazy val baseTest     = Base.Test.project
  lazy val baseUtil     = Base.Util.project
  lazy val baseUtilSjs  = Base.UtilSjs.project

  lazy val webapp         = Webapp.project
  lazy val webappBase     = Webapp.Base.project
  lazy val webappBaseTest = Webapp.BaseTest.project
  lazy val webappClient   = Webapp.Client.project
  lazy val webappServer   = Webapp.Server.project

  lazy val taskman             = Taskman.project
  lazy val taskmanApi          = Taskman.Api.project
  lazy val taskmanApiLogic     = Taskman.Api.Logic.project
  lazy val taskmanApiImpl      = Taskman.Api.Impl.project
  lazy val taskmanServer       = Taskman.Server.project
  lazy val taskmanServerLogic  = Taskman.Server.Logic.project
  lazy val taskmanServerImpl   = Taskman.Server.Impl.project
  lazy val taskmanServerSchema = Taskman.Server.Schema.project

  lazy val benchmark     = Benchmark.project
  lazy val benchmarkBase = Benchmark.Base.project
  lazy val benchmarkJvm  = Benchmark.Jvm.project
  lazy val benchmarkJs   = Benchmark.Js.project

  lazy val utils = Utils.project

  sealed trait Module {
    def project: Project
    def dir: String

    def deps: MS = MS.empty
    protected def depScope(s: String)(ms: MS): MS = ms % s
    protected def depScope(c: Configuration)(ms: MS): MS = depScope(c.name)(ms)
    protected def testScope = depScope("test") _
    protected def providedScope = depScope("provided") _

    def ideSettings = IdeSettings(this)

    def commonSettings: Project => Project =
      _.configure(Common.settings, ideSettings)

    def moduleToSettings: Project => Project =
      _.settings(libraryDependencies ++= deps.ms)

    protected def typicalProject: Project =
      Project(dir, file(dir)).configure(commonSettings, moduleToSettings).settings(name := dir)

    protected def umbrellaOf(ps: ProjectReference*): Project =
      typicalProject.aggregate(ps: _*).dependsOn(ps.map{p => p: ClasspathDep[ProjectReference]}: _*)
  }

  // ===================================================================================================================
  object Root extends Module {
    def dir = "."
    override def project = Project("root", file(dir))
      .configure(commonSettings, Common.useHiddenTargetDir)
      .aggregate(base, webapp, taskman, benchmark, utils)
  }

  // ===================================================================================================================
  object Base extends Module {
    val dir = "base"
    override def project = typicalProject
      .aggregate(baseUtilSjs, baseUtil, baseDb, baseTest) // not umbrella cos it shouldn't dependOn

    // ----------------------------------------------------
    object UtilSjs extends Module {
      val dir = "base-util-sjs"

      override def deps =
        Scalaz.effect ++ Nyaya.jvm.core ++ testScope(μTest.jvm)

      override def project = typicalProject
        .configure(Common.utestOnJvm, Common.addSourceDialectJvm)
    }

    // ----------------------------------------------------
    object Util extends Module {
      val dir = "base-util"

      override def deps =
        SLF4J.api ++ Scalaz.effect ++
        providedScope(logback ++ jodaTime) ++
        testScope(Specs2.combo ++ Scalaz.scalacheck)

      override def project = typicalProject
        .dependsOn(baseUtilSjs)
        .configure(dontInline) // crashes scalac 2.11.2
    }

    // ----------------------------------------------------
    object Db extends Module {
      val dir = "base-db"

      override def deps =
        postgresql ++ slick ++ bonecp ++ flyway ++ logback ++
        providedScope(jodaTime)

      override def project = typicalProject
        .dependsOn(baseUtil)
    }

    // ----------------------------------------------------
    object Test extends Module {
      val dir = "base-test"

      override def deps =
        providedScope(scalaTest ++ Specs2.combo)

      override def project = typicalProject
        .dependsOn(baseUtil)
        .dependsOn(baseDb % "provided")
        // TODO Delete after upgrade to 2.11 and switch from Manifest to TypeTag
        .settings(scalacOptions in Compile ~= removeValues("-deprecation"))
        .settings(scalacOptions in Compile += "-nowarn")
    }
  }

  // ===================================================================================================================
  object Taskman extends Module {
    val dir = "taskman"
    override def project = umbrellaOf(taskmanApi, taskmanServer)

    // API --------------------------------------------------
    object Api extends Module {
      val dir = "taskman-api"
      override def project = umbrellaOf(taskmanApiLogic, taskmanApiImpl)

      // API: Logic -----------------------------------------
      object Logic extends Module with ExportsTestLib {
        val dir = "taskman-api-logic"

        override def deps =
          Scalaz.core ++ Scalaz.effect ++
          depScope(TestLib)(scalaCheck ++ Scala.reflect) ++ testScope(Specs2.combo)

        override def project = typicalProject
          .dependsOn(baseUtil)
          .configure(testLibSettings)
      }

      // API: Impl ------------------------------------------
      object Impl extends Module {
        val dir = "taskman-api-impl"
        override def project = typicalProject
          .dependsOn(taskmanApiLogic % "compile->compile;test->test-lib")
          .dependsOn(taskmanServerSchema % "test")
          .dependsOn(baseTest % "test")
          .dependsOn(baseUtil, baseDb) // Stupid IDEA auto-import needs this

        override def deps =
          Json4s.jackson ++ testScope(Specs2.combo)
      }
    }

    // Server -----------------------------------------------
    object Server extends Module {
      val dir = "taskman-server"
      override def project = umbrellaOf(taskmanServerLogic, taskmanServerImpl, taskmanServerSchema)

      // Server: Logic --------------------------------------
      object Logic extends Module {
        val dir = "taskman-server-logic"
        override def project = typicalProject
          .dependsOn(taskmanApiLogic)
          .dependsOn(baseTest % "test")
          .dependsOn(baseUtil) // Stupid IDEA auto-import needs this
          .configure(dontInline) // crashes scalac 2.11.2

        override def deps =
          jodaTime ++ logback ++ testScope(Specs2.combo)
      }

      // Server: Schema -------------------------------------
      object Schema extends Module {
        val dir = "taskman-server-schema"
        override def project = typicalProject.dependsOn(baseDb)
      }

      // Server: Impl ---------------------------------------
      object Impl extends Module {
        val dir = "taskman-server-impl"

        override def deps =
          Akka.actor ++ javaMail ++ okHttp ++ httpCore ++
          testScope(Akka.testkit ++ Specs2.combo)

        def consoleCmds = """
          import org.json4s._
          import org.json4s.jackson.JsonMethods._
          import org.json4s.JsonDSL._
        """

        import sbtassembly.Plugin._
        import AssemblyKeys._

        override def project = typicalProject
          .dependsOn(taskmanServerLogic, taskmanServerSchema, taskmanApi)
          .dependsOn(baseTest % "test")
          .settings(assemblySettings: _*)
          .settings(
            initialCommands += consoleCmds,
            test in assembly := {}) // Disable tests during assembly
          .configure(dontInline) // because Akka docs + crashes scalac 2.11.2
      }
    }
  }

  // ===================================================================================================================
  object Webapp extends Module {
    val dir = "webapp"
    override def project = typicalProject
      .aggregate(webappClient, webappBase, webappBaseTest, webappServer) // not umbrella cos it shouldn't dependOn
      .configure(cmdAliases)

    lazy val cmdAliases = {
      def WB = "webapp-base"
      def WT = "webapp-base-test"
      def WC = "webapp-client"
      def WS = "webapp-server"
      addCommandAliases(
        "ctbc"-> ";clean ;clear ;tbc",                                       // Clean Test Base & Client
        "tbc" -> s";$WT/test:compile ;$WC/test:compile ;$WT/test ;$WC/test", // Test Base & Client
        "js"  -> s";$WC/${Client.jsCmd} ;$WS/linkClientJs",                  // compile JavaScript
        "up"  -> s";$WS/container:stop ;clear ;$WS/container:start",         // webapp: UP
        "d"   -> s"$WS/container:stop",                                      // webapp: Down
        "wd"  -> ";up ;~js")                                                 // WebDev
    }

    // ----------------------------------------------------
    object Base extends Module {
      val dir = "webapp-base"

      override def deps =
        μPickle.jvm ++ Monocle.macros ++ shapeless.jvm ++ Nyaya.jvm.core ++ parboiled.jvm
        testScope(μTest.jvm)

      override def project = typicalProject
        .configure(
          useMacroParadise,
          Common.utestOnJvm,
          dontInline, // crashes scalac 2.11.5
          cmdAliases)
        .dependsOn(baseUtilSjs)
    }

    // ----------------------------------------------------
    object BaseTest extends Module {
      val dir = "webapp-base-test"

      override def deps =
        μTest.jvm ++ Nyaya.jvm.test

      override def project = typicalProject
        .configure(Common.utestOnJvm, cmdAliases)
        .dependsOn(webappBase)
    }

    // ----------------------------------------------------
    object Client extends Module {
      import ScalaJSPlugin._
      import ScalaJSPlugin.autoImport._
      import org.scalajs.core.tools.sem._

      def stage  = if (releaseMode) FullOptStage else FastOptStage
      def jsTask = if (releaseMode) fullOptJS    else fastOptJS
      def jsCmd  = if (releaseMode) "fullOptJS"  else "fastOptJS"

      val dir = "webapp-client"

      override def deps =
        ScalaJS.Scalaz.effect ++ ScalaJS.React.most ++ ScalaJS.Monocle.macros ++ ScalaJS.ScalaCSS.react ++
        μPickle.js ++ shapeless.js ++ Nyaya.js.core ++ parboiled.js ++
        testScope(ScalaJS.React.test ++ μTest.js ++ Nyaya.js.test)

      def testjs(path: String) = ProvidedJS / s"testjs/$path"

      def testSettings = (_: Project)
        .configure(Common.utestOnJs)
        .settings(
          scalaJSStage in Global := stage,
          jsDependencies in Test ++= Seq(
            testjs("react-with-addons.js"),
            testjs("jquery.min.js"),
            testjs("jquery.textcomplete.js") dependsOn "testjs/jquery.min.js",
            testjs("sizzle.min.js")),
          emitSourceMaps in Test := false, // PhantomJS doesn't use
          requiresDOM := true,
          jsEnv in Test := new PhantomJS2Env(scalaJSPhantomJSClassLoader.value))

      def prodJsSettings = (_: Project).settings(
        emitSourceMaps := false,
        scalaJSOptimizerOptions ~= (_
          //.withPrettyPrintFullOptJS(true)
          .withBatchMode(true)
          .withCheckScalaJSIR(true)
        ),
        scalaJSSemantics ~= (_
          .withRuntimeClassName(_ => "")
          .withAsInstanceOfs(CheckedBehavior.Unchecked)
        ))

      override def project = typicalProject
        .enablePlugins(ScalaJSPlugin)
        .configure(
          useMacroParadise,
          jsStyleDependsOn(baseUtilSjs, webappBase),
          jsStyleDependsOnS(webappBaseTest)(Compile -> Test),
          Common.addSourceDialectJsFrom(baseUtilSjs),
          testSettings,
          dontInline, // ScalaJS inlines
          cmdAliases,
          debugOrRelease(identity, prodJsSettings))
    }

    // ----------------------------------------------------
    object Server extends Module {
      import com.earldouglas.xsbtwebplugin.PluginKeys.{packageWar, start}
      import com.earldouglas.xsbtwebplugin.WebPlugin.{container, webSettings}

      val dir = "webapp-server"

      val linkClientJs = taskKey[Unit]("Creates symlinks to webapp client resources.")
      val clientJsLinks = settingKey[ClientJsLinks]("Map of symlinks between client and server.")

      class ClientJsLinks(sRoot: File, tRoot: File) {
        private val s = sRoot / "scala-2.11"
        private val t = tRoot / "src/main/webapp/assets"
        private def sPrefix = Webapp.Client.dir + "-"
        private val devMap = {
          val o = t/"dev"
          val js = s"${sPrefix}fastopt.js"
          val map = js + ".map"
          Map(s/js -> o/js, s/map -> o/map)
        }
        private val releaseMap =
          Map(s/s"${sPrefix}opt.js" -> t/"C.js")
        def links =
          if (releaseMode) releaseMap else devMap
        def cleanable =
          (devMap.values ++ releaseMap.values).map(_.asFile).toSet[File]
      }

      lazy val jsBuildTask =
        Client.jsTask in Compile in webappClient

      def clientJsSettings = (_: Project).settings(
        clientJsLinks := new ClientJsLinks((target in webappClient).value, baseDirectory.value),
        cleanFiles ++= clientJsLinks.value.cleanable.toSeq,
        { val k = Keys.`package` in Compile;        k <<= k.dependsOn(linkClientJs) },
        { val k = start in container.Configuration; k <<= k.dependsOn(linkClientJs) },
        { val k = test in Test;                     k <<= k.dependsOn(linkClientJs) },
        linkClientJs := {
          jsBuildTask.value // Ensure client JS is built
          val log = streams.value.log
          for ((s, t) <- clientJsLinks.value.links)
            ln(s, t, log)
        })

      def warSettings = (_: Project).settings(
        // Remove certain files from the WAR
        excludeFilter in packageWar ~= { (a: FileFilter) =>
          var b = a || new FileFilter { def accept(f: File) = f.getPath.containsSlice("/_scalate/") }
          if (releaseMode)
            b = b || new FileFilter { def accept(f: File) = f.getPath.containsSlice("/dev/") }
          b
        })

      def testSettings = (_: Project).settings(
        // Put webapp on test classpath so templates load
        unmanagedResourceDirectories in Test += baseDirectory.value / "src/main/webapp",
        parallelExecution in Test := false)

      lazy val IntegrationTest = config("it") extend Test
      def integrationTestSettings = (_: Project)
        .configs(IntegrationTest)
        .settings(inConfig(IntegrationTest)(Defaults.testSettings): _*)
        .settings(parallelExecution in IntegrationTest := false)

      override def deps =
        Scalaz.core ++ Lift.webkit ++ Shiro.all ++ scalate ++ commonsLang ++
        testScope(scalaTest ++ scalaCheck ++ mockito ++ Lift.testkit ++ commonsIo ++ twitterEval) ++
        depScope("it")(selenium) ++
        (jetty % "container,test") ++ (servlet % "container,test,provided")

      def consoleCmds = """
        import scalaz._, shipreq.base.util._, shipreq.webapp._, db._, lib.Types._, feature.uc, uc._, uc.field._, uc.step._, uc.text._, FreeTextTerms._, util._
        def initlift() = {val b = new bootstrap.liftweb.Boot; b.configureLift; b}
      """

      override def project = typicalProject
        .settings(webSettings: _*)
        .configure(
          Common.generateBuildPropFile(),
          clientJsSettings,
          warSettings,
          testSettings,
          integrationTestSettings,
          dontInline, // crashes scalac 2.11.5
          cmdAliases)
        .settings(
          initialCommands += consoleCmds,
          // Ensure templates can be loaded from the console
          fullClasspath in console in Compile += file("src/main/webapp"))
        .dependsOn(baseDb, taskmanApi, webappBase)
        .dependsOn(baseUtil, taskmanApiLogic, taskmanApiImpl) // Stupid IDEA auto-import needs this
      }
  }

  // ===================================================================================================================
  object Utils extends Module {
    val dir = "utils"

    override def deps =
      commonsLang ++ Nyaya.jvm.test ++
      testScope(twitterEval)

    override def project = typicalProject
      .configure(Common.utestOnJvm)
      .dependsOn(webappBaseTest)
      .settings(
        connectInput in run  := true,
        fork         in run  := true,
        javaOptions  in run ++= Seq("-Xmx4g", "-Xss4m"))
  }

  // ===================================================================================================================
  trait BenchmarkModule extends Module {
    override def commonSettings: Project => Project =
      _.configure(Common.settingsMin, ideSettings)
        .settings(scalacOptions ++= scalacFlags)

    def scalacFlags = Seq(
      "-unchecked",
      "-deprecation",
      "-YclasspathImpl:flat", // https://github.com/scala/scala/pull/4176
      "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")
  }

  object Benchmark extends BenchmarkModule {
    val dir = "benchmark"

    override def project = typicalProject
      .aggregate(benchmarkBase, benchmarkJvm, benchmarkJs) // not umbrella cos it shouldn't dependOn

    // ----------------------------------------------------
    object Base extends BenchmarkModule {
      val dir = "benchmark-base"

      override def project = typicalProject
        .dependsOn(webappBase)
    }

    // ----------------------------------------------------
    object Jvm extends BenchmarkModule {
      import pl.project13.scala.sbt.SbtJmh

      val dir = "benchmark-jvm"

      override def project = typicalProject
        .enablePlugins(SbtJmh)
        .dependsOn(benchmarkBase, webappServer)
    }

    // ----------------------------------------------------
    object Js extends BenchmarkModule {
      import org.scalajs.sbtplugin._
      import ScalaJSPlugin.autoImport._

      val dir = "benchmark-js"

      val outputJs = "shipreq-benchmark.js"

      override def project = typicalProject
        .enablePlugins(ScalaJSPlugin)
        .dependsOn(webappClient)
        .configure(
          jsStyleDependsOn(benchmarkBase),
          useMacroParadise,
          Webapp.Client.prodJsSettings)
        .settings(
          scalaJSStage in Global := FullOptStage,
          artifactPath in (Compile, fastOptJS) := ((target in Compile).value / outputJs),
          artifactPath in (Compile, fullOptJS) := ((target in Compile).value / outputJs),
          scalaJSStage in Test := Stage.PreLink,
          test in Test := ())
    }
  }
}
