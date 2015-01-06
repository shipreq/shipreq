import sbt._
import Keys._
import Common.ExportsTestLib
import Common.Functions._
import Common.Values.releaseMode
import Deps._

object ShipReq extends Build {

  // Declare modules
  lazy val root = Root.project

  lazy val prop     = Prop.project
  lazy val propCore = Prop.PropCore.project
  lazy val propTest = Prop.PropTest.project

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
        .settings(libraryDependencies ++= deps.ms)

    protected def typicalProject: Project =
      Project(dir, file(dir)).configure(commonSettings).settings(name := dir)

    protected def umbrellaOf(ps: ProjectReference*): Project =
      typicalProject.aggregate(ps: _*).dependsOn(ps.map{p => p: ClasspathDep[ProjectReference]}: _*)
  }

  // ===================================================================================================================
  object Root extends Module {
    def dir = "."
    override def project = Project("root", file(dir))
      .configure(commonSettings, Common.useHiddenTargetDir)
      .aggregate(base, webapp, taskman, prop)
  }

  // ===================================================================================================================
  object Prop extends Module {
    val dir = "prop"

    override def project = typicalProject
      .aggregate(propCore, propTest) // not umbrella cos it shouldn't dependOn

    // ----------------------------------------------------
    object PropCore extends Module {
      val dir = "prop-core"

      override def deps =
        Scalaz.core ++ testScope(μTest.jvm)

      override def project = typicalProject
        .configure(Common.scalaAndScalaJsShared)
    }

    // ----------------------------------------------------
    object PropTest extends Module {
      val dir = "prop-test"

      override def deps =
        Scalaz.core ++ Monocle.macros ++ RNG.jvm ++ μTest.jvm

      override def project = typicalProject
        .configure(Common.scalaAndScalaJsShared)
        .dependsOn(propCore)
        .settings(unmanagedSourceDirectories in Compile += baseDirectory.value / "src/main/scala-jvm")
    }
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
        Scalaz.core ++ testScope(μTest.jvm)

      override def project = typicalProject
        .configure(Common.scalaAndScalaJsShared)
    }

    // ----------------------------------------------------
    object Util extends Module {
      val dir = "base-util"

      override def deps =
        SLF4J.api ++ Scalaz.core ++
        providedScope(Scalaz.effect ++ logback ++ jodaTime) ++
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

    // ----------------------------------------------------
    object Base extends Module {
      val dir = "webapp-base"

      override def deps =
        μPickle.jvm ++ Monocle.macros ++ shapeless.jvm
        testScope(μTest.jvm)

      override def project = typicalProject
        .configure(
          Common.scalaAndScalaJsShared,
          addCommandAliases(
            "tbc" -> ";webapp-base-test/test; webapp-client/test",
            "js"  -> Client.jsCmd,
            "wd"  -> ";up;~js"))
        .dependsOn(propCore, baseUtilSjs)
    }

    // ----------------------------------------------------
    object BaseTest extends Module {
      val dir = "webapp-base-test"

      override def deps = μTest.jvm

      override def project = typicalProject
        .configure(Common.scalaAndScalaJsShared)
        .dependsOn(webappBase, propTest % "test")
    }

    // ----------------------------------------------------
    object Client extends Module {
      import scala.scalajs.sbtplugin.ScalaJSPlugin._
      import scala.scalajs.sbtplugin.InliningMode
      import scala.scalajs.sbtplugin.env.phantomjs.PhantomJSEnv
      import utest.jsrunner.Plugin.utestJsSettings
      import ScalaJSKeys._

      def stage  = if (releaseMode) fullOptStage else fastOptStage
      def jsTask = if (releaseMode) fullOptJS    else fastOptJS
      def jsCmd  = if (releaseMode) "fullOptJS"  else "fastOptJS"

      val dir = "webapp-client"

      override def deps =
        ScalaJS.Scalaz.effect ++ ScalaJS.React.most ++ ScalaJS.Monocle.macros ++ μPickle.js ++ shapeless.js ++
        testScope(ScalaJS.React.test ++ μTest.js ++ RNG.js)

      def testSettings = (_: Project)
        .settings(utestJsSettings: _*)
        .settings(
          test      in Test := (test      in(Test, stage)).value,
          testOnly  in Test := (testOnly  in(Test, stage)).evaluated,
          testQuick in Test := (testQuick in(Test, stage)).evaluated,
          jsDependencies += ScalaJS.reactJs % "test" / "react-with-addons.js" commonJSName "React",
          jsDependencies += ScalaJS.sizzleJs % "test" / "sizzle.min.js" commonJSName "Sizzle",
          requiresDOM := true,
          postLinkJSEnv in Test := new PhantomJSEnv)

      def prodJsSettings = (_: Project).settings(
        emitSourceMaps in fullOptJS := false,
        // checkScalaJSIR in fullOptJS := true, https://github.com/lihaoyi/upickle/issues/27
        inliningMode in fullOptJS := InliningMode.Batch)

      // Recompile shared source rather than depending directly
      // https://github.com/scala-js/scala-js/issues/1067
      def jsStyleDependsOn(deps: Project*) =
        deps.foldLeft(identity[Project]_)(_ compose jsStyleDependsOnS(_)(Compile -> Compile, Test -> Test))

      def jsStyleDependsOnS(deps: Project*)(scopes: (Configuration, Configuration)*) = (_: Project)
        .settings((
          for {
            dep    <- deps
            (a, b) <- scopes
          } yield
            unmanagedSourceDirectories in b += (scalaSource in a in dep).value
          ): _*)

      override def project = typicalProject
        .settings(scalaJSSettings: _*)
        .configure(
          jsStyleDependsOn(propCore, baseUtilSjs, webappBase),
          jsStyleDependsOnS(propTest, webappBaseTest)(Compile -> Test, Test -> Test),
          testSettings,
          dontInline, // crashes scalac 2.11.2
          prodJsSettings)
        .settings(unmanagedSourceDirectories in Test += (baseDirectory in Compile in propTest).value / "src/main/scala-js")
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

      lazy val jsBuildTask = {
        import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._
        Client.jsTask in Compile in webappClient
      }

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
        testScope(scalaTest ++ scalaCheck ++ mockito ++ Lift.testkit ++ commonsIo /*++ twitterEval*/) ++
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
          addCommandAliases(
            "up" -> ";container:stop ;clear ;container:start",
            "d" -> "container:stop"))
        .settings(
          initialCommands += consoleCmds,
          // Ensure templates can be loaded from the console
          fullClasspath in console in Compile += file("src/main/webapp"))
        .dependsOn(baseDb, taskmanApi, webappBase)
        .dependsOn(propTest % "test") //, webappBaseTest % "test") μTest??
        .dependsOn(baseUtil, taskmanApiLogic, taskmanApiImpl) // Stupid IDEA auto-import needs this
      }
  }
}
