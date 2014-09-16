import sbt._
import Keys._
import Common.ExportsTestLib
import Common.Functions._
import Common.Values.releaseMode
import Deps._

object ShipReq extends Build {

  // Declare modules
  lazy val root = Root.project

  lazy val base        = Base.project
  lazy val baseDb      = Base.Db.project
  lazy val baseTest    = Base.Test.project
  lazy val baseUtil    = Base.Util.project
  lazy val baseUtilSjs = Base.UtilSjs.project

  lazy val webapp       = Webapp.project
  lazy val webappClient = Webapp.Client.project
  lazy val webappShared = Webapp.Shared.project
  lazy val webappServer = Webapp.Server.project

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
      .aggregate(base, webapp, taskman)
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
        providedScope(ScalaJS.Scalaz.effect)

      override def project = typicalProject
        .configure(Common.scalaAndScalaJsShared)
    }

    // ----------------------------------------------------
    object Util extends Module {
      val dir = "base-util"

      override def deps =
        SLF4J.api ++ Scalaz.core ++
        providedScope(Scalaz.effect ++ logback ++ jodaTime) ++
        testScope(specs2 ++ Scalaz.scalacheck)

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
        providedScope(scalaTest ++ specs2)

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
          depScope(TestLib)(scalaCheck ++ Scala.reflect) ++ testScope(specs2)

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
          Json4s.jackson ++ testScope(specs2)
      }
    }

    // Server -----------------------------------------------
    object Server extends Module {
      val dir = "taskman-server"
      override def project = umbrellaOf(taskmanServerLogic, taskmanServerImpl, taskmanServerSchema)

      // Server: Logic --------------------------------------
      object Logic extends Module {
        val dir = "taskman-server-logic"
        override def project = typicalProject.dependsOn(taskmanApiLogic)
          .dependsOn(baseTest % "test")
          .dependsOn(baseUtil) // Stupid IDEA auto-import needs this
          .configure(dontInline) // crashes scalac 2.11.2

        override def deps =
          jodaTime ++ logback ++ testScope(specs2)
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
          testScope(Akka.testkit ++ specs2)

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
      .aggregate(webappClient, webappShared, webappServer) // not umbrella cos it shouldn't dependOn

    // ----------------------------------------------------
    object Shared extends Module {
      val dir = "webapp-shared"

//      override def deps =
//        providedScope(ScalaJS.Scalaz.effect)

      override def project = typicalProject
        .configure(Common.scalaAndScalaJsShared)
        .dependsOn(baseUtilSjs)
    }

    // ----------------------------------------------------
    object Client extends Module {
      import scala.scalajs.sbtplugin.ScalaJSPlugin._

      val dir = "webapp-client"

      override def deps =
        ScalaJS.Scalaz.effect ++ ScalaJS.React.core ++ ScalaJS.monocle ++
        testScope(ScalaJS.React.test)

      override def project = typicalProject
        .settings(scalaJSSettings: _*)
        .dependsOn(webappShared)
    }

    // ----------------------------------------------------
    object Server extends Module {
      import com.earldouglas.xsbtwebplugin.PluginKeys.packageWar
      import com.earldouglas.xsbtwebplugin.WebPlugin.webSettings

      val dir = "webapp-server"

      val linkClientJs = taskKey[Unit]("Creates symlinks to webapp client resources.")
      val clientJsLinks = settingKey[ClientJsLinks]("Map of symlinks between client and server.")

      class ClientJsLinks(sRoot: File, tRoot: File) {
        private val s = sRoot / "scala-2.11"
        private val t = tRoot / "src/main/webapp/assets"
        private def sPrefix = Webapp.Client.dir + "-"
        private def tName = "blah.js"
        private val devMap = Map(
          s / s"${sPrefix}fastopt.js"     -> t / tName,
          s / s"${sPrefix}fastopt.js.map" -> t / s"$tName.map")
        private val releaseMap =
          Map(s / s"${sPrefix}opt.js" -> t / tName)
        def links =
          if (releaseMode) releaseMap else devMap
        def cleanable =
          (devMap.values ++ releaseMap.values).map(_.asFile).toSet[File]
      }

      def clientJsSettings = (_: Project).settings(
        clientJsLinks := new ClientJsLinks((target in webappClient).value, baseDirectory.value),
        cleanFiles ++= clientJsLinks.value.cleanable.toSeq,
        linkClientJs := {
          val log = streams.value.log
          for ((s, t) <- clientJsLinks.value.links)
            ln(s, t, log)
        })

      def warSettings = (_: Project).settings(
        // Don't allow WEB-INF/_scalate into the WAR
        excludeFilter in packageWar ~= { _ ||
          new FileFilter { def accept(f: File) = f.getPath.containsSlice("/_scalate/") }
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
        .configure(
          Common.generateBuildPropFile(),
          clientJsSettings,
          warSettings,
          testSettings,
          integrationTestSettings,
          addCommandAliases(
            "up" -> ";container:stop ;clear ;container:start",
            "d" -> "container:stop"))
        .settings(webSettings: _*)
        .settings(
          initialCommands += consoleCmds,
          // Ensure templates can be loaded from the console
          fullClasspath in console in Compile += file("src/main/webapp"))
        .dependsOn(baseDb, taskmanApi, webappShared)
        .dependsOn(baseUtil, taskmanApiLogic, taskmanApiImpl) // Stupid IDEA auto-import needs this
      }
  }
}
