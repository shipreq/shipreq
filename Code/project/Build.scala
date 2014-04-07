import sbt._
import Keys._
import Common.ExportsTestLib
import Common.Functions._
import Deps._

object ShipReq extends Build {

  // Declare modules
  lazy val root = Root.project

  lazy val base     = Base.project
  lazy val baseDb   = Base.Db.project
  lazy val baseTest = Base.Test.project
  lazy val baseUtil = Base.Util.project

  lazy val webapp = Webapp.project

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
      .aggregate(baseUtil, baseDb, baseTest) // not umbrella cos it shouldn't dependOn

    // ----------------------------------------------------
    object Util extends Module {
      val dir = "base-util"

      override def deps =
        SLF4J.api ++ Scalaz.core ++ jodaTime % "provided" ++ specs2 % "test"

      override def project = typicalProject
    }

    // ----------------------------------------------------
    object Db extends Module {
      val dir = "base-db"

      override def deps =
        postgresql ++ slick ++ bonecp ++ flyway ++ logback ++ testScope(scalaTest)

      override def project = typicalProject
        .dependsOn(baseUtil)
    }

    // ----------------------------------------------------
    object Test extends Module {
      val dir = "base-test"

      override def deps =
        depScope("provided")(scalaTest ++ specs2)

      override def project = typicalProject
        .dependsOn(baseUtil)
        .dependsOn(baseDb % "provided")
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
          Akka.actor ++ javaMail ++
          testScope(Akka.testkit ++ specs2)

        import com.typesafe.sbt.{SbtNativePackager => N}
        //import N.NativePackagerKeys._

        override def project = typicalProject
          .dependsOn(taskmanServerLogic, taskmanServerSchema, taskmanApi)
          .dependsOn(baseTest % "test")
          .settings(N.packageArchetype.java_application: _*)
          .settings(
            scalacOptions in Compile ~= removeValues("-optimise") // because Akka docs
          )
      }
    }
  }

  // ===================================================================================================================
  object Webapp extends Module {
    import com.earldouglas.xsbtwebplugin.PluginKeys.packageWar
    import com.earldouglas.xsbtwebplugin.WebPlugin.webSettings

    val dir = "webapp"

    def warSettings = (p: Project) => p.settings(
      // Don't allow WEB-INF/_scalate into the WAR
      excludeFilter in packageWar ~= { _ ||
        new FileFilter { def accept(f: File) = f.getPath.containsSlice("/_scalate/") }
      }
    )

    def testSettings = (p: Project) => p.settings(
      // Put webapp on test classpath so templates load
      unmanagedResourceDirectories in Test <+= baseDirectory { _ / "src/main/webapp" },
      parallelExecution in Test := false
    )

    lazy val IntegrationTest = config("it") extend Test
    def integrationTestSettings = (p: Project) =>
      p.configs(IntegrationTest)
        .settings(inConfig(IntegrationTest)(Defaults.testSettings): _*)
        .settings(
        parallelExecution in IntegrationTest := false
      )

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
      .configure(
        Common.generateBuildPropFile(),
        warSettings,
        testSettings,
        integrationTestSettings
      )
      .settings(webSettings: _*)
      .settings(
        initialCommands += consoleCmds,
        // Ensure templates can be loaded from the console
        fullClasspath in console in Compile += file("src/main/webapp")
      )
      .dependsOn(baseDb, taskmanApi)
      .dependsOn(baseUtil, taskmanApiLogic, taskmanApiImpl) // Stupid IDEA auto-import needs this
    }
}
