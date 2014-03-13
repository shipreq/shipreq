import sbt._
import Keys._
import Common.Functions._
import Deps._

object ShipReq extends Build {

  // Declare modules
  lazy val root            = Root.project
  lazy val base            = Base.project
  lazy val baseDb          = Base.Db.project
  lazy val webapp          = Webapp.project
  lazy val taskmanApiLogic = TaskmanApi.Logic.project
  lazy val taskmanApi      = TaskmanApi.project
  lazy val taskmanLogic    = Taskman.Logic.project
  lazy val taskman         = Taskman.project

  sealed trait Module {
    def project: Project
    def dir: String

    def deps: MS = MS.empty
    protected def depScope(s: String)(ms: MS): MS = ms % s
    protected def testScope = depScope("test") _

    def ideSettings = IdeSettings(this)

    def commonSettings: Project => Project =
      _.configure(Common.settings, ideSettings)
        .settings(libraryDependencies ++= deps.ms)

    protected def typicalProject: Project =
      Project(dir, file(dir)).configure(commonSettings).settings(name := dir)
  }

  // ===================================================================================================================
  object Root extends Module {
    def dir = "."
    override def project = Project("root", file(dir))
      .configure(commonSettings, Common.useHiddenTargetDir)
      .aggregate(base, baseDb, webapp, taskman)
  }

  // ===================================================================================================================
  object Base extends Module {
    val dir = "base"

    override def deps =
      scalaTest % "test"

    override def project = typicalProject

    // ----------------------------------------------------
    object Db extends Module {
      val dir = "base-db"

      override def deps =
        postgresql ++ slick ++ bonecp ++ flyway ++ logback ++ testScope(scalaTest)

      override def project = typicalProject
        .dependsOn(base)
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
    }

  // ===================================================================================================================
  object TaskmanApi extends Module {
    val dir = "taskman-api"
    override def project = typicalProject
      .aggregate(taskmanApiLogic)
      .dependsOn(taskmanApiLogic)

//    override def deps =
//      Json4s.jackson ++ testScope(specs2)

    // ----------------------------------------------------
    object Logic extends Module {
      val dir = "taskman-api-logic"

      override def deps =
        Scalaz.core ++ Scalaz.effect ++ testScope(specs2 ++ scalaCheck)

      override def project = typicalProject
        .dependsOn(base)
    }
  }

  // ===================================================================================================================
  object Taskman extends Module {
    val dir = "taskman"

    override def deps =
      Akka.actor ++ testScope(Akka.testkit)

    override def project = typicalProject
      .aggregate(taskmanLogic, taskmanApi)
      .dependsOn(taskmanLogic, taskmanApi, baseDb)
      .settings(
        scalacOptions in Compile ~= removeValues("-optimise") // see Akka docs
      )

    // ----------------------------------------------------
    object Logic extends Module {
      val dir = "taskman-logic"
      override def project = typicalProject
        .dependsOn(taskmanApiLogic)
    }
  }
}
