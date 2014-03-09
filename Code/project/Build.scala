import sbt._
import Keys._
import Common.Functions._

object ShipReq extends Build {

  // Declare modules
  lazy val root = Root.project
  lazy val common = CommonModule.project
  lazy val webapp = Webapp.project
  lazy val taskman = Taskman.project

  sealed trait Module {
    def ideSettings = IdeSettings(this)
  }

  // ===================================================================================================================
  object Root extends Module {

    def project = Project("root", file("."))
      .configure(Common.settings, ideSettings)
      .aggregate(common, webapp, taskman)
  }

  // ===================================================================================================================
  object CommonModule extends Module {

    val dir = "common"

    def project = Project(dir, file(dir))
      .configure(Common.settings, ideSettings)
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

    def project = Project("webapp", file(dir))
      .configure(
        Common.settings,
        ideSettings,
        Common.generateBuildPropFile(),
        warSettings,
        testSettings,
        integrationTestSettings
      )
      .settings(webSettings: _*)
      .settings(
        // Ensure templates can be loaded from the console
        fullClasspath in console in Compile += file("src/main/webapp")
      )
      .dependsOn(common)
    }

  // ===================================================================================================================
  object Taskman extends Module {

    val dir = "taskman"

    def project = Project(dir, file(dir))
      .configure(Common.settings, ideSettings)
      .dependsOn(common)
      .settings(
        scalacOptions in Compile ~= removeValues("-optimise") // see Akka docs
      )
  }
}
