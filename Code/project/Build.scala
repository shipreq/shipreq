import sbt._
import Keys._
import Common.Functions._
import Common.Values._

object ShipReq extends Build {
  sealed trait Module

  // Declare modules
  lazy val root = Root.project
  lazy val webapp = Webapp.project

  // ===================================================================================================================
  object Root extends Module {

    def project = Project("root", file("."))
      .configure(Common.settings, IdeSettings(Root))
      .aggregate(webapp)
  }

  // ===================================================================================================================
  object Webapp extends Module {
    import com.earldouglas.xsbtwebplugin.PluginKeys.packageWar
    import com.earldouglas.xsbtwebplugin.WebPlugin.webSettings

    val dir = "webapp"

    def compilerFlags = debugOrRelease(
      _.settings(scalacOptions ++= Seq("-Xcheckinit")),
      nonTestCompilerFlags("-optimise", /*"-Yinline-warnings",*/ "-Xelide-below", "OFF"))

    def warSettings = (p: Project) => p.settings(
      // Don't allow WEB-INF/_scalate into the WAR
      excludeFilter in packageWar ~= { _ ||
        new FileFilter { def accept(f: File) = f.getPath.containsSlice("/_scalate/") }
      }
    )

    def testSettings = (p: Project) => p.settings(
      // Put webapp on test classpath so templates load
      unmanagedResourceDirectories in Test <+= baseDirectory { _ / "src/main/webapp" },
      // Prevent src/test/java appearing in .classpath
      unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_)),
      scalacOptions in Test ++= Seq("-language:reflectiveCalls"),
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
        IdeSettings(Webapp),
        Common.generateBuildPropFile(),
        compilerFlags,
        warSettings,
        testSettings,
        integrationTestSettings
      )
      .settings(webSettings: _*)
      .settings(
        version := s"${fmtTimeNow("yyyyMMdd")}-${gitRevisionShort}${snapshotSuffix}",
        isSnapshot := snapshotSuffix.nonEmpty,
        // Ensure templates can be loaded from the console
        fullClasspath in console in Compile += file("src/main/webapp"),
        // Prevent src/main/java appearing in .classpath
        unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_))
      )
    }
}
