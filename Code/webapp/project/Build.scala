import sbt._
import Keys._
import org.sbtidea.SbtIdeaPlugin._
import com.earldouglas.xsbtwebplugin.PluginKeys.{packageWar, webappResources}
import com.earldouglas.xsbtwebplugin.WebPlugin.webSettings

object B extends Build {

  val baseVersion = SettingKey[String]("base-version", "The version.")
  val buildRev    = SettingKey[String]("build-rev",    "The source revision according to version control.")

  val clear = TaskKey[Unit]("clear", "Clears the screen.")

  def buildPropertyFileGeneration = (p: Project) => {
    def createBuildProps(outDir: File, verBase: String, verFull: String, rev: String) = {
      val outFile = outDir / "build.properties"
      val props = Map[String, String](
        "version.base" -> verBase,
        "version.full" -> verFull,
        "revision" -> rev,
        "time" -> new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date)
      )
      val contents = props.toList.map {case (k, v) => "build." + k + "=" + v}.mkString("\n")
      IO.write(outFile, contents)
      Seq(outFile)
    }
    p.settings(
      version <<= (baseVersion, buildRev) {(ver,rev) => s"$ver-SNAPSHOT-${rev.substring(0, 8)}"},
      buildRev := Process("git rev-parse HEAD").lines.head.trim,
      resourceGenerators in Compile <+= (resourceManaged in Compile, baseVersion, version, buildRev) map createBuildProps
    )
  }

  def eclipseSettings = (p: Project) => {
    import com.typesafe.sbteclipse.core.EclipsePlugin._
    p.settings(
      EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE17),
      EclipseKeys.withSource := true,
      EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource
    )
  }

  def intellijSettings = (p: Project) => p.settings(
    ideaExcludeFolders := List(
      ".idea", ".idea_modules", ".settings", "WEB-INF", "_scalate", "vendor",
      "node_modules", ".bower",
      "src/main/webapp/WEB-INF/_scalate",
      "src/main/webapp/css/vendor", "src/main/webapp/js/vendor")
  )

  lazy val releaseMode: Boolean = {
    val mode = System.getProperty("MODE", "").trim
    val r = mode.compareToIgnoreCase("release") == 0
    if (r) println("Release Mode.")
    r
  }

  def debugSettings = (p: Project) => p.settings(
    scalacOptions ++= Seq("-Xcheckinit")
  )

  def releaseSettings = {
    val compilerSettings = Seq("-optimise", /*"-Yinline-warnings",*/ "-Xelide-below", "OFF")
    (p: Project) => p.settings(
      scalacOptions in Compile ++= compilerSettings,
      scalacOptions in Test ~= (_ filterNot (compilerSettings contains _))
    )
  }

  def warSettings = (p: Project) => p.settings(
    // Don't allow WEB-INF/_scalate into the WAR
    excludeFilter in packageWar ~= { _ ||
        new FileFilter { def accept(f: File) = f.getPath.containsSlice("/_scalate/") }
    }
  )

  def testSettings = (p: Project) => p.settings(
    // Put webapp on test classpath so templates load
    unmanagedResourceDirectories in Test <+= (baseDirectory) { _ / "src/main/webapp" },
    // Prevent src/test/java appearing in .classpath
    unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_)),
    scalacOptions in Test ++= Seq("-language:reflectiveCalls"),
    parallelExecution in Test := false
  )

  lazy val IntegrationTest = config("it") extend(Test)
  def integrationTestSettings = (p: Project) =>
    p.configs(IntegrationTest)
    .settings(inConfig(IntegrationTest)(Defaults.testSettings): _*)
    .settings(
      parallelExecution in IntegrationTest := false
    )

  lazy val root =
    Project("root", file("."))
    .settings(webSettings: _*)
    .settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*) // Dependency graph
    .configure(
      buildPropertyFileGeneration,
      eclipseSettings,
      intellijSettings,
      if (releaseMode) releaseSettings else debugSettings,
      warSettings,
      testSettings,
      integrationTestSettings
    )
    .settings(
      startYear := Some(2013),
      clear := { println("\033[2J\033[;H") },
      // Ensure templates can be loaded from the console
      fullClasspath in console in Compile += file("src/main/webapp"),
      // Prevent src/main/java appearing in .classpath
      unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_))
    )
}
