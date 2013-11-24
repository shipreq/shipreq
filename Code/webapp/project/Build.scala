import sbt._
import Keys._
import org.sbtidea.SbtIdeaPlugin._
import com.earldouglas.xsbtwebplugin.PluginKeys.webappResources
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
      ".idea", ".idea_modules", ".sass-cache", ".settings", "WEB-INF", "_scalate", "vendor",
      "src/main/webapp/WEB-INF/_scalate", "src/main/javascript/vendor",
      "src/main/webapp/css/vendor", "src/main/webapp/js/vendor")
  )

  def javascriptSettings = (p: Project) => {
    import com.untyped.sbtjs.Plugin._
    p.settings(jsSettings: _*)
    .settings(
      // Minify JS as part of compile task
      (compile in Compile) <<= compile in Compile dependsOn (JsKeys.js in Compile),
      // Minify JS in src/main/javascript
      (sourceDirectory in (Compile, JsKeys.js)) <<= (sourceDirectory in Compile)(_ / "javascript"),
      // Put minified JS in js/
      (resourceManaged in (Compile,JsKeys.js)) <<= (resourceManaged in Compile)( _ / "js"),
      // Put Javascript in WAR root
      (webappResources in Compile) <+= (resourceManaged in Compile)
      // Puts Javascript in WEB-INF/classes
      // (resourceGenerators in Compile) <+= (JsKeys.js in Compile)
    )
  }

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
      javascriptSettings,
      testSettings,
      integrationTestSettings
    )
    .settings(
      startYear := Some(2013),
      clear := { println("\033[2J\033[;H") },
      // Prevent src/main/java appearing in .classpath
      unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_))
    )
}
