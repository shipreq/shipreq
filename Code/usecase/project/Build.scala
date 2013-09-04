import sbt._
import Keys._
import org.sbtidea.SbtIdeaPlugin._

object B extends Build {

  val baseVersion = SettingKey[String]("base-version", "The version.")

  val buildRev = SettingKey[String]("build-rev", "The source revision according to version control.")

  val BuildPropsFilename = "build.properties"

  val clear = TaskKey[Unit]("clear", "Clears the screen.")

  lazy val root =
    Project("root", file("."))
    .configs(SeleniumTest)
    .settings(com.earldouglas.xsbtwebplugin.WebPlugin.webSettings: _*)
    .settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*) // Dependency graph
    .settings(inConfig(SeleniumTest)(Defaults.testSettings): _*)
    .settings(
      clear := { println("\033[2J\033[;H") },

      ideaExcludeFolders := List(".idea", ".idea_modules", ".sass-cache", ".settings", "WEB-INF", "src/main/webapp/WEB-INF/_scalate", "src/main/webapp/css"),

      version <<= (baseVersion, buildRev) {(ver,rev) => ver + "-SNAPSHOT-" + rev.substring(0, 8)},

      buildRev := Process("git rev-parse HEAD").lines.head.trim,
      resourceGenerators in Compile <+= (resourceManaged in Compile, baseVersion, version, buildRev) map (createBuildProps),

      testOptions in Test := Seq(Tests.Filter(normalTestFilter)),
      testOptions in SeleniumTest := Seq(Tests.Filter(seleniumTestFilter)),
      parallelExecution in Test := false,
      parallelExecution in SeleniumTest := false
    )

  lazy val SeleniumTest = config("selenium") extend (Test)

  def normalTestFilter(name: String): Boolean = !seleniumTestFilter(name)
  def seleniumTestFilter(name: String): Boolean = name.contains(".integration.")

  def createBuildProps(outDir: File, verBase: String, verFull: String, rev: String) = {
    val outFile = outDir / BuildPropsFilename
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
}
