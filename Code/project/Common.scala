import sbt._
import Keys._

object Common {
  import Functions._
  import Values._

  val clearScreenTask = TaskKey[Unit]("clear", "Clears the screen.")

  def generateBuildPropFile(filename: String = "build.properties", prefix: String = "build.") = (p: Project) => {
    def createBuildProps(outDir: File, version: String) = {
      val outFile = outDir / filename
      val props = Map[String, String](
        "version" -> version,
        "revision" -> gitRevision,
        "time" -> fmtTimeNow("yyyy-MM-dd HH:mm:ss")
      )
      val contents = props.toList.map {case (k, v) => s"${prefix}$k=$v" }.mkString("\n")
      IO.write(outFile, contents)
      Seq(outFile)
    }
    p.settings(resourceGenerators in Compile <+= (resourceManaged in Compile, version) map createBuildProps)
  }

  def compilerFlags = Seq(
    "-unchecked",
    "-deprecation",
    "-Yno-generic-signatures",
    "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

  lazy val settings = (p: Project) => p
    .settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*) // Dependency graph
    .settings(
      clearScreenTask := { println("\033[2J\033[;H") },
      scalacOptions ++= compilerFlags
    )

  // ===================================================================================================================
  object Values {

    lazy val releaseMode: Boolean = {
      val mode = System.getProperty("MODE", "").trim
      val r = mode.compareToIgnoreCase("release") == 0
      if (r) println("Release Mode.")
      r
    }

    lazy val snapshotSuffix: String =
      if (releaseMode) "" else "-SNAPSHOT"

    lazy val timeNow = new java.util.Date

    lazy val gitRevision = Process("git rev-parse HEAD").lines.head.trim
    lazy val gitRevisionShort = gitRevision.substring(0, 8)
  }

  // ===================================================================================================================
  object Functions {

    def fmtTimeNow(fmt: String): String = new java.text.SimpleDateFormat(fmt).format(timeNow)

    def debugOrRelease(debug: Project => Project, release: Project => Project): Project => Project =
      p => (if (releaseMode) release else debug)(p)

    def nonTestCompilerFlags(flags: String*): Project => Project =
      _.settings(
        scalacOptions in Compile ++= flags,
        scalacOptions in Test ~= (_ filterNot (flags contains _))
      )
  }
}
