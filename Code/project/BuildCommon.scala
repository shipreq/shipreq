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

  def scalacFlags = Seq(
    "-unchecked",
    "-deprecation",
    // "-Yno-generic-signatures", // Stuffs up json4s
    "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

  def scalacTestFlags = Seq("-language:reflectiveCalls")

  def debugAndReleaseCompilerFlags = debugOrRelease(
    _.settings(scalacOptions ++= Seq("-Xcheckinit")),
    nonTestCompilerFlags("-optimise", /*"-Yinline-warnings",*/ "-Xelide-below", "OFF"))

  def targetJdk = "1.7"

  def javacFlags = Seq("-target", targetJdk, "-source", targetJdk)

  lazy val settings = (p: Project) => p
    .settings((
        net.virtualvoid.sbt.graph.Plugin.graphSettings ++ // Dependency graph
        addCommandAlias("cc",   ";clear;compile") ++
        addCommandAlias("ctc",  ";clear;test:compile") ++
        addCommandAlias("ct",   ";clear;test") ++
        addCommandAlias("ccc",  ";clear;clean;compile") ++
        addCommandAlias("cctc", ";clear;clean;test:compile") ++
        addCommandAlias("cct",  ";clear;clean;test")
      ): _*)
    .settings(
      clearScreenTask := { println("\033[2J\033[;H") },
      version := s"${fmtTimeNow("yyyyMMdd")}-${gitRevisionShort}${snapshotSuffix}",
      isSnapshot := snapshotSuffix.nonEmpty,
      javacOptions ++= javacFlags,
      scalaVersion := Deps.Scala.version,
      scalacOptions ++= scalacFlags,
      scalacOptions in Test ++= scalacTestFlags,
      cleanKeepFiles ++= Seq("resolution-cache", "streams").map(target.value / _) // stop those constant dep updates
    )
    .configure(debugAndReleaseCompilerFlags)

  def useHiddenTargetDir: Project => Project =
    _.settings(target <<= baseDirectory(_ / ".target"))

  trait ExportsTestLib {
    lazy val TestLib = config("test-lib") extend Compile describedAs "Reusable test helpers"

    def testLibSettings = (p: Project) =>
      p.configs(TestLib)
      .settings(inConfig(TestLib)(Defaults.configSettings): _*)
      .settings(
        classpathConfiguration in Test := Test extend TestLib,
        scalacOptions in TestLib <<= scalacOptions in Test,
        javacOptions in TestLib <<= javacOptions in Test
      )
  }

  // ===================================================================================================================
  object Values {

    lazy val releaseMode: Boolean = {
      val mode = System.getProperty("MODE", "").trim
      val r = mode.compareToIgnoreCase("release") == 0
      if (r) println("[mode] \033[1;31mRelease Mode.\033[0m")
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
        scalacOptions in Test ~= removeValues(flags: _*)
      )

    def removeValues[T](values: T*): Seq[T] => Seq[T] = (_ filterNot (values contains _))
  }
}
