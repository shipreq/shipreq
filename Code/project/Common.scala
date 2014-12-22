import sbt._
import Keys._
import java.nio.file.{Files, Path}
import scala.scalajs.sbtplugin.ScalaJSPlugin._

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
    // "-Xstrict-inference", // Don't infer known-unsound types
    // "-Yno-generic-signatures", // Stuffs up json4s
    "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

  def scalacTestFlags = Seq("-language:reflectiveCalls")

  def debugAndReleaseCompilerFlags = debugOrRelease(
    _.settings(
      scalacOptions ++= Seq("-Xcheckinit"),
      cleanKeepFiles ++= Seq("resolution-cache", "streams").map(target.value / _) // stop those constant dep updates
    ),
    nonTestCompilerFlags(
      // "-optimise",
      "-Ybackend:GenBCode",
      "-Yclosure-elim",
      "-Yconst-opt",
      "-Ydead-code",
      "-Yinline",
      "-Yinline-handlers",
      // "-Yinline-warnings",
      "-Xelide-below", "OFF")
  )

  def targetJdk = "1.8"

  def javacFlags = Seq("-target", targetJdk, "-source", targetJdk)

  def getMethod(loader: ClassLoader, className: String, methodName: String): Option[java.lang.reflect.Method] =
    try {
      Option(loader.loadClass(className).getMethod(methodName))
    } catch {
      case  _: Throwable => None
    }

  def shutdownTestDb(loader: ClassLoader): Unit = {
    getMethod(loader, "shipreq.base.test.specs2.db.TestDb", "shutdown").foreach(_ invoke null)
    getMethod(loader, "shipreq.webapp.test.TestJetty", "shutdown").foreach(_ invoke null)
    getMethod(loader, "shipreq.webapp.db.DB", "shutdown").foreach(_ invoke null)
  }

  lazy val settings = (p: Project) => p
    .settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*) // Dependency graph
    .settings(
      clearScreenTask := { println("\033[2J\033[;H") },
      organization := "com.beardedlogic.shipreq",
      organizationName := "Bearded Logic",
      version := s"${fmtTimeNow("yyyyMMdd")}-${gitRevisionShort}${snapshotSuffix}",
      isSnapshot := snapshotSuffix.nonEmpty,
      shellPrompt in ThisBuild := { (s: State) => Project.extract(s).currentRef.project + "> " },
      incOptions := incOptions.value.withNameHashing(true),
      updateOptions := updateOptions.value.withConsolidatedResolution(true),
      javacOptions ++= javacFlags,
      scalaVersion := Deps.Scala.version,
      scalacOptions ++= scalacFlags,
      scalacOptions in Test ++= scalacTestFlags,
      testOptions in Test += Tests.Cleanup(shutdownTestDb(_))
    )
    .configure(
      debugAndReleaseCompilerFlags,
      addCommandAliases(
        "/"    -> "project root",
        "P"    -> "project prop",
        "B"    -> "project base",
        "T"    -> "project taskman",
        "W"    -> "project webapp",
        "TAI"  -> "project taskman-api-impl",
        "TAL"  -> "project taskman-api-logic",
        "TSI"  -> "project taskman-server-impl",
        "TSL"  -> "project taskman-server-logic",
        "WB"   -> "project webapp-base",
        "WT"   -> "project webapp-base-test",
        "WC"   -> "project webapp-client",
        "WS"   -> "project webapp-server",
        "cc"   -> ";clear;compile",
        "ctc"  -> ";clear;test:compile",
        "ct"   -> ";clear;test",
        "cq"   -> ";clear;testQuick",
        "ccc"  -> ";clear;clean;compile",
        "cctc" -> ";clear;clean;test:compile",
        "cct"  -> ";clear;clean;test")
    )

  def useHiddenTargetDir: Project => Project =
    _.settings(target <<= baseDirectory(_ / ".target"))

  def scalaAndScalaJsShared: Project => Project =
    _.settings(testFrameworks += new TestFramework("utest.runner.JvmFramework"))

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

    def dontInline: Project => Project =
      _.settings(scalacOptions in Compile ~= removeValues("-optimise", "-Yinline"))

    def ln_s(src: File, tgt: File, log: Logger): Unit = ln_s(src.toPath, tgt.toPath, log)
    def ln_s(src: Path, tgt: Path, log: Logger): Unit = {
      if (Files.isSymbolicLink(tgt) && Files.readSymbolicLink(tgt).equals(src))
        log.debug(s"Symlink up-to-date: $tgt")
      else {
        log.info(s"Creating symlink $tgt -> $src")
        Files.deleteIfExists(tgt)
        Files.createSymbolicLink(tgt, src)
      }
    }

    def ln(src: File, tgt: File, log: Logger): Unit = ln(src.toPath, tgt.toPath, log)
    def ln(src: Path, tgt: Path, log: Logger): Unit = {
      log.info(s"Creating hard link $tgt -> $src")
      Files.deleteIfExists(tgt)
      Files.createLink(tgt, src)
    }

    def addCommandAliases(m: (String, String)*) = {
      val s = m.map(p => addCommandAlias(p._1, p._2)).reduce(_ ++ _)
      (_: Project).settings(s: _*)
    }
  }
}
