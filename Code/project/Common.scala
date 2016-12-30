import sbt._
import Keys._
import java.nio.file.{Files, Path}
import scala.concurrent.duration._
import com.typesafe.sbt.GitPlugin.autoImport._
import org.scalajs.core.tools.sem._
import org.scalajs.sbtplugin.cross.CrossProject
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import com.timushev.sbt.updates.UpdatesKeys._
import DependencyLib.{Dep, HasJs, HasJvm, HasBoth, JVM, JS, ModDepScope}

sealed trait JsTestType
case object NoTests extends JsTestType
case object NoDom   extends JsTestType
case object NeedDom extends JsTestType

object Common {
  import Functions._
  import Values._

  def generateBuildPropFile(filename: String = "build.properties", prefix: String = "build.") = (p: Project) => {
    def createBuildProps = Def.task {
      val outDir: File = resourceManaged.in(Compile).value
      val outFile = outDir / filename
      val props = Map[String, String](
        "version" -> version.value,
        "revision" -> git.gitHeadCommit.value.getOrElse("?"),
        "time" -> fmtTimeNow("yyyy-MM-dd HH:mm:ss")
      )
      val contents = props.toList.map {case (k, v) => s"${prefix}$k=$v" }.mkString("\n")
      IO.write(outFile, contents)
      Seq(outFile)
    }
    p.settings(resourceGenerators in Compile += createBuildProps.taskValue)
  }

  def targetJdk = "1.8"

  def scalacFlags = Seq(
    "-unchecked",
    "-deprecation",
    "-target:jvm-" + targetJdk,
    "-Ypatmat-exhaust-depth", "off",
    //"-Ybackend:GenBCode",
    //"-Ydelambdafy:method",
    // "-Xstrict-inference", // Don't infer known-unsound types
    // "-Yno-generic-signatures", // Stuffs up json4s
    "-YclasspathImpl:flat", // https://github.com/scala/scala/pull/4176
    "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

  def scalacTestFlags = Seq("-language:reflectiveCalls")

  val debugSettings: Project => Project =
    _.settings(
      scalacOptions ++= Seq("-Xcheckinit"))

  val optimisationSettings: Project => Project =
    nonTestCompilerFlags(
      // "-optimise", // incompatible with GenBCode
      //"-Yopt:l:classpath", // new GenBCode optimiser
      "-Yclosure-elim",
      "-Yconst-opt",
      "-Ydead-code",
      "-Yinline",
      "-Yinline-handlers",
      // "-Yinline-warnings",
      "-Xelide-below", "OFF")

  def javacFlags = Seq("-target", targetJdk, "-source", targetJdk)

  def getMethod(loader: ClassLoader, className: String, methodName: String): Option[java.lang.reflect.Method] =
    try {
      Option(loader.loadClass(className).getDeclaredMethod(methodName))
    } catch {
      case  _: Throwable => None
    }

  def shutdownTestDb(loader: ClassLoader): Unit = {
    getMethod(loader, "shipreq.base.test.db.TestDb",          "shutdown").foreach(_ invoke null)
    getMethod(loader, "shipreq.webapp.server.test.TestDb",    "shutdown").foreach(_ invoke null)
    getMethod(loader, "shipreq.webapp.server.test.TestJetty", "shutdown").foreach(_ invoke null)
  }

  val redirectTargetDir: File => File =
    System.getenv(if (releaseMode) "SHIPREQ_RELEASE_TARGET" else "SHIPREQ_TARGET") match {
      case null | "" => identity
      case envValue =>
        val newTarget = envValue.replaceFirst("/*$", "/")
        val codePathRegex = "^.+/Code/".r
        println(s"[info] Redirecting targets to $newTarget")
        oldTarget => {
          val a = oldTarget.getAbsolutePath
          val b = codePathRegex.replaceFirstIn(a, newTarget)
          file(b)
        }
    }

  def regexFilter(r: String) = new PatternFilter(r.r.pattern)

  /** Minimal settings used by benchmark modules too */
  lazy val settingsMin = (p: Project) => p
    .enablePlugins(net.virtualvoid.sbt.graph.DependencyGraphPlugin)
    .enablePlugins(com.typesafe.sbt.GitVersioning)
    .settings(
      organization                := "com.beardedlogic.shipreq",
      organizationName            := "Bearded Logic",
      shellPrompt in ThisBuild    := { (s: State) => Project.extract(s).currentRef.project + "> " },
      incOptions                  := incOptions.value.withNameHashing(true),
      incOptions                  := incOptions.value.withLogRecompileOnMacro(false),
      updateOptions               := updateOptions.value.withCachedResolution(true),
      aggregate in update         := true,
      scalaVersion                := Dependencies.Scala.version,
      javacOptions               ++= javacFlags,
      scalacOptions              ++= scalacFlags,
      dependencyUpdatesExclusions := moduleFilter(name = regexFilter("^(jetty-(server|websocket)|ammonite)$")) |
                                     moduleFilter(organization = regexFilter("^org.scala-lang$")),
      testFrameworks              += new TestFramework("utest.runner.Framework"),
      minForcegcInterval          := 3.minutes,
      triggeredMessage            := Watched.clearWhenTriggered,
      target                      := redirectTargetDir(target.value)
    )
    .configure(
      addCommandAliases(
        "B"   -> "project base",
        "BU"  -> "project base-util-jvm",
        "BT"  -> "project base-test-jvm",
        "T"   -> "project taskman",
        "W"   -> "project webapp",
        "TAI" -> "project taskman-api-impl",
        "TAL" -> "project taskman-api-logic",
        "TSI" -> "project taskman-server-impl",
        "TSL" -> "project taskman-server-logic",
        "WB"  -> "project webapp-base-jvm",
        "WT"  -> "project webapp-base-test-jvm",
        "WC"  -> "project webapp-client",
        "WCB" -> "project webapp-client-base",
        "WCH" -> "project webapp-client-home",
        "WCP" -> "project webapp-client-project",
        "WCW" -> "project webapp-client-ww",
        "WS"  -> "project webapp-server",
        "BM"  -> "project benchmark-jvm",
        "BMJ" -> "project benchmark-js",
        "/"   -> "project root",
        "C"   -> "root/clean",
        "T"   -> ";root/clean;root/test",
        "c"   -> "compile",
        "tc"  -> "test:compile",
        "t"   -> "test",
        "to"  -> "test-only",
        "tq"  -> "testQuick",
        "cc"  -> ";clean;compile",
        "ctc" -> ";clean;test:compile",
        "ct"  -> ";clean;test"))

  /** Common settings used by standard modules - not benchmarks, not test modules */
  lazy val settings = (p: Project) => settingsMin(p)
    .settings(
      scalacOptions in Test ++= scalacTestFlags,
      testOptions   in Test  += Tests.Cleanup(shutdownTestDb(_)))
    .configure(
      debugOrRelease(debugSettings, optimisationSettings))

  lazy val testModuleSettings = (p: Project) => settingsMin(p)
    .settings(
      scalacOptions      ++= scalacTestFlags,
      testOptions in Test += Tests.Cleanup(shutdownTestDb(_)))
    .configure(
      debugOrRelease(debugSettings, identity))

  lazy val macroModuleSettings = (p: Project) => settingsMin(p)
    .configure(
      definesMacros,
      debugOrRelease(debugSettings, identity))

  def definesMacros: Project => Project =
    _.settings(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies ++= Dependencies.Scala.macroDef(JVM))

  def jvmSettings: Project => Project =
    _.configure(
      InBrowserTesting.jvm)
//    _.settings(
//      scalacOptions       ++= Seq("-Ybackend:GenBCode", "-Ydelambdafy:method"),
//      libraryDependencies  += Dependencies.Scala.java8compat)

  def jsSettings(t: JsTestType): Project => Project =
    _.configure(
      jsTests(t),
      debugOrRelease(jsDevSettings, jsProdSettings),
      Dependencies.useJavaTimeJS,
      InBrowserTesting.js)
    .settings(
      parallelExecution in testOnly := false,
      // scalaJSOptimizerOptions in fullOptJS ~= (_ withPrettyPrintFullOptJS true),
      scalaJSSemantics in fullOptJS ~= (_
        .withRuntimeClassName(_ => "")
        .withAsInstanceOfs(CheckedBehavior.Unchecked)
        ))

  private def jsDevSettings = (_: Project).settings(
    emitSourceMaps := true)

  private def jsProdSettings = (_: Project).settings(
    emitSourceMaps := false,
    scalaJSStage := FullOptStage,
    scalaJSOptimizerOptions ~= (_
      .withBatchMode(true)
      .withCheckScalaJSIR(true)
      ))

  // Compile-scope only
  def jsFastDevSettings = (_: Project).settings(
    scalaJSOptimizerOptions in fastOptJS ~= { _.withDisableOptimizer(true) },
    emitSourceMaps in Compile in fastOptJS := false)

  private def jsTests(t: JsTestType): Project => Project =
    t match {
      case NoTests =>
        _.settings(test := ())
      case NoDom =>
        _.settings(
          requiresDOM := false)
//          jsEnv in Test := NodeJSEnv().value)
//          jsEnv in Test := new PhantomJS2Env(scalaJSPhantomJSClassLoader.value))
      case NeedDom =>
        _.settings(
          requiresDOM                         := true,
          emitSourceMaps in fastOptJS in Test := false, // PhantomJS doesn't use
          jsEnv                       in Test := new PhantomJS2Env(scalaJSPhantomJSClassLoader.value))
    }

  // ===================================================================================================================
  object Values {

    lazy val releaseMode: Boolean = {
      val mode = System.getProperty("MODE", "").trim
      val r = mode.compareToIgnoreCase("release") == 0
      if (r) println("[mode] \033[1;31mRelease Mode.\033[0m")
      r
    }

    def devMode: Boolean = !releaseMode

    lazy val timeNow = new java.util.Date
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

    def removeValues[T](values: T*): Seq[T] => Seq[T] =
      _ filterNot values.contains

    def dontInline: Project => Project =
      _.settings(scalacOptions in Compile ~= removeValues("-optimise", "-Yinline"))

    def ln_s(src: File, tgt: File)(implicit log: Logger): Unit = ln_s(src.toPath, tgt.toPath)(log)
    def ln_s(src: Path, tgt: Path)(implicit log: Logger): Unit = {
      if (Files.isSymbolicLink(tgt) && Files.readSymbolicLink(tgt).equals(src))
        log.debug(s"Symlink up-to-date: $tgt")
      else {
        log.info(s"Creating symlink $tgt -> $src")
        Files.deleteIfExists(tgt)
        Files.createSymbolicLink(tgt, src)
      }
    }

    def ln(src: File, tgt: File)(implicit log: Logger): Unit = ln(src.toPath, tgt.toPath)(log)
    def ln(src: Path, tgt: Path)(implicit log: Logger): Unit = {
      log.info(s"Creating hard link $tgt -> $src")
      Files.deleteIfExists(tgt)
      Files.createLink(tgt, src)
    }

    def fileSync(from: File, to: File, mandatory: Boolean)(implicit log: Logger): Unit =
      if (from.exists()) {
        log.info(s"Copying $from → $to")
        IO.copyFile(from, to, preserveLastModified = true)
      } else if (mandatory)
        sys.error("File not found: " + from.absolutePath)
      else if (to.exists()) {
        log.info(s"Deleting $to")
        IO.delete(to)
      }

    def addCommandAliases(m: (String, String)*) = {
      val s = m.map(p => addCommandAlias(p._1, p._2)).reduce(_ ++ _)
      (_: Project).settings(s: _*)
    }

    implicit class CrossProjectExt(val p: CrossProject) extends AnyVal {

      def configureBoth(fs: (Project => Project)*): CrossProject =
        fs.foldLeft(p)((q,f) => q.jvmConfigure(f).jsConfigure(f))

      def configureJvm(fs: (Project => Project)*): CrossProject =
        fs.foldLeft(p)((q,f) => q.jvmConfigure(f))

      def configureJs(fs: (Project => Project)*): CrossProject =
        fs.foldLeft(p)((q,f) => q.jsConfigure(f))

      def depsForBoth(deps: Dep[HasBoth]): CrossProject =
        depsForJvm(deps.widen).depsForJs(deps.widen)

      def depsForJvm(deps: Dep[HasJvm]): CrossProject =
        p.jvmSettings(libraryDependencies ++= deps(JVM))

      def depsForJs(deps: Dep[HasJs]): CrossProject =
        p.jsSettings(libraryDependencies ++= deps(JS))

      def aggregateJvm(refs: sbt.ProjectReference*):  CrossProject =
        p.jvmConfigure(_.aggregate(refs: _*))

      def aggregateJs(refs: sbt.ProjectReference*):  CrossProject =
        p.jsConfigure(_.aggregate(refs: _*))
    }

    implicit class ProjectExt(val p: Project) extends AnyVal {
      def deps(deps: Dep[HasJvm]): Project =
        p.settings(libraryDependencies ++= deps(JVM))

      def depsForJs(deps: Dep[HasJs]): Project =
        p.settings(libraryDependencies ++= deps(JS))
    }

    def depScope(s: String): ModDepScope = ModDepScope(s)
    def depScope(c: Configuration): ModDepScope = depScope(c.name)
    def testScope = depScope("test")
    def providedScope = depScope("provided")
  }
}
