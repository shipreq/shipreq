import sbt._
import Keys._
import java.nio.file.{Files, Path}
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
      scalacOptions ++= Seq("-Xcheckinit"),
      cleanKeepFiles ++= Seq("resolution-cache", "streams").map(target.value / _) // stop those constant dep updates
    )

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
      Option(loader.loadClass(className).getMethod(methodName))
    } catch {
      case  _: Throwable => None
    }

  def shutdownTestDb(loader: ClassLoader): Unit = {
    getMethod(loader, "shipreq.base.test.specs2.db.TestDb",   "shutdown").foreach(_ invoke null)
    getMethod(loader, "shipreq.webapp.server.test.TestJetty", "shutdown").foreach(_ invoke null)
    getMethod(loader, "shipreq.webapp.server.db.DB",          "shutdown").foreach(_ invoke null)
  }

  val redirectTargetDir: File => File =
    System.getenv("SHIPREQ_TARGET") match {
      case null | "" => identity
      case envValue =>
        val newTarget = envValue.replaceFirst("/*$", "/experiments/")
        val codePathRegex = "^.+/Code/".r
        println(s"[info] Redirecting targets to $newTarget")
        oldTarget => {
          val a = oldTarget.getAbsolutePath
          val b = codePathRegex.replaceFirstIn(a, newTarget)
          file(b)
        }
    }

  /** Minimal settings used by benchmark modules too */
  lazy val settingsMin = (p: Project) => p
    .settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*) // Dependency graph
    .settings(
      clearScreenTask             := { println("\033[2J\033[;H") },
      organization                := "com.beardedlogic.shipreq.experiment",
      organizationName            := "Bearded Logic",
      version                     := s"${fmtTimeNow("yyyyMMdd")}-${gitRevisionShort}${snapshotSuffix}",
      isSnapshot                  := snapshotSuffix.nonEmpty,
      shellPrompt in ThisBuild    := { (s: State) => Project.extract(s).currentRef.project + "> " },
      incOptions                  := incOptions.value.withNameHashing(true),
      updateOptions               := updateOptions.value.withCachedResolution(true),
      aggregate in update         := true,
      scalaVersion                := Dependencies.Scala.version,
      javacOptions               ++= javacFlags,
      scalacOptions              ++= scalacFlags,
      dependencyUpdatesExclusions := moduleFilter(name = new PatternFilter("^jetty-(?:server|websocket)$".r.pattern)),
      testFrameworks              += new TestFramework("utest.runner.Framework"),
      target                      := redirectTargetDir(target.value)
    )
    .configure(
      addCommandAliases(
        "C"    -> "root/clean",
        "/"    -> "project root",
        "cc"   -> ";clear;compile",
        "ctc"  -> ";clear;test:compile",
        "ct"   -> ";clear;test",
        "cq"   -> ";clear;testQuick",
        "ccc"  -> ";clear;clean;compile",
        "cctc" -> ";clear;clean;test:compile",
        "cct"  -> ";clear;clean;test"))

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

  def jsSettings(t: JsTestType) = (p: Project) => {
    import sbinary.DefaultProtocol.StringFormat
    import Cache.seqFormat
    p.settings(
      scalaJSStage in Global := jsStage,
      parallelExecution in testOnly := false,
      // Temp fix for https://github.com/scala-js/scala-js/issues/1817
      inConfig(Test)(Seq(
        definedTestNames <<= definedTests map (_.map(_.name).distinct) storeAs definedTestNames
      ))
    ).configure(jsTests(t))
  }

  def jsStage = if (releaseMode) FullOptStage else FastOptStage

  private def jsTests(t: JsTestType): Project => Project =
    t match {
      case NoTests =>
        _.settings(
          scalaJSStage in Test := PreLinkStage,
          test                 := ())
      case NoDom =>
        _.settings(
          requiresDOM           := false)
//          postLinkJSEnv in Test := NodeJSEnv().value)
//          postLinkJSEnv  in Test := new PhantomJS2Env(scalaJSPhantomJSClassLoader.value))
      case NeedDom =>
        _.settings(
          requiresDOM            := true,
          emitSourceMaps in Test := false, // PhantomJS doesn't use
          postLinkJSEnv  in Test := new PhantomJS2Env(scalaJSPhantomJSClassLoader.value))
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
