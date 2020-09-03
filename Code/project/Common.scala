import sbt._
import sbt.Keys._
import com.timushev.sbt.updates.UpdatesPlugin.autoImport._
import com.typesafe.sbt.GitPlugin.autoImport._
import java.nio.file.{Files, Path}
import org.scalajs.jsenv.Input
import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
import org.scalajs.jsenv.phantomjs.sbtplugin.PhantomJSEnvPlugin.autoImport._
import org.scalajs.linker.interface.{CheckedBehavior, Semantics}
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.scalajs.sbtplugin.Stage
import sbtcrossproject.CrossPlugin.autoImport._
import sbtcrossproject.CrossProject
import scala.{Console => C}
import scala.concurrent.duration._
import scalafix.sbt.ScalafixPlugin
import scalafix.sbt.ScalafixPlugin.autoImport.ScalafixConfig
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import LibDependency.{Dep, HasBoth, HasJs, HasJvm, JS, JVM, ModDepScope}

sealed trait JsTestType
case object NoTests      extends JsTestType
case object UseNode      extends JsTestType
case object UsePhantomJs extends JsTestType

object Common {

  private val cores = java.lang.Runtime.getRuntime.availableProcessors()

  private def readConfigVar(name: String): String =
    Option(System.getProperty(name)).orElse(Option(System.getenv(name)))
      .fold("")(_.trim.toLowerCase)

  val releaseMode = readConfigVar("MODE") == "release"
  if (releaseMode) {
    println(s"[info] ${C.RED_B}${C.WHITE}Release Mode.${C.RESET}")
  }

  val inCI = readConfigVar("CI") == "1"
  if (inCI) {
    println(s"[info] ======== CI Mode ========")
    println(s"[info] $cores cores available")
  }

  def scalafixEnabled =
    !releaseMode

  lazy val emitSourceMapsValue: Boolean =
    System.getProperty("emitSourceMaps", "0").trim.toLowerCase match {
      case "0" | "no" | "n" | "off" => false
      case _                        =>
        println("[info] \u001b[1;93mSource maps enabled.\u001b[0m")
        true
    }

  def scalacFlags = Seq(
    "-deprecation",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-target:" + Dependencies.Java.major,            // Target platform for object files. ([8],9,10,11,12)
    "-unchecked",                                    // Enable additional warnings where generated code depends on assumptions.
    "-Wconf:msg=may.not.be.exhaustive:e",            // Make non-exhaustive matches errors instead of warnings
    "-Wdead-code",                                   // Warn when dead code is identified.
    "-Wunused:explicits",                            // Warn if an explicit parameter is unused.
    "-Wunused:implicits",                            // Warn if an implicit parameter is unused.
    "-Wunused:imports",                              // Warn if an import selector is not referenced.
    "-Wunused:locals",                               // Warn if a local definition is unused.
    "-Wunused:nowarn",                               // Warn if a @nowarn annotation does not suppress any warnings.
    "-Wunused:patvars",                              // Warn if a variable bound in a pattern is unused.
    "-Wunused:privates",                             // Warn if a private member is unused.
    "-Xlint:adapted-args",                           // An argument list was modified to match the receiver.
    "-Xlint:constant",                               // Evaluation of a constant arithmetic expression resulted in an error.
    "-Xlint:delayedinit-select",                     // Selecting member of DelayedInit.
    "-Xlint:deprecation",                            // Enable -deprecation and also check @deprecated annotations.
    "-Xlint:eta-zero",                               // Usage `f` of parameterless `def f()` resulted in eta-expansion, not empty application `f()`.
    "-Xlint:implicit-not-found",                     // Check @implicitNotFound and @implicitAmbiguous messages.
    "-Xlint:inaccessible",                           // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                              // A type argument was inferred as Any.
    "-Xlint:missing-interpolator",                   // A string literal appears to be missing an interpolator id.
    "-Xlint:nonlocal-return",                        // A return statement used an exception for flow control.
    "-Xlint:nullary-unit",                           // `def f: Unit` looks like an accessor; add parens to look side-effecting.
    "-Xlint:option-implicit",                        // Option.apply used an implicit view.
    "-Xlint:poly-implicit-overload",                 // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",                         // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                            // In a pattern, a sequence wildcard `_*` should match all of a repeated parameter.
    "-Xlint:valpattern",                             // Enable pattern checks in val definitions.
    "-Xmixin-force-forwarders:false",                // Only generate mixin forwarders required for program correctness.
    "-Xno-forwarders",                               // Do not generate static forwarders in mirror classes.
    "-Xsource:2.13",
    "-Ybackend-parallelism", cores.min(16).toString,
    "-Ycache-macro-class-loader:last-modified",
    "-Ycache-plugin-class-loader:last-modified",
    "-Yimports:java.lang,scala,shipreq.Predef",      // Use custom Predef
    "-Yjar-compression-level", "9",                  // compression level to use when writing jar files
    "-Ymacro-annotations",                           // Enable support for macro annotations, formerly in macro paradise.
    "-Yno-generic-signatures",                       // Suppress generation of generic signatures for Java.
    "-Ypatmat-exhaust-depth", "off"
  )
/*
    "-Xsource:2.14",                                 // Prepare for Dotty -- Disabled because of warnings in macro-generated code. Fix 3rd-libs first.
*/

  def scalacTestFlags = Seq(
    "-language:reflectiveCalls")

  def scalacTestNonFlags = Seq(
    "-Xlint:valpattern")

  val debugSettings: Project => Project =
    _.settings(
      scalacOptions ++= Seq("-Xcheckinit"))

  def optimisationScalacFlags = Seq(
    "-opt:l:method",
    "-opt:l:inline",
    "-opt-inline-from:**",
    //"-opt-warnings:at-inline-failed",
  )

  val optimisationSettings: Project => Project =
    nonTestCompilerFlags(
      "-Xdisable-assertions",
      "-Xelide-below", "OFF"
    ) compose
    nonTestCompilerFlags(optimisationScalacFlags: _*)

  val ciSettings: Project => Project =
    if (inCI)
      _.settings(Global / concurrentRestrictions += Tags.limit(Tags.Test, 3))
    else
      identity

  val scalafixSettings: Project => Project =
    if (scalafixEnabled)
      _.enablePlugins(ScalafixPlugin).dependsOn(ScalafixBuild.`scalafix-rules` % ScalafixConfig)
    else
      _.disablePlugins(ScalafixPlugin)

  val redirectTargetDir: File => File =
    System.getenv(if (releaseMode) "SHIPREQ_RELEASE_TARGET" else "SHIPREQ_TARGET") match {
      case null | "" => identity
      case envValue =>
        val newTarget = envValue.replaceFirst("/*$", "/")
        val codePathRegex = "^.+/[cC]ode/".r
        println(s"[info] Redirecting targets to $newTarget")
        oldTarget => {
          val a = oldTarget.getAbsolutePath
          val b = codePathRegex.replaceFirstIn(a, newTarget)
          file(b)
        }
    }

  private def versionFn(gitSha: Option[String], snapshot: Boolean): String = {
    var v = gitSha.getOrElse("UNKNOWN")
    if (devMode) v += "-dev"
    if (snapshot) v += "-SNAPSHOT"
    v
  }

  def packageBinaryOnly = (_: Project)
    .settings(
      sources in (Compile, doc) := Nil,
      publishArtifact in (Compile, packageDoc) := false,
      publishArtifact in (Compile, packageSrc) := false,
      publishArtifact in packageDoc := false,
      publishArtifact in packageSrc := false)

  def dockerLayerReuse = (_: Project)
    .settings(
      // Remove versions from filenames
      artifactName in (Compile, packageBin) := ((_, _, a) => a.name + "." + a.extension),
      // Remove versions from manifests
      packageOptions in (Compile, packageBin) := Nil)

  lazy val settingsMinForScalafix = (p: Project) => p
    .settings(
      organization                := "com.beardedlogic.shipreq",
      organizationName            := "Bearded Logic",
      isSnapshot                  := git.gitUncommittedChanges.value,
      version                     := versionFn(git.gitHeadCommit.value, isSnapshot.value),
      shellPrompt in ThisBuild    := ((s: State) => Project.extract(s).currentRef.project + "> "),
      incOptions                  := incOptions.value.withLogRecompileOnMacro(false),
      updateOptions               := updateOptions.value.withCachedResolution(true),
      aggregate in update         := true,
      scalaVersion                := Dependencies.Scala.version,
      scalacOptions              ++= scalacFlags,
    //cancelable in Global        := true, // Allows ctrl-c to kill apps started with run without exiting SBT
      dependencyUpdatesFilter     -= Dependencies.updateExclusions,
      minForcegcInterval          := 3.minutes,
      target                      := redirectTargetDir(target.value))
    .configure(
      packageBinaryOnly,
      ciSettings,
      Dependencies.useKindProjector,
      Dependencies.useBetterMonadicFor)

  /** Minimal settings used by benchmark modules too */
  lazy val settingsMin =
    settingsMinForScalafix.andThen(_
      .configure(
        scalafixSettings,
        dockerLayerReuse)
      .settings(
        testFrameworks := List(new TestFramework("utest.runner.Framework"))
      ))

  /** Common settings used by standard modules - not benchmarks, not test modules */
  private def settings: Project => Project =
    _.configure(settingsMin)
      .settings(
        excludeDependencies += "commons-logging" % "commons-logging", // commons-logging should be replaced by jcl-over-slf4j
        scalacOptions in Test ++= scalacTestFlags,
        scalacOptions in Test --= scalacTestNonFlags)
      .configure(debugOrRelease(debugSettings, optimisationSettings))

  lazy val jvmSettings: Project => Project =
    _.configure(settings, InBrowserTesting.jvm)
      .settings(testOptions in Test += Tests.Cleanup(shutdownTestDb(_)))

  /** This doesn't work when fork := true */
  def shutdownTestDb(loader: ClassLoader): Unit = {
    def invoke(objectName: String, methodName: String): Unit = {
      import scala.util._
      def Try2[A](a: => A) = {
        val t = try Success(a) catch { case e: Throwable => Failure(e) }
//          println(t)
        t
      }
      for {
        objC <- Try2(loader.loadClass(objectName + "$"))
        clsC <- Try2(loader.loadClass(objectName))
        objM <- Try2(objC.getField("MODULE$"))
        clsM <- Try2(clsC.getDeclaredMethod(methodName))
        objI <- Try2(objM.get(null))
        _    <- Try2(clsM.invoke(objI))
      } yield ()
    }

    invoke("shipreq.webapp.server.test.LiveTestUtils", "shutdown")
    invoke("shipreq.webapp.server.test.TestJetty",     "shutdown")
    invoke("shipreq.base.test.db.TestDb",              "shutdown")
  }

  def jsSettings(t: JsTestType): Project => Project =
    _.configure(
      settings,
      jsTests(t),
      debugOrRelease(jsDevSettings, jsProdSettings),
      InBrowserTesting.js)
    .depsForJs(Dependencies.scalajsJavaTime)
    .settings(
      parallelExecution in testOnly := false,
      scalaJSLinkerConfig ~= { _.withSourceMap(emitSourceMapsValue) })

  private def jsDevSettings: Project => Project =
    identity

  private def jsProdSettings: Project => Project =
    _.settings(
      scalaJSStage := FullOptStage,
      scalaJSLinkerConfig ~= { _
        .withSemantics(_
          .withRuntimeClassNameMapper(Semantics.RuntimeClassNameMapper.discardAll())
          .withArrayIndexOutOfBounds(CheckedBehavior.Unchecked)
          .withAsInstanceOfs(CheckedBehavior.Unchecked)
          .withProductionMode(true)
        )
          .withPrettyPrint(false)
          .withClosureCompiler(true)
          .withCheckIR(true)
      },
      // More than 1 running instance of Google Closure exponentially increases time & mem-usage
      Global / concurrentRestrictions += Tags.limit(ScalaJSTags.Link, 1)
    )

  lazy val testModuleSettings = (p: Project) => settingsMin(p)
    .settings(scalacOptions ++= scalacTestFlags)
    .configure(debugOrRelease(debugSettings, identity))

  lazy val macroModuleSettings = (p: Project) => settingsMin(p)
    .configure(
      definesMacros,
      debugOrRelease(debugSettings, identity))

  def definesMacros: Project => Project =
    _.settings(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies ++= Dependencies.Scala.macroDef(JVM))

  private def jsTests(t: JsTestType): Project => Project =
    t match {
      case NoTests =>
        _.settings(test := {})
      case UseNode =>
        _.settings(
          jsEnv in Test := new JSDOMNodeJSEnv(JSDOMNodeJSEnv.Config()))
      case UsePhantomJs =>
        _.settings(
          Test / scalaJSLinkerConfig ~= { _.withESFeatures(_.withUseECMAScript2015(false)) },
          Test / jsEnv := PhantomJSEnv().value,
          Test / jsEnvInput := Input.Script(((ThisBuild / baseDirectory).value / "project/phantomjs-fix.js").toPath) +: (Test / jsEnvInput).value)
    }

  def devMode: Boolean = !releaseMode

  def debugOrRelease(debug: Project => Project, release: Project => Project): Project => Project =
    p => (if (releaseMode) release else debug)(p)

  def nonTestCompilerFlags(flags: String*): Project => Project =
    _.settings(
      scalacOptions in Compile ++= flags,
      scalacOptions in Test --= flags)

  def dontOptimise: Project => Project =
    _.settings(scalacOptions in Compile --= optimisationScalacFlags)

  def dontInline: Project => Project =
    debugOrRelease(identity, _
      .configure(dontOptimise, nonTestCompilerFlags("-opt:l:method")))

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

  def execInBash(cmd: String): Unit =
    try sys.process.Process(List("bash", "-c", cmd)).!!
    catch {
      case t: Throwable =>
        System.err.println(s"> $cmd\n$t")
        throw t
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

  def gzipLength(in: File): Long = {
    import java.io._
    import java.util.zip._
    val bos = new ByteArrayOutputStream()
    try {
      val gzip = new GZIPOutputStream(bos) { this.`def`.setLevel(Deflater.BEST_COMPRESSION) }
      try
        IO.transfer(in, gzip)
      finally
        gzip.close()
    } finally
      bos.close()
    bos.toByteArray.length
  }

  def printFileBatches(batchesT: Iterable[Iterable[File]]): Unit = {
    val sep = "=" * 100
    println(sep)
    val batches = batchesT.toVector
    val sizes = batches.map(_.map(_.length()).foldLeft(0L)(_ + _)).toVector
    (batches zip sizes).foreach { case (files, size) =>
      files.foreach(println)
      printf("%,94d bytes\n", size)
      println(sep)
    }
    println("Sizes:")
    sizes.foreach { size =>
      printf("  %,12d bytes\n", size)
    }
//    println("    ----------------")
//    printf("Σ %,12d bytes\n", sizes.sum)
    println(sep)
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

  def propOrEnv(key: String): Option[String] =
    sys.props.get(key).orElse(sys.env.get(key))

  def jprofilerAgent(wait: Boolean): String = {
    var s = "-agentpath:/opt/jprofiler/bin/linux-x64/libjprofilerti.so=port=8849"
    if (!wait) s += ",nowait"
    s
  }

  def stageKey(stage: Stage) =
    stage match {
      case Stage.FastOpt => fastOptJS
      case Stage.FullOpt => fullOptJS
    }
}
