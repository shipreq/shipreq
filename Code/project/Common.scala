import sbt._, Keys._
import java.nio.file.{Files, Path}
import scala.concurrent.duration._
import com.typesafe.sbt.GitPlugin.autoImport._
import org.scalajs.core.tools.sem._
import org.scalajs.jsenv.phantomjs.PhantomJSEnv
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{crossProject => _, CrossType => _, _}
import sbtcrossproject.CrossProject
import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import sbtdocker.DockerPlugin, DockerPlugin.autoImport._
import LibDependency.{Dep, HasBoth, HasJs, HasJvm, JS, JVM, ModDepScope}

sealed trait JsTestType
case object NoTests extends JsTestType
case object NoDom   extends JsTestType
case object NeedDom extends JsTestType

object Common {

  def targetJdk = "1." + Dependencies.Java.major

  def scalacFlags = Seq(
    "-unchecked",
    "-deprecation",
    "-target:jvm-" + targetJdk,
    "-Xsource:2.13",
    // "-Xstrict-inference", // Don't infer known-unsound types
    // "-Yno-generic-signatures", // Stuffs up json4s
    "-Ypartial-unification",
    "-Ypatmat-exhaust-depth", "off",
    "-Ywarn-inaccessible",
    "-Ybackend-parallelism", "8",
    "-Ycache-plugin-class-loader:last-modified",
    "-Ycache-macro-class-loader:last-modified",
    "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

  def scalacTestFlags = Seq("-language:reflectiveCalls")

  val debugSettings: Project => Project =
    _.settings(
      scalacOptions ++= Seq("-Xcheckinit"))

  val optimisationSettings: Project => Project =
    nonTestCompilerFlags(
      "-opt:l:method",
      "-opt:l:inline",
      "-opt-inline-from:**",
      //"-opt-warnings:at-inline-failed",
      "-Xelide-below", "OFF")

  def javacFlags = Seq("-target", targetJdk, "-source", targetJdk)

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

  /** Minimal settings used by benchmark modules too */
  lazy val settingsMin = (p: Project) => p
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
      javacOptions               ++= javacFlags,
      scalacOptions              ++= scalacFlags,
      testFrameworks              += new TestFramework("utest.runner.Framework"),
    //cancelable in Global        := true, // Allows ctrl-c to kill apps started with run without exiting SBT
      minForcegcInterval          := 3.minutes,
      triggeredMessage            := Watched.clearWhenTriggered,
      target                      := redirectTargetDir(target.value))
    .configure(
      packageBinaryOnly,
      dockerLayerReuse,
      Dependencies.useKindProjector,
      Dependencies.useBetterMonadicFor,
      addCommandAliases(
        "/"   -> "project root",
        "B"   -> "project base",
        "BU"  -> "project base-util-jvm",
        "BT"  -> "project base-test-jvm",
        "T"   -> "project taskman",
        "W"   -> "project webapp",
        "TA"  -> "project taskman-api",
        "TAL" -> "project taskman-api-logic",
        "TS"  -> "project taskman-server",
        "TSL" -> "project taskman-server-logic",
        "WB"  -> "project webapp-base-jvm",
        "WBM" -> "project webapp-base-member-jvm",
        "WT"  -> "project webapp-base-test-jvm",
        "WC"  -> "project webapp-client",
        "WCA" -> "project webapp-client-public-js", // A for Anonymous
        "WCB" -> "project webapp-client-base",
        "WCH" -> "project webapp-client-home",
        "WCP" -> "project webapp-client-project",
        "WCW" -> "project webapp-client-ww",
        "WSL" -> "project webapp-server-logic-jvm",
        "WS"  -> "project webapp-server",
        "BM"  -> "project benchmark-jvm",
        "BMJ" -> "project benchmark-js",
        "C"   -> "root/clean",
        "CT"  -> ";root/clean;root/test"))

  /** Common settings used by standard modules - not benchmarks, not test modules */
  private def settings: Project => Project =
    _.configure(settingsMin)
      .settings(
        excludeDependencies += "commons-logging" % "commons-logging", // commons-logging should be replaced by jcl-over-slf4j
        scalacOptions in Test ++= scalacTestFlags)
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
    invoke("shipreq.webapp.server.test.TestDb",        "shutdown")
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
      scalacOptions += "-P:scalajs:sjsDefinedByDefault",
      parallelExecution in testOnly := false,
      // scalaJSOptimizerOptions in fullOptJS ~= (_ withPrettyPrintFullOptJS true),
      scalaJSSemantics in fullOptJS ~= (_
        .withProductionMode(true)
        .withRuntimeClassNameMapper(Semantics.RuntimeClassNameMapper.discardAll())
        .withArrayIndexOutOfBounds(CheckedBehavior.Unchecked)
        .withAsInstanceOfs(CheckedBehavior.Unchecked)))

  private def jsDevSettings: Project => Project =
    _.settings(emitSourceMaps := true)

  private def jsProdSettings: Project => Project =
    _.settings(
      emitSourceMaps := false,
      scalaJSStage := FullOptStage,
      scalaJSOptimizerOptions ~= (_
        .withBatchMode(true)
        .withCheckScalaJSIR(true)))

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

  // Compile-scope only
  def jsFastDevSettings = (_: Project).settings(
    scalaJSOptimizerOptions in fastOptJS ~= { _.withDisableOptimizer(true) },
    emitSourceMaps in Compile in fastOptJS := false)

  private def jsTests(t: JsTestType): Project => Project =
    t match {
      case NoTests =>
        _.settings(test := {})
      case NoDom =>
        _.settings(
          jsEnv in Test := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv)
      case NeedDom =>
        _.settings(
          emitSourceMaps in fastOptJS in Test := false, // PhantomJS doesn't use
          jsEnv                       in Test := new PhantomJS2Env(PhantomJSEnv.Config().withJettyClassLoader(scalaJSPhantomJSClassLoader.value)))
//          emitSourceMaps in fastOptJS in Test := true)
    }

  def dockerBaseSettings(name: String): Project => Project =
    _.settings(
      buildOptions in docker := BuildOptions(pullBaseImage = BuildOptions.Pull.IfMissing),
      imageNames in docker := {
        var versions = Seq(version.value, "latest")
        if (!isSnapshot.value && releaseMode) versions :+= "latest-prod"
        versions.map(ver => ImageName(s"shipreq/$name:$ver"))
      }
    )

  def dockerBaseEnv = Def.task(
    List[(String, String)](
      "VERSION" -> version.value,
      "BUILD_MODE" -> (if (releaseMode) "release" else "dev")))

  lazy val releaseMode: Boolean = {
    val mode = System.getProperty("MODE", "").trim
    val r = mode.compareToIgnoreCase("release") == 0
    if (r) println("[mode] \u001b[1;31mRelease Mode.\u001b[0m")
    r
  }

  def devMode: Boolean = !releaseMode

  def debugOrRelease(debug: Project => Project, release: Project => Project): Project => Project =
    p => (if (releaseMode) release else debug)(p)

  def nonTestCompilerFlags(flags: String*): Project => Project =
    _.settings(
      scalacOptions in Compile ++= flags,
      scalacOptions in Test --= flags)

  def dontOptimise: Project => Project =
    _.settings(scalacOptions in Compile -= "-opt:l:classpath")

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

  def printFileBatches(batchesT: Traversable[Traversable[File]]): Unit = {
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
