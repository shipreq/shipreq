import sbt._
import Keys._
import Common.Functions._
import Dependencies._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.cross.{CrossProject, CrossType}
import ScalaJSPlugin.autoImport.{crossProject => _, _}
import TaskmanBuild._
import WebappBuild._

object ShipReqBuild {

  def project(dir: String) =
    Project(dir, file(dir))

  def crossProject(dir: String) =
    CrossProject(dir + "-jvm", dir + "-js", file(dir), CrossType.Full).settings(name := dir)

  lazy val root =
    Project("root", file("."))
      .configure(Common.settings, IdeSettings.settingsForRoot)
      .aggregate(base, taskman, webapp, utils, benchmarkJvm, benchmarkJs)

  // ===================================================================================================================
  // base-* : General utils for taskman, webapp, benchmarking, etc.

  lazy val base =
    project("base")
      .configure(Common.settings)
      .aggregate(baseUtilJvm, baseUtilJs, baseDb, baseTestJvm, baseTestJs)

  lazy val baseUtilJvm = baseUtil.jvm
  lazy val baseUtilJs  = baseUtil.js
  lazy val baseUtil =
    crossProject("base-util")
      .configureBoth(Common.settings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoDom))
      .depsForBoth(
        UnivEq.scalaz ++ Scalaz.effect ++ Nyaya.prop ++ Monocle.core ++
        Microlibs.adtMacros ++ Microlibs.config ++ Microlibs.nonempty ++ Microlibs.scalazExt ++ Microlibs.stdlibExt ++
        testScope(μTest ++ Nyaya.test))
      .depsForJvm(
        SLF4J.api ++
        providedScope(logback) ++
        testScope(Specs2.combo ++ Scalaz.scalacheck)).jvmSettings()

  lazy val baseDb =
    project("base-db")
      .configure(
        Common.settings,
        Common.jvmSettings,
        Common.macroModuleSettings)
      .deps(postgresql ++ Doobie.main ++ hikariCP ++ flyway ++ logback ++ Microlibs.macroUtils)
      .dependsOn(baseUtilJvm)

  lazy val baseTestJvm = baseTest.jvm
  lazy val baseTestJs  = baseTest.js
  lazy val baseTest =
    crossProject("base-test")
      .configureBoth(Common.testModuleSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoDom))
      .dependsOn(baseUtil)
      .depsForBoth(
        Microlibs.testUtil ++
        providedScope(Nyaya.gen) ++
        testScope(μTest ++ Nyaya.test))
      .configureJvm(_
        .deps(providedScope(Specs2.combo))
        .dependsOn(baseDb % "provided"))

  // ===================================================================================================================
  // utils & benchmark-*

  lazy val utils =
    project("utils")
      .configure(Common.settings)
      .deps(
        commonsLang ++ Nyaya.test ++
        testScope(twitterEval)
      )
      .dependsOn(webappBaseTestJvm)
      .settings(
        connectInput in run  := true,
        fork         in run  := true,
        javaOptions  in run ++= Seq("-Xmx4g", "-Xss4m")
      )

  object Benchmark {
    def commonSettings: Project => Project =
      _.configure(Common.settingsMin)

    def jvmSettings: Project => Project = {
      import pl.project13.scala.sbt.SbtJmh
      _.enablePlugins(SbtJmh)
        .dependsOn(webappServer)
        .configure(Common.jvmSettings)
    }

    def jsSettings: Project => Project = {
      import org.scalajs.sbtplugin._
      import ScalaJSPlugin.autoImport._

      val outputJs = "shipreq-benchmark.js"

      _.enablePlugins(ScalaJSPlugin)
        .dependsOn(webappClientProject)
        .depsForJs(scalajsBenchmark)
        .configure(
          Common.jsSettings(NoTests),
          useMacroParadise)
        //.settings(
        //  // skip in packageJSDependencies := false,
        //  // scalaJSStage in Global := FullOptStage,
        //  artifactPath in (Compile, fastOptJS) := ((target in Compile).value / outputJs),
        //  artifactPath in (Compile, fullOptJS) := ((target in Compile).value / outputJs))
    }
  }

  lazy val benchmarkJvm = benchmark.jvm
  lazy val benchmarkJs  = benchmark.js
  lazy val benchmark =
    crossProject("benchmark")
      .configureBoth(Benchmark.commonSettings)
      .dependsOn(webappBase)
      .configureJvm(Benchmark.jvmSettings)
      .configureJs(Benchmark.jsSettings)
}
