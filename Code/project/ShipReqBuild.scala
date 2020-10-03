import sbt._
import sbt.Keys._
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport._
import sbtcrossproject.CrossProject
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import Common._
import Dependencies._
import TaskmanBuild._
import WebappBuild._

object ShipReqBuild {

  def project(dir: String): Project =
    Project(dir, file(dir))

  def crossProject(dir: String): CrossProject =
    CrossProject(dir, file(dir))(JVMPlatform, JSPlatform)
      .jvmConfigure(_.withId(dir + "-jvm"))
      .jsConfigure(_.withId(dir + "-js"))
      .settings(name := dir)

  lazy val root =
    Project("root", file("."))
      .configure(Common.jvmSettings)
      .aggregate(base, taskman, webapp, utils, benchmarkJvm, benchmarkJs)
      .aggregate(ScalafixBuild.projects: _*)

  // ===================================================================================================================
  // base-* : General utils for taskman, webapp, benchmarking, etc.

  lazy val base =
    project("base")
      .configure(Common.jvmSettings)
      .aggregate(basePredefJvm, basePredefJs, baseUtilJvm, baseUtilJs, baseOps, baseDb, baseTestJvm, baseTestJs)

  lazy val basePredefJvm = basePredef.jvm
  lazy val basePredefJs  = basePredef.js
  lazy val basePredef =
    crossProject("base-predef")
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .depsForBoth(UnivEq.scalaz ++ scalaz ++ Nyaya.prop ++ Microlibs.nonempty)
      .settings(
        scalacOptions ~= (_.filterNot(_.startsWith("-Yimports:")) :+ "-Yimports:scala"),
        test := (()))

  lazy val baseUtilJvm = baseUtil.jvm
  lazy val baseUtilJs  = baseUtil.js
  lazy val baseUtil =
    crossProject("base-util")
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(UseNode))
      .dependsOn(basePredef)
      .depsForBoth(
        UnivEq.scalaz ++ scalaz ++ Nyaya.prop ++ Monocle.core ++
        Microlibs.adtMacros ++ Microlibs.nonempty ++ Microlibs.recursion ++
        Microlibs.scalazExt ++ Microlibs.stdlibExt ++ Microlibs.utils ++
        (Circe.main % Provided) ++
        testScope(μTest ++ Nyaya.test ++ Microlibs.testUtil))
      .depsForJvm(
        SLF4J.api ++ Logback.withPlugins ++ scalaLogging ++ clearConfig ++ catsEffect)

  lazy val baseOps =
    project("base-ops")
      .configure(
        Common.jvmSettings,
        Common.macroModuleSettings)
      .dependsOn(baseUtilJvm)
      .deps(jaegerClient ++ Prometheus.client)

  lazy val baseDb =
    project("base-db")
      .configure(Common.jvmSettings)
      .dependsOn(baseOps)
      .deps(postgresql ++ Doobie.main ++ hikariCP ++ flyway ++ Circe.main)

  lazy val baseTestJvm = baseTest.jvm
  lazy val baseTestJs  = baseTest.js
  lazy val baseTest =
    crossProject("base-test")
      .configureBoth(Common.testModuleSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(UseNode))
      .dependsOn(baseUtil)
      .configureJvm(_.dependsOn(baseDb % Provided))
      .depsForBoth(
        Microlibs.testUtil ++ pprint ++
        providedScope(μTest ++ Nyaya.gen ++ Circe.main) ++
        testScope(μTest ++ Nyaya.test ++ Circe.main))
      .depsForJvm(providedScope(scalaCheck))

  // ===================================================================================================================
  // utils & benchmark-*

  lazy val utils =
    project("utils")
      .configure(Common.jvmSettings)
      .deps(commonsText ++ Nyaya.test)
      .dependsOn(webappBaseTestJvm)
      .settings(
        connectInput in run  := true,
        fork         in run  := true,
      //javaOptions  in run  += jprofilerAgent(wait = false),
        javaOptions  in run ++= Seq("-Xmx8g", "-Xss8m"))

  object Benchmark {
    def commonSettings: Project => Project =
      _.configure(Common.settingsMin)

    def jvmSettings: Project => Project = {
      import pl.project13.scala.sbt.SbtJmh
      _.enablePlugins(SbtJmh)
        .dependsOn(webappServer)
        .configure(Common.jvmSettings)
        .settings(libraryDependencies ++= Seq(
          "dev.zio" %% "zio" % "1.0.0"))
        .deps(JJWT.all)
    }

    def jsSettings: Project => Project = {
      import org.scalajs.sbtplugin._

      _.enablePlugins(ScalaJSPlugin, JSDependenciesPlugin)
        .dependsOn(webappClientProject, webappClientWw)
        .depsForJs(scalajsBenchmark)
        .configure(
          Common.jsSettings(NoTests))
        .settings(
          scalaJSLinkerConfig ~= { _.withSourceMap(true) },
          skip in packageJSDependencies := false)
    }
  }

  lazy val benchmarkJvm = benchmark.jvm
  lazy val benchmarkJs  = benchmark.js
  lazy val benchmark =
    crossProject("benchmark")
      .configureBoth(Benchmark.commonSettings)
      .dependsOn(webappBaseTest, webappSampleData)
      .configureJvm(Benchmark.jvmSettings)
      .configureJs(Benchmark.jsSettings)
}
