import sbt._
import sbt.Keys._
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import Common._
import Dependencies._
import TaskmanBuild._
import WebappBuild._

object ShipReqBuild {

  lazy val root =
    Project("root", file("."))
      .configure(Common.jvmSettings)
      .aggregate(base, taskman, webapp, utils, benchmarkJvm, benchmarkJs)
      .aggregate(ScalafixBuild.projects: _*)

  // ===================================================================================================================
  // base-* : General utils for taskman, webapp, benchmarking, etc.

  lazy val base =
    project
      .configure(Common.jvmSettings)
      .aggregate(basePredefJvm, basePredefJs, baseUtilJvm, baseUtilJs, baseOps, baseDb, baseTestJvm, baseTestJs)

  lazy val basePredefJvm = basePredef.jvm
  lazy val basePredefJs  = basePredef.js
  lazy val basePredef =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("base-predef"))
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .depsForBoth(Cats.core ++ Microlibs.catsExt ++ Microlibs.disjunction ++ Microlibs.multimap ++ Microlibs.nonempty ++ UnivEq.cats)
      .depsForJvm(Circe.main.widen) // We don't want circe on the frontend
      .depsForJs(scalajsDom)
      .depsForJs(scalajsJavaTime)
      .settings(
        scalacOptions ~= (_.filterNot(_.startsWith("-Yimports:")) :+ "-Yimports:scala"),
        test := (()))

  lazy val baseUtilJvm = baseUtil.jvm
  lazy val baseUtilJs  = baseUtil.js
  lazy val baseUtil =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("base-util"))
      .configureBoth(Common.macroModuleSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(UseNode))
      .dependsOn(basePredef)
      .depsForBoth(
        UnivEq.cats ++ Cats.free ++ Nyaya.prop ++ Monocle.core ++
        Microlibs.adtMacros ++ Microlibs.nonempty ++ Microlibs.recursion ++
        Microlibs.catsExt ++ Microlibs.stdlibExt ++ Microlibs.utils ++
        (Circe.main % Provided) ++
        testScope(utest ++ Nyaya.test ++ Microlibs.testUtil))
      .depsForJvm(
        SLF4J.api ++ Logback.withPlugins ++ scalaLogging ++ clearConfig ++ CatsEffect.core)

  lazy val baseOps =
    project
      .in(file("base-ops"))
      .configure(
        Common.jvmSettings,
        Common.macroModuleSettings)
      .dependsOn(baseUtilJvm)
      .deps(jaegerClient ++ Prometheus.client)

  lazy val baseDb =
    project
      .in(file("base-db"))
      .configure(Common.jvmSettings)
      .dependsOn(baseOps)
      .deps(postgresql ++ Doobie.main ++ hikariCP ++ flyway ++ Circe.main)

  lazy val baseTestJvm = baseTest.jvm
  lazy val baseTestJs  = baseTest.js
  lazy val baseTest =
    crossProject(JSPlatform, JVMPlatform)
      .in(file("base-test"))
      .configureBoth(Common.testModuleSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(UseNode))
      .dependsOn(baseUtil)
      .configureJvm(_.dependsOn(baseDb % Provided))
      .depsForBoth(
        Microlibs.testUtil ++ pprint ++
        providedScope(utest ++ Nyaya.gen ++ Circe.main) ++
        testScope(utest ++ Nyaya.test ++ Circe.main))
      .depsForJvm(providedScope(scalaCheck))
      .depsForJs(providedScope(React.core))

  // ===================================================================================================================
  // utils & benchmark-*

  lazy val utils =
    project
      .configure(Common.jvmSettings)
      .deps(commonsText ++ Nyaya.test)
      .dependsOn(webappMemberTestJVM)
      .settings(
        run / connectInput  := true,
        run / fork          := true,
      //run / javaOptions   += jprofilerAgent(wait = false),
        run / javaOptions  ++= Seq("-Xmx8g", "-Xss8m"))

  object Benchmark {
    def commonSettings: Project => Project =
      _.configure(Common.settingsMin)

    def jvmSettings: Project => Project = {
      import pl.project13.scala.sbt.SbtJmh
      _.enablePlugins(SbtJmh)
        .dependsOn(webappServer)
        .configure(Common.jvmSettings)
        .settings(libraryDependencies ++= Seq(
          "dev.zio" %% "zio" % "1.0.12"))
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
          packageJSDependencies / skip := false)
    }
  }

  lazy val benchmarkJvm = benchmark.jvm
  lazy val benchmarkJs  = benchmark.js
  lazy val benchmark =
    crossProject(JSPlatform, JVMPlatform)
      .configure(Benchmark.commonSettings)
      .dependsOn(webappMemberTest, webappSampleData)
      .configureJvm(Benchmark.jvmSettings)
      .configureJs(Benchmark.jsSettings)
}
