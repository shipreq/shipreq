
import sbt._, Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{crossProject => _, CrossType => _, _}
import sbtcrossproject.CrossProject
import sbtcrossproject.CrossPlugin.autoImport.{crossProject => _, _}
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
      .settings(addCommandAlias("dockers", ";root/compile ;taskman-server/docker ;webapp-server/docker"))

  /** All JS modules */
  lazy val js =
    Project("js", file(".js"))
      .configure(Common.jvmSettings)
      .aggregate(
        webappMacroJs,
        webappBaseJs,
        webappBaseMemberJs,
        webappBaseTestJs,
        webappClientPublicJs,
        webappClientHome,
        webappClientWwApi,
        webappClientWw,
        webappClientProject,
        webappSsrJs,
        webappServerLogicJs)

  // ===================================================================================================================
  // base-* : General utils for taskman, webapp, benchmarking, etc.

  lazy val base =
    project("base")
      .configure(Common.jvmSettings)
      .aggregate(baseUtilJvm, baseUtilJs, baseOps, baseDb, baseTestJvm, baseTestJs)

  lazy val baseUtilJvm = baseUtil.jvm
  lazy val baseUtilJs  = baseUtil.js
  lazy val baseUtil =
    crossProject("base-util")
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(UseNode))
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
        Microlibs.testUtil ++
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
          "dev.zio" %% "zio" % "1.0.0-RC18-2"))
        .deps(JJWT.all)
    }

    def jsSettings: Project => Project = {
      import org.scalajs.sbtplugin._

      _.enablePlugins(ScalaJSPlugin)
        .dependsOn(webappClientProject)
        .depsForJs(scalajsBenchmark)
        .configure(
          Common.jsSettings(NoTests))
        .settings(
          skip in packageJSDependencies := false,
          emitSourceMaps := true)
    }
  }

  lazy val benchmarkJvm = benchmark.jvm
  lazy val benchmarkJs  = benchmark.js
  lazy val benchmark =
    crossProject("benchmark")
      .configureBoth(Benchmark.commonSettings)
      .dependsOn(webappBase)
      .dependsOn(webappBaseTest) // TODO Shouldn't be generating random data, should be using fixed sample
      .configureJvm(Benchmark.jvmSettings)
      .configureJs(Benchmark.jsSettings)
}
