import sbt._
import Keys._
import Common.Functions._
import Dependencies._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.cross.{CrossProject, CrossType}
import ScalaJSPlugin.autoImport.{crossProject => _, _}

object ShipReq {

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
      .aggregate(baseMacroJvm, baseMacroJs, baseUtilJvm, baseUtilJs, baseDb, baseTestJvm, baseTestJs)

  lazy val baseMacroJvm = baseMacro.jvm
  lazy val baseMacroJs  = baseMacro.js
  lazy val baseMacro =
    crossProject("base-macro")
      .configureBoth(Common.macroModuleSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .depsForBoth(UnivEq.scalaz ++ Scalaz.core ++ Nyaya.util)

  lazy val baseUtilJvm = baseUtil.jvm
  lazy val baseUtilJs  = baseUtil.js
  lazy val baseUtil =
    crossProject("base-util")
      .configureBoth(Common.settings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoDom))
      .dependsOn(baseMacro)
      .depsForBoth(
        UnivEq.scalaz ++ Scalaz.effect ++ Nyaya.prop ++ Monocle.core ++
        testScope(μTest ++ Nyaya.test))
      .depsForJvm(
        SLF4J.api ++
        providedScope(logback ++ jodaTime) ++
        testScope(Specs2.combo ++ Scalaz.scalacheck))

  lazy val baseDb =
    project("base-db")
      .configure(
        Common.settings,
        Common.jvmSettings,
        Common.macroModuleSettings)
      .deps(
        postgresql ++ slick ++ hikariCP ++ flyway ++ logback ++
        providedScope(jodaTime))
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
        providedScope(Nyaya.gen) ++
        testScope(Nyaya.test))
      .configureBoth(_
        // TODO Delete after upgrade to 2.11 and switch from Manifest to TypeTag
        .settings(scalacOptions in Compile ~= removeValues("-deprecation"))
        .settings(scalacOptions in Compile += "-nowarn"))
      .configureJvm(_
        .deps(providedScope(Specs2.combo))
        .dependsOn(baseDb % "provided"))

  // ===================================================================================================================
  // taskman-* : Async task execution system.

  lazy val taskman =
    project("taskman")
      .configure(Common.settings)
      .aggregate(taskmanApi, taskmanServer)
      .dependsOn(taskmanApi, taskmanServer)

  lazy val taskmanApiLogic =
    project("taskman-api-logic")
      .configure(Common.settings, Common.jvmSettings)
      .deps(
        Scalaz.core ++ Scalaz.effect ++
        testScope(Specs2.combo ++ scalaCheck ++ Scala.reflect)
      )
      .dependsOn(baseUtilJvm)

  lazy val taskmanApiImpl =
    project("taskman-api-impl")
      .configure(Common.settings, Common.jvmSettings)
      .deps(
        Json4s.jackson ++
        testScope(Specs2.combo ++ scalaCheck ++ Scala.reflect))
      .dependsOn(taskmanApiLogic, baseDb)
      .dependsOn(taskmanServerSchema % "test")
      .dependsOn(baseTestJvm % "test")
      //.dependsOn(baseUtilJvm) // Stupid IDEA auto-import needs this

  lazy val taskmanApi =
    project("taskman-api")
      .configure(Common.settings, Common.jvmSettings)
      .aggregate(taskmanApiLogic, taskmanApiImpl)
      .dependsOn(taskmanApiLogic, taskmanApiImpl)

  lazy val taskmanServerLogic =
    project("taskman-server-logic")
      .configure(Common.settings, Common.jvmSettings)
      .deps(
        jodaTime ++ logback ++ testScope(Specs2.combo))
      .dependsOn(taskmanApiLogic)
      .dependsOn(baseTestJvm % "test")
      .configure(dontInline) // crashes scalac 2.11.2
      //.dependsOn(baseUtilJvm) // Stupid IDEA auto-import needs this

  lazy val taskmanServerSchema =
    project("taskman-server-schema")
      .configure(Common.settings, Common.jvmSettings)
      .dependsOn(baseDb)

  lazy val taskmanServerImpl = {
    import sbtassembly.Plugin._
    import AssemblyKeys._

    def consoleCmds =
      """
        |import org.json4s._
        |import org.json4s.jackson.JsonMethods._
        |import org.json4s.JsonDSL._
      """.stripMargin

    project("taskman-server-impl")
      .configure(Common.settings, Common.jvmSettings)
      .deps(
        Akka.actor ++ javaMail ++ okHttp ++ httpCore ++
        testScope(Akka.testkit ++ Specs2.combo)
      )
      .dependsOn(taskmanServerLogic, taskmanServerSchema, taskmanApi)
      .dependsOn(baseTestJvm % "test")
      .settings(assemblySettings: _*)
      .settings(
        initialCommands += consoleCmds,
        test in assembly := {}) // Disable tests during assembly
      .configure(dontInline) // because Akka docs + crashes scalac 2.11.2
  }

  lazy val taskmanServer =
    project("taskman-server")
      .configure(Common.settings, Common.jvmSettings)
      .aggregate(taskmanServerLogic, taskmanServerImpl, taskmanServerSchema)
      .dependsOn(taskmanServerLogic, taskmanServerImpl, taskmanServerSchema)

  // ===================================================================================================================
  // webapp-* : The user-facing app.

  lazy val webappSettings =
    Common.settings.andThen(_.configure(webappCmdAliases))

  lazy val webappCmdAliases = {
    def WS = "webapp-server"
    addCommandAliases(
      "js"  -> s"$WS/webappPrepare",                // compile JavaScript
      "up"  -> s";$WS/jetty:stop ;$WS/jetty:start", // webapp: UP
      "d"   -> s"$WS/jetty:stop")                   // webapp: Down
  }

  lazy val webapp =
    project("webapp")
      .configure(webappSettings)
      .aggregate(
        webappMacroJvm, webappBaseJvm, webappBaseServerJvm, webappBaseTestJvm,
        webappMacroJs , webappBaseJs , webappBaseServerJs , webappBaseTestJs ,
        webappClient, webappServer)

  lazy val webappClient =
    project("webapp-client")
      .configure(webappSettings)
      .aggregate(
        webappClientBase,
        webappClientHome,
        webappClientWwApi, webappClientWw, webappClientProject)

  lazy val webappMacroJvm = webappMacro.jvm
  lazy val webappMacroJs  = webappMacro.js
  lazy val webappMacro =
    crossProject("webapp-macro")
      .configureBoth(
        Common.macroModuleSettings,
        useMacroParadise,
        webappCmdAliases)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .dependsOn(baseUtil)
      .depsForBoth(
        μPickle ++ boopickle ++ Monocle.core ++
        providedScope(Scala.library) ++
        testScope(μTest))
      .depsForJvm(postgresql)

  lazy val webappBaseJvm = webappBase.jvm
  lazy val webappBaseJs  = webappBase.js
  lazy val webappBase =
    crossProject("webapp-base")
      .configureBoth(webappSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoTests))
      .depsForBoth(
        μPickle ++ Monocle.macros ++ shapeless ++ Nyaya.prop ++ parboiled ++ boopickle ++
        testScope(μTest)) // TODO Move tests into this
      .configureBoth(
        useMacroParadise,
        dontInline) // crashes scalac 2.11.7
      .dependsOn(baseUtil, webappMacro)

  lazy val webappBaseServerJvm = webappBaseServer.jvm
  lazy val webappBaseServerJs  = webappBaseServer.js
  lazy val webappBaseServer =
    crossProject("webapp-base-server")
      .configureBoth(webappSettings)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoDom))
      .depsForBoth(testScope(μTest ++ Nyaya.test))
      .dependsOn(webappBase)

  lazy val webappBaseTestJvm = webappBaseTest.jvm
  lazy val webappBaseTestJs  = webappBaseTest.js
  lazy val webappBaseTest =
    crossProject("webapp-base-test")
      .configureBoth(Common.testModuleSettings, webappCmdAliases)
      .configureJvm(Common.jvmSettings)
      .configureJs(Common.jsSettings(NoDom))
      .depsForBoth(μTest ++ Nyaya.test)
      .dependsOn(baseTest, webappBase, webappBaseServer)

  lazy val webappClientBase =
    project("webapp-client-base")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(baseUtilJs, webappBaseJs, webappBaseTestJs % "test->compile")
      .depsForJs(
        Scalaz.effect ++ React.most ++ Monocle.macros ++ ScalaCSS.react ++
        μPickle ++ boopickle ++
        testScope(
          TestState.nyaya ++ TestState.domZipperSizzle ++ TestState.scalajsReact ++
          React.test ++ μTest ++ Nyaya.test))
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings,
        useMacroParadise,
        // Common.jsFastDevSettings,
        dontInline) // probably crashes, try with Scala 2.12

  lazy val webappClientHome =
    project("webapp-client-home")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(webappClientBase, webappBaseTestJs % "test->compile")
      .depsForJs(
        Scalaz.effect ++ React.most ++ Monocle.macros ++ ScalaCSS.react ++
        μPickle ++ boopickle ++
        testScope(
          TestState.nyaya ++ TestState.domZipperSizzle ++ TestState.scalajsReact ++
          React.test ++ μTest ++ Nyaya.test))
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings,
        useMacroParadise,
        // Common.jsFastDevSettings,
        dontInline) // crashes 2.11.7 / 0.6.4
      .settings(
        jsDependencies in Test += ProvidedJS / "shipreq-client-test.js")

  lazy val webappClientWwApi =
    project("webapp-client-ww-api")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(webappBaseJs)
      .depsForJs(
        boopickle ++ scalajsDom ++
        testScope(μTest))
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings,
        dontInline) // probably crashes, try with Scala 2.12

  lazy val webappClientWw =
    project("webapp-client-ww")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(webappClientWwApi, webappBaseTestJs % "test->compile")
      .depsForJs(
        boopickle ++ scalajsDom ++
        testScope(μTest))
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings,
        dontInline) // probably crashes, try with Scala 2.12
    .settings(
      scalaJSOutputWrapper := ("", "Main().main();"))

  lazy val webappClientProject =
    project("webapp-client-project")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(webappClientBase, webappClientWwApi, webappBaseTestJs % "test->compile")
      .depsForJs(
        Scalaz.effect ++ React.most ++ Monocle.macros ++ ScalaCSS.react ++
        μPickle ++ boopickle ++ shapeless ++ Nyaya.prop ++ parboiled ++
        testScope(
          TestState.nyaya ++ TestState.domZipperSizzle ++ TestState.scalajsReact ++
          React.test ++ μTest ++ Nyaya.test))
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings,
        useMacroParadise,
        // Common.jsFastDevSettings,
        dontInline) // crashes 2.11.7 / 0.6.4
      .settings(
        jsDependencies in Test += ProvidedJS / "shipreq-client-test.js")

  lazy val webappServer =
    project("webapp-server").configure(WebappServer.apply)

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
