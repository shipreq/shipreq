import sbt._
import Keys._
import Common.Functions._
import Common.Values.releaseMode
import Dependencies._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.cross.{CrossProject, CrossType}
import ScalaJSPlugin.autoImport.{crossProject => _, _}
import DependencyLib.JVM

object ShipReq extends Build {

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
      .configureJs(Common.jsSettings(NoTests))
      .depsForBoth(Scalaz.core ++ Nyaya.util)

  lazy val baseUtilJvm = baseUtil.jvm
  lazy val baseUtilJs  = baseUtil.js
  lazy val baseUtil =
    crossProject("base-util")
      .configureBoth(Common.settings)
      .configureJs(Common.jsSettings(NoDom))
      .dependsOn(baseMacro)
      .depsForBoth(
        Scalaz.effect ++ Nyaya.prop ++
        testScope(μTest ++ Nyaya.test))
      .depsForJvm(
        SLF4J.api ++ Scalaz.effect ++
        providedScope(logback ++ jodaTime) ++
        testScope(Specs2.combo ++ Scalaz.scalacheck))

  lazy val baseDb =
    project("base-db")
      .configure(Common.settings)
      .deps(
        postgresql ++ slick ++ hikariCP ++ flyway ++ logback ++
        providedScope(jodaTime))
      .dependsOn(baseUtilJvm)

  lazy val baseTestJvm = baseTest.jvm
  lazy val baseTestJs  = baseTest.js
  lazy val baseTest =
    crossProject("base-test")
      .configureBoth(Common.testModuleSettings)
      .dependsOn(baseUtil)
      .depsForBoth(providedScope(Nyaya.gen))
      .configureBoth(_
        // TODO Delete after upgrade to 2.11 and switch from Manifest to TypeTag
        .settings(scalacOptions in Compile ~= removeValues("-deprecation"))
        .settings(scalacOptions in Compile += "-nowarn"))
      .configureJvm(_
        .deps(providedScope(scalaTest ++ Specs2.combo))
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
      .configure(Common.settings)
      .deps(
        Scalaz.core ++ Scalaz.effect ++
        testScope(Specs2.combo ++ scalaCheck ++ Scala.reflect)
      )
      .dependsOn(baseUtilJvm)

  lazy val taskmanApiImpl =
    project("taskman-api-impl")
      .configure(Common.settings)
      .deps(
        Json4s.jackson ++
        testScope(Specs2.combo ++ scalaCheck ++ Scala.reflect))
      .dependsOn(taskmanApiLogic, baseDb)
      .dependsOn(taskmanServerSchema % "test")
      .dependsOn(baseTestJvm % "test")
      //.dependsOn(baseUtilJvm) // Stupid IDEA auto-import needs this

  lazy val taskmanApi =
    project("taskman-api")
      .configure(Common.settings)
      .aggregate(taskmanApiLogic, taskmanApiImpl)
      .dependsOn(taskmanApiLogic, taskmanApiImpl)

  lazy val taskmanServerLogic =
    project("taskman-server-logic")
      .configure(Common.settings)
      .deps(
        jodaTime ++ logback ++ testScope(Specs2.combo))
      .dependsOn(taskmanApiLogic)
      .dependsOn(baseTestJvm % "test")
      .configure(dontInline) // crashes scalac 2.11.2
      //.dependsOn(baseUtilJvm) // Stupid IDEA auto-import needs this

  lazy val taskmanServerSchema =
    project("taskman-server-schema")
      .configure(Common.settings)
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
      .configure(Common.settings)
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
      .configure(Common.settings)
      .aggregate(taskmanServerLogic, taskmanServerImpl, taskmanServerSchema)
      .dependsOn(taskmanServerLogic, taskmanServerImpl, taskmanServerSchema)

  // ===================================================================================================================
  // webapp-* : The user-facing app.

  lazy val webapp =
    project("webapp")
      .configure(webappSettings)
      .aggregate(
        webappMacroJvm, webappMacroJs, webappBaseJvm, webappBaseJs, webappBaseTestJvm, webappBaseTestJs,
        webappClient, webappServer)

  lazy val webappSettings =
    Common.settings.andThen(_.configure(webappCmdAliases))

  lazy val webappCmdAliases = {
    def WB = "webapp-base"
    def WT = "webapp-base-test"
    def WC = "webapp-client"
    def WS = "webapp-server"
    addCommandAliases(
      "ctbc"-> ";clean ;clear ;tbc",                                       // Clean Test Base & Client
      "tbc" -> s";$WT/test:compile ;$WC/test:compile ;$WT/test ;$WC/test", // Test Base & Client
      "js"  -> s";$WC/${WebappClient.jsCmd} ;$WS/linkClientJs",            // compile JavaScript
      "jsp" -> s";$WC/${WebappClient.jsCmd} ;$WS/webappPrepare",           // compile JavaScript, auto deploy
      "up"  -> s";$WS/jetty:stop ;clear ;$WS/jetty:start",                 // webapp: UP
      "d"   -> s"$WS/jetty:stop",                                          // webapp: Down
      "wd"  -> ";up ;~js")                                                 // WebDev
  }

  lazy val webappMacroJvm = webappMacro.jvm
  lazy val webappMacroJs  = webappMacro.js
  lazy val webappMacro =
    crossProject("webapp-macro")
      .configureBoth(
        Common.macroModuleSettings,
        useMacroParadise,
        webappCmdAliases)
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
      .configureJs(Common.jsSettings(NoTests))
      .depsForBoth(
        μPickle ++ Monocle.macros ++ shapeless ++ Nyaya.prop ++ parboiled ++ boopickle ++
        testScope(μTest) // TODO Move tests into this
      )
      .configureBoth(
        useMacroParadise,
        dontInline // crashes scalac 2.11.7
      )
      .dependsOn(baseUtil, webappMacro)

  lazy val webappBaseTestJvm = webappBaseTest.jvm
  lazy val webappBaseTestJs  = webappBaseTest.js
  lazy val webappBaseTest =
    crossProject("webapp-base-test")
      .configureBoth(Common.testModuleSettings, webappCmdAliases)
      .configureJs(Common.jsSettings(NoDom))
      .depsForBoth(
        μTest ++ Nyaya.test
      )
      .dependsOn(baseTest, webappBase)

  // -------------------------------------------------------------------------------------------------------------------
  object WebappClient {
    import org.scalajs.core.tools.sem._

    def dir = "webapp-client"

    def jsTask = if (releaseMode) fullOptJS   else fastOptJS
    def jsCmd  = if (releaseMode) "fullOptJS" else "fastOptJS"

    def testjs(path: String) = ProvidedJS / s"testjs/$path"

    def testSettings = (_: Project)
      .settings(
        jsDependencies in Test ++= Seq(
          testjs("react-with-addons.js"),
          testjs("react-dom.js"),
          testjs("jquery.min.js"),
          testjs("jquery.textcomplete.js") dependsOn "testjs/jquery.min.js",
          testjs("sizzle.min.js")),
        // emitSourceMaps in Compile := false, // I want speed
        scalaJSOptimizerOptions in fastOptJS ~= { _.withDisableOptimizer(true) })

    def prodJsSettings = (_: Project).settings(
      emitSourceMaps := false,
      scalaJSOptimizerOptions ~= (_
        //.withPrettyPrintFullOptJS(true)
        .withBatchMode(true)
        .withCheckScalaJSIR(true)
        ),
      scalaJSSemantics ~= (_
        .withRuntimeClassName(_ => "")
        .withAsInstanceOfs(CheckedBehavior.Unchecked)
        ))

    def createProject =
      project(WebappClient.dir)
        .enablePlugins(ScalaJSPlugin)
        .dependsOn(baseUtilJs, webappBaseJs, webappBaseTestJs % "test->compile")
        .depsForJs(
          Scalaz.effect ++ React.most ++ Monocle.macros ++ ScalaCSS.react ++
          μPickle ++ shapeless ++ Nyaya.prop ++ parboiled ++ boopickle ++
          testScope(React.test ++ μTest ++ Nyaya.test)
        )
        .configure(
          Common.jsSettings(NeedDom),
          webappSettings,
          useMacroParadise,
          WebappClient.testSettings,
          dontInline, // crashes 2.11.7 / 0.6.4
          debugOrRelease(identity, WebappClient.prodJsSettings)
        )
  }

  lazy val webappClient = WebappClient.createProject

  // -------------------------------------------------------------------------------------------------------------------
  object WebappServer {
    import com.earldouglas.xwp._
    import ContainerPlugin.autoImport._
    import JettyPlugin    .autoImport._
    import WebappPlugin   .autoImport._

    val linkClientJs = taskKey[Unit]("Creates symlinks to webapp client resources.")
    val clientJsLinks = settingKey[ClientJsLinks]("Map of symlinks between client and server.")

    class ClientJsLinks(sRoot: File, tRoot: File) {
      private val s = sRoot / "scala-2.11"
      private val t = tRoot / "src/main/webapp/assets"
      private def sPrefix = WebappClient.dir + "-"
      private val devMap = {
        val o = t/"dev"
        val js = s"${sPrefix}fastopt.js"
        val map = js + ".map"
        Map(s/js -> o/js, s/map -> o/map)
      }
      private val releaseMap =
        Map(s/s"${sPrefix}opt.js" -> t/"C.js")
      def links =
        if (releaseMode) releaseMap else devMap
      def cleanable =
        (devMap.values ++ releaseMap.values).map(_.asFile).toSet[File]
    }

    lazy val jsBuildTask =
      WebappClient.jsTask in Compile in webappClient

    def clientJsSettings = (_: Project).settings(
      clientJsLinks := new ClientJsLinks((target in webappClient).value, baseDirectory.value),
      cleanFiles ++= clientJsLinks.value.cleanable.toSeq,
      { val k = Keys.`package`; k <<= k.dependsOn(linkClientJs) },
      { val k = webappPrepare ; k <<= k.dependsOn(linkClientJs) },
      // { val k = start in Jetty; k <<= k.dependsOn(linkClientJs) },
      // { val k = test in Test;   k <<= k.dependsOn(linkClientJs) },
      linkClientJs := {
        jsBuildTask.value // Ensure client JS is built
        val log = streams.value.log
        for ((s, t) <- clientJsLinks.value.links) {
          log.info(s"Copying $s → $t")
          IO.copyFile(s, t)
        }
      })

    def warSettings = {
      var dirHitList = Set("_scalate")
      if (releaseMode)
        dirHitList += "dev"

      (_: Project).settings(

        // Expand this the webapp-server module instead of building a jar
        // At the minimum, scripts in Release/webapp expect to find WEB-INF/classes/build.properties
        webappWebInfClasses := true,

        // Remove dirs from the WAR
        webappPostProcess := { webappDir =>
          def go(f: File): Unit = {
            if (f.isDirectory) {
              if (dirHitList contains f.getName) {
                streams.value.log.info(s"Deleting ${f.getAbsolutePath}")
                IO.delete(f)
              } else
                f.listFiles foreach go
            }
          }
          go(webappDir)
        })
    }

    def testSettings = (_: Project)
      .dependsOn(webappBaseTestJvm % "test->compile")
      .settings(inConfig(Test)(Seq(
        fork                         := true,
        javaOptions                  += "-Drun.mode=test",
        unmanagedResourceDirectories += baseDirectory.value / "src/main/webapp", // So templates load
        parallelExecution            := false)
      ): _*)

    lazy val IntegrationTest = config("it") extend Test
    def integrationTestSettings = (_: Project)
      .configs(IntegrationTest)
      .settings(inConfig(IntegrationTest)(Defaults.testSettings): _*)
      .settings(parallelExecution in IntegrationTest := false)

    def consoleCmds = """
        import scalaz._, shipreq.base.util._, shipreq.webapp._, db._, lib.Types._, feature.uc, uc._, uc.field._, uc.step._, uc.text._, FreeTextTerms._, util._
        def initlift() = {val b = new bootstrap.liftweb.Boot; b.configureLift; b}
                      """

    def createProject =
      project("webapp-server")
        .enablePlugins(JettyPlugin, WarPlugin)
        .dependsOn(baseDb, taskmanApi, webappBaseJvm)
        .deps(
          Scalaz.core ++ Lift.webkit ++ Shiro.all ++ scalate ++ commonsLang ++ guava ++
          testScope(μTest ++ scalaTest ++ scalaCheck ++ mockito ++ Lift.testkit ++ commonsIo ++ twitterEval) ++
          depScope("it")(selenium) ++
          (LibJetty.webapp % "test") ++ (servlet % "test,provided")
        )
        .configure(
          webappSettings,
          Common.generateBuildPropFile(),
          clientJsSettings,
          warSettings,
          testSettings,
          integrationTestSettings,
          dontInline // crashes scalac 2.11.7
        )
        .settings(
          addCommandAlias("livejs", "~;clear;jsp"),
          containerLibs in Jetty := LibJetty.runner(JVM).map(_.intransitive()),
          javaOptions in Jetty += "-Xmx1g",
          initialCommands += consoleCmds,
          fullClasspath in console in Compile += file("src/main/webapp") // So templates can be loaded from console
        )
  }

  lazy val webappServer = WebappServer.createProject

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
    }

    def jsSettings: Project => Project = {
      import org.scalajs.sbtplugin._
      import ScalaJSPlugin.autoImport._

      val outputJs = "shipreq-benchmark.js"

      _.enablePlugins(ScalaJSPlugin)
        .dependsOn(webappClient)
        .configure(
          Common.jsSettings(NoTests),
          useMacroParadise,
          WebappClient.prodJsSettings
        )
        .settings(
          // scalaJSStage in Global := FullOptStage,
          artifactPath in (Compile, fastOptJS) := ((target in Compile).value / outputJs),
          artifactPath in (Compile, fullOptJS) := ((target in Compile).value / outputJs))
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
