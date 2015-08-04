import sbt._
import Keys._
import Common.Functions._
import Common.Values.releaseMode
import Dependencies._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.cross.{CrossProject, CrossType}
import ScalaJSPlugin.autoImport.{crossProject => _, _}

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
      .aggregate(baseUtilJvm, baseUtilJs, baseDb, baseTest)

  lazy val baseUtilJvm = baseUtil.jvm
  lazy val baseUtilJs  = baseUtil.js
  lazy val baseUtil =
    crossProject("base-util")
      .configureBoth(Common.settings)
      .configureJs(Common.jsSettings)
      .depsForBoth(
        Scalaz.effect ++ Nyaya.core ++ testScope(μTest)
      )
      .depsForJvm(
        SLF4J.api ++ Scalaz.effect ++
        providedScope(logback ++ jodaTime) ++
        testScope(Specs2.combo ++ Scalaz.scalacheck)
      )
      .configureJvm(
        dontInline // crashes scalac 2.11.2
      )

  lazy val baseDb =
    project("base-db")
      .configure(Common.settings)
      .deps(
        postgresql ++ slick ++ hikariCP ++ flyway ++ logback ++
        providedScope(jodaTime)
      )
      .dependsOn(baseUtilJvm)

  lazy val baseTest =
    project("base-test")
      .configure(Common.settings)
      .deps(
        providedScope(scalaTest ++ Specs2.combo)
      )
      .dependsOn(baseUtilJvm)
      .dependsOn(baseDb % "provided")
      // TODO Delete after upgrade to 2.11 and switch from Manifest to TypeTag
      .settings(scalacOptions in Compile ~= removeValues("-deprecation"))
      .settings(scalacOptions in Compile += "-nowarn")

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
        testScope(Specs2.combo ++ scalaCheck ++ Scala.reflect)
      )
      .dependsOn(taskmanApiLogic, baseDb)
      .dependsOn(taskmanServerSchema % "test")
      .dependsOn(baseTest % "test")
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
        jodaTime ++ logback ++ testScope(Specs2.combo)
      )
      .dependsOn(taskmanApiLogic)
      .dependsOn(baseTest % "test")
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
      .dependsOn(baseTest % "test")
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
        webappMacrosJvm, webappMacrosJs, webappBaseJvm, webappBaseJs,
        // webappBaseTestJvm, webappBaseTestJs, // Don't want this included by compile. Tests will pull it in when needed
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
      "up"  -> s";$WS/container:stop ;clear ;$WS/container:start",         // webapp: UP
      "d"   -> s"$WS/container:stop",                                      // webapp: Down
      "wd"  -> ";up ;~js")                                                 // WebDev
  }

  lazy val webappMacrosJvm = webappMacros.jvm
  lazy val webappMacrosJs  = webappMacros.js
  lazy val webappMacros =
    crossProject("webapp-macros")
      .configureBoth(webappSettings)
      .configureJs(Common.jsSettings, Common.noJsTests)
      .dependsOn(baseUtil)
      .depsForBoth(
        μPickle ++ boopickle ++ Monocle.core ++
        providedScope(Scala.library) ++
        testScope(μTest)
      )
      .dependsOn(baseUtil)
      .configureBoth(
        Common.definesMacros,
        useMacroParadise
      )

  lazy val webappBaseJvm = webappBase.jvm
  lazy val webappBaseJs  = webappBase.js
  lazy val webappBase =
    crossProject("webapp-base")
      .configureBoth(webappSettings)
      .configureJs(Common.jsSettings, Common.noJsTests)
      .depsForBoth(
        μPickle ++ Monocle.macros ++ shapeless ++ Nyaya.core ++ parboiled ++ boopickle ++
        testScope(μTest) // TODO Move tests into this
      )
      .configureBoth(
        useMacroParadise,
        dontInline // crashes scalac 2.11.5
      )
      .dependsOn(baseUtil, webappMacros)

  lazy val webappBaseTestJvm = webappBaseTest.jvm
  lazy val webappBaseTestJs  = webappBaseTest.js
  lazy val webappBaseTest =
    crossProject("webapp-base-test")
      .configureBoth(webappSettings)
      .configureJs(Common.jsSettings)
      .depsForBoth(
        μTest ++ Nyaya.test
      )
      .dependsOn(webappBase)

  // -------------------------------------------------------------------------------------------------------------------
  object WebappClient {
    import org.scalajs.core.tools.sem._

    def dir = "webapp-client"

    def stage  = if (releaseMode) FullOptStage else FastOptStage
    def jsTask = if (releaseMode) fullOptJS    else fastOptJS
    def jsCmd  = if (releaseMode) "fullOptJS"  else "fastOptJS"

    def testjs(path: String) = ProvidedJS / s"testjs/$path"

    def testSettings = (_: Project)
      .settings(
        scalaJSStage in Global := stage,
        jsDependencies in Test ++= Seq(
          testjs("react-with-addons.js"),
          testjs("jquery.min.js"),
          testjs("jquery.textcomplete.js") dependsOn "testjs/jquery.min.js",
          testjs("sizzle.min.js")),
        emitSourceMaps in Test := false, // PhantomJS doesn't use
        requiresDOM := true,
        jsEnv in Test := new PhantomJS2Env(scalaJSPhantomJSClassLoader.value))

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
          μPickle ++ shapeless ++ Nyaya.core ++ parboiled ++ boopickle ++
          testScope(React.test ++ μTest ++ Nyaya.test)
        )
        .configure(
          Common.jsSettings,
          webappSettings,
          useMacroParadise,
          WebappClient.testSettings,
          dontInline, // ScalaJS inlines
          debugOrRelease(identity, WebappClient.prodJsSettings)
        )
  }

  lazy val webappClient = WebappClient.createProject

  // -------------------------------------------------------------------------------------------------------------------
  object WebappServer {
    import com.earldouglas.xsbtwebplugin.PluginKeys.{packageWar, start}
    import com.earldouglas.xsbtwebplugin.WebPlugin.{container, webSettings}

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
      { val k = Keys.`package` in Compile;        k <<= k.dependsOn(linkClientJs) },
      { val k = start in container.Configuration; k <<= k.dependsOn(linkClientJs) },
      { val k = test in Test;                     k <<= k.dependsOn(linkClientJs) },
      linkClientJs := {
        jsBuildTask.value // Ensure client JS is built
        val log = streams.value.log
        for ((s, t) <- clientJsLinks.value.links)
          ln(s, t, log)
      })

    def warSettings = (_: Project).settings(
      // Remove certain files from the WAR
      excludeFilter in packageWar ~= { (a: FileFilter) =>
        var b = a || new FileFilter { def accept(f: File) = f.getPath.containsSlice("/_scalate/") }
        if (releaseMode)
          b = b || new FileFilter { def accept(f: File) = f.getPath.containsSlice("/dev/") }
        b
      })

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
        .dependsOn(baseDb, taskmanApi, webappBaseJvm)
        //.dependsOn(baseUtilJvm, taskmanApiLogic, taskmanApiImpl) // Stupid IDEA auto-import needs this
        .deps(
          Scalaz.core ++ Lift.webkit ++ Shiro.all ++ scalate ++ commonsLang ++ guava ++
          testScope(μTest ++ scalaTest ++ scalaCheck ++ mockito ++ Lift.testkit ++ commonsIo ++ twitterEval) ++
          depScope("it")(selenium) ++
          (jetty % "container,test") ++ (servlet % "container,test,provided")
        )
        .configure(
          webappSettings,
          Common.generateBuildPropFile(),
          clientJsSettings,
          warSettings,
          testSettings,
          integrationTestSettings,
          dontInline // crashes scalac 2.11.5
        )
        .settings(webSettings: _*)
        .settings(
          initialCommands += consoleCmds,
          // Ensure templates can be loaded from the console
          fullClasspath in console in Compile += file("src/main/webapp")
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
    lazy val commonSettings: Project => Project =
      _.configure(Common.settingsMin)
        .settings(scalacOptions ++= scalacFlags)

    def scalacFlags = Seq(
      "-unchecked",
      "-deprecation",
      "-YclasspathImpl:flat", // https://github.com/scala/scala/pull/4176
      "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

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
          Common.jsSettings,
          useMacroParadise,
          WebappClient.prodJsSettings
        )
        .settings(
          // scalaJSStage in Global := FullOptStage,
          artifactPath in (Compile, fastOptJS) := ((target in Compile).value / outputJs),
          artifactPath in (Compile, fullOptJS) := ((target in Compile).value / outputJs),
          scalaJSStage in Test := Stage.PreLink,
          test in Test := ()
        )
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
