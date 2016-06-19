import sbt.{project => _, _}
import Keys._
import org.scalajs.core.tools.io.{IO => _, _}
import org.scalajs.sbtplugin.{ScalaJSPlugin, ScalaJSPluginInternal, Stage}
import Common.Functions._
import Common.Values.{devMode, releaseMode}
import Dependencies._
import DependencyLib.JVM
import ScalaJSPlugin.autoImport.{crossProject => _, _}
import ScalaJSPluginInternal.stageKeys
import ShipReqBuild._

/** The user-facing app.
  */
object WebappBuild {

  // TODO This is obsolete
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
        webappMacroJvm, webappBaseJvm, webappBaseServerJvm, webappBaseTestJvm, webappGenJvm,
        webappMacroJs , webappBaseJs , webappBaseServerJs , webappBaseTestJs , webappGenJs ,
        webappClient, webappServer)
      .settings(
        jsSizesFast := jsSizesTask(Stage.FastOpt).value,
        jsSizesFull := jsSizesTask(Stage.FullOpt).value,
        addCommandAlias("jsSizes", ";jsSizesFast;jsSizesFull"))

  lazy val webappClient =
    project("webapp-client")
      .configure(webappSettings)
      .aggregate(
        webappClientBase, webappClientBaseTest,
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
        Scalaz.effect ++ React.most ++ Monocle.macros ++ ScalaCSS.react ++ scalajsDom ++
        μPickle ++ boopickle)
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings,
        useMacroParadise,
        // Common.jsFastDevSettings,
        dontInline) // probably crashes, try with Scala 2.12

  lazy val webappClientBaseTest =
    project("webapp-client-base-test")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(webappClientBase, webappBaseServerJs, webappBaseTestJs)
      .depsForJs(
        TestState.nyaya ++ TestState.domZipperSizzle ++ TestState.scalajsReact ++
        React.test ++ μTest ++ Nyaya.test)
      .configure(
        Common.jsSettings(NeedDom),
        webappSettings,
        useMacroParadise,
        // Common.jsFastDevSettings,
        dontInline)

  lazy val webappClientHome =
    project("webapp-client-home")
      .enablePlugins(ScalaJSPlugin)
      .dependsOn(webappClientBase, webappClientBaseTest % "test->compile")
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
      .dependsOn(webappClientWwApi, webappClientBaseTest % "test->compile")
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
      .dependsOn(webappClientBase, webappClientWwApi, webappClientBaseTest % "test->compile")
      .depsForJs(
        Scalaz.effect ++ React.most ++ Monocle.macros ++ ScalaCSS.react ++ scalajsDom ++
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

  lazy val webappGenJvm = webappGen.jvm
  lazy val webappGenJs  = webappGen.js
  lazy val webappGen =
    crossProject("webapp-gen")
      .configureBoth(Common.settings)
      .configureJvm(Common.jvmSettings, _.dependsOn(webappBaseJvm)).depsForJvm(Lift.webkit)
      .configureJs(
        Common.jsSettings(NeedDom),
        _.dependsOn(webappClientProject)
          .settings(
            // Ensure that the production asset paths are used, and there is no dev-only markup in generated html
            // scalacOptions ++= Seq("-Xelide-below", "OFF"),
            // scalaJSStage := FullOptStage,
            // ↑ Doesn't work, needs to be applied to deps too. Will hack dev. Release-mode test will prove validity.
            jsDependencies += ProvidedJS / "shipreq-gen-deps.js"))
      .depsForBoth(testScope(μTest))
      .dependsOn(webappBaseTest % "test->compile")

  lazy val webappServer =
    project("webapp-server").configure(WebappServerBuild.apply)

  // ===================================================================================================================

  val jsSizesFast = TaskKey[Unit]("jsSizesFast", "Print JS sizes (using fastOptJS).")
  val jsSizesFull = TaskKey[Unit]("jsSizesFull", "Print JS sizes (using fullOptJS).")

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

  def jsSizesTask(stage: Stage) = Def.task[Unit] {
    def report(name: String, f: Attributed[File]): Unit =
      printf("%,11d → %,9d - %s\n", f.data.length, gzipLength(f.data), name)
    val header = s"JS $stage Sizes"
    println()
    println(header)
    println("=" * header.length)
    report((moduleName in webappClientHome   ).value, (stageKeys(stage) in Compile in webappClientHome   ).value)
    report((moduleName in webappClientProject).value, (stageKeys(stage) in Compile in webappClientProject).value)
    report((moduleName in webappClientWw     ).value, (stageKeys(stage) in Compile in webappClientWw     ).value)
    println()
  }

}
