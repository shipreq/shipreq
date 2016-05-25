import sbt.{project => _, _}
import Keys._
import org.scalajs.core.tools.io.{IO => _, _}
import org.scalajs.sbtplugin.ScalaJSPlugin
import Common.Functions._
import Common.Values.{devMode, releaseMode}
import Dependencies._
import DependencyLib.JVM
import ScalaJSPlugin.autoImport.{crossProject => _, _}
import ShipReqBuild._

/** The user-facing app.
  */
object WebappBuild {

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
    project("webapp-server").configure(WebappServerBuild.apply)

}
