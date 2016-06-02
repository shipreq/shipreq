import sbt._
import Keys._
import org.scalajs.core.tools.io.{IO => _, _}
import org.scalajs.sbtplugin.ScalaJSPlugin
import Common.Functions._
import Common.Values.{devMode, releaseMode}
import Dependencies._
import DependencyLib.JVM
import ScalaJSPlugin.autoImport.{crossProject => _, _}
import ShipReqBuild._
import WebappBuild._

import com.earldouglas.xwp._
import ContainerPlugin.autoImport._
import JettyPlugin    .autoImport._
import WebappPlugin   .autoImport._

object WebappServerBuild {

  lazy val copyClientJs = taskKey[Unit]("Copies required webapp client resources.")

  def clientJsSettings: Project => Project =
    _.settings(

      cleanFiles ++= {
        val webapp = (sourceDirectory in webappPrepare).value
        def sjs(name: String, sourceMap: String): Seq[File] =
          Seq(
            webapp / s"dev/$name.js",
            webapp / s"dev/$sourceMap-fastopt.js.map",
            webapp / s"a/$name.js")

        sjs("client-home"   , "webapp-client-home") ++
        sjs("client-project", "webapp-client-project") ++
        sjs("ww"            , "webapp-client-ww")
      },

      copyClientJs := {
        implicit val log = streams.value.log
        val webapp = (sourceDirectory in webappPrepare).value

        def syncSJS(jsf: VirtualJSFile, name: String): Unit =
          jsf match {
            case f: FileVirtualJSFile =>
              if (devMode) {
                fileSync(f.file         , webapp / s"dev/$name.js"                , mandatory = true)
                fileSync(f.sourceMapFile, webapp / "dev" / f.sourceMapFile.getName, mandatory = false)
                // This exact filename is specified at end of js ↑
              } else {
                fileSync(f.file, webapp / s"a/$name.js", mandatory = true)
              }
            case other =>
              sys.error("Unsupported virtual file type: " + other)
          }

        syncSJS((scalaJSLinkedFile in Compile in webappClientHome   ).value, "client-home")
        syncSJS((scalaJSLinkedFile in Compile in webappClientProject).value, "client-project")
        syncSJS((scalaJSLinkedFile in Compile in webappClientWw     ).value, "ww")
      },

      { val k = Keys.`package`; k <<= k.dependsOn(copyClientJs) },
      { val k = webappPrepare ; k <<= k.dependsOn(copyClientJs) }
    )

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
      parallelExecution            := false) // Due to UserFixture+Oshiro and LiveTest
    ): _*)

  def consoleCmds = "def initLift() = {val b = new bootstrap.liftweb.Boot; b.configureLift; b}"

  def apply = (_: Project)
    .enablePlugins(JettyPlugin, WarPlugin)
    .dependsOn(baseDb, taskmanApi, webappBaseJvm, webappBaseServerJvm)
    .deps(
      Scalaz.core ++ Lift.webkit ++ Shiro.all ++ scalate ++ commonsLang ++
      testScope(μTest ++ scalaTest ++ scalaCheck ++ mockito ++ Lift.testkit ++ commonsIo ++ twitterEval) ++
      (LibJetty.webapp % "test") ++
      (LibJetty.servletApi % "test,provided"))
    .configure(
      webappSettings,
      Common.jvmSettings,
      Common.generateBuildPropFile(),
      clientJsSettings,
      warSettings,
      testSettings,
      dontInline) // crashes scalac 2.11.7
    .settings(
      containerLibs in Jetty := LibJetty.runner(JVM).map(_.intransitive()),
      javaOptions in Jetty += "-Xmx1g",
      initialCommands += consoleCmds,
      fullClasspath in console in Compile += file("src/main/webapp")) // So templates can be loaded from console
}
