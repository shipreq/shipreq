import sbt._
import Keys._
import Common.Functions._
import Common.Values.releaseMode
import Dependencies._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.cross.{CrossProject, CrossType}
import ScalaJSPlugin.autoImport.{crossProject => _, _}
import DependencyLib.JVM
// import org.scalajs.core.tools.sem._

object ShipReqExperiments extends Build {

  def project(dir: String) =
    Project(dir, file(dir))

  def crossProject(dir: String) =
    CrossProject(dir + "-jvm", dir + "-js", file(dir), CrossType.Full).settings(name := dir)

  lazy val root =
    Project("root", file("."))
      .configure(Common.settings, IdeSettings.settingsForRoot)
      .aggregate(sjs)

  // ===================================================================================================================

  lazy val sjs =
    project("sjs")
      .enablePlugins(ScalaJSPlugin)
      .depsForJs(Scalaz.effect ++ React.most ++ Monocle.macros)
      .configure(
        Common.settings,
        Common.jsSettings(NeedDom),
        useMacroParadise)
      .settings(
        emitSourceMaps in Compile := false, // I want speed
        scalaJSOptimizerOptions in fastOptJS ~= (_ withDisableOptimizer true))

}
