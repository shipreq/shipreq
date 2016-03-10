import sbt._
import sbt.Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.scalajs.jsenv.selenium._

object IntegrationTesting {

  def defaultBrowser: SeleniumBrowser =
    Firefox

  def testWithBrowser(browser: SeleniumBrowser = defaultBrowser): Project => Project =
    _.settings(
      jsEnv in Test := new SeleniumJSEnv(browser))

  def debugWithBrowser(browser: SeleniumBrowser = defaultBrowser): Project => Project =
    _.settings(
      jsEnv             in Test := new SeleniumJSEnv(browser).withKeepAlive(),
      parallelExecution in Test := false,
      emitSourceMaps            := true)
}