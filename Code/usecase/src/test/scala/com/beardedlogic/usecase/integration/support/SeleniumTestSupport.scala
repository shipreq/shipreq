package com.beardedlogic.usecase
package integration.support

import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.{HasInputDevices, JavascriptExecutor, WebDriver}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Suite}
import test.{TestDatabaseSupport, SharedGlobal, Jetty}
import SeleniumTestSupport._

/**
 * @since 30/04/2013
 */
object SeleniumTestSupport {
  type SeleniumDriver = WebDriver with JavascriptExecutor with HasInputDevices

  val SeleniumJetty = new Jetty(8091)

  implicit class SeleniumDriverExt(d: SeleniumDriver) {
    def disableJqueryEffects() = { d.executeScript("jQuery.fx.off = true"); d }
    def getRel(page: String) = { d.get(SeleniumJetty.url + page); d }
    def ensureNoTestFuncIds() = { if (d.getPageSource.contains("lift_ajaxHandler(&quot;f0000000")) throw new IllegalStateException("FFS. Test func IDs are on again :("); d }
  }

  val SeleniumDriverRef = SharedGlobal(None, newDriver _) { _.close }

  def newDriver : SeleniumDriver = new FirefoxDriver
}

/**
 * Brings up Jetty and provides a managed Selenium helper.
 *
 * @since 30/04/2013
 */
trait SeleniumTestSupport extends BeforeAndAfterAll { this: Suite =>

  override def beforeAll() {
    TestDatabaseSupport.init()
    SeleniumJetty.acquire
    _selenium = SeleniumDriverRef.acquire
  }

  override def afterAll() {
    _selenium = SeleniumDriverRef.release
    SeleniumJetty.release
  }

  private var _selenium : SeleniumDriver = null
  def selenium = _selenium

  def baseUrl = SeleniumJetty.url
}