package com.beardedlogic.usecase.test

import java.util.concurrent.TimeUnit
import org.openqa.selenium.{HasInputDevices, JavascriptExecutor, WebDriver}
import org.openqa.selenium.firefox.FirefoxDriver
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Suite}

/**
 * @since 30/04/2013
 */
object SeleniumTestSupport {
  type SeleniumDriver = WebDriver with JavascriptExecutor with HasInputDevices

  implicit class SeleniumDriverExt(d: SeleniumDriver) {
    def disableJqueryEffects() = { d.executeScript("jQuery.fx.off = true"); d }
    def getRel(page: String) = { d.get(Jetty.URL + page); d }
  }

  val SeleniumDriverRef = SharedGlobal(None, newDriver _) { _.close }

  def newDriver : SeleniumDriver = new FirefoxDriver
}

/**
 * Brings up Jetty and provides a managed Selenium helper.
 *
 * @since 30/04/2013
 */
trait SeleniumTestSupport extends BeforeAndAfterAll with BeforeAndAfterEach { this: Suite =>

  import SeleniumTestSupport._

  override def beforeAll() {
    Jetty.acquire
    _s = SeleniumDriverRef.acquire
  }

  override def afterAll() {
    _s = SeleniumDriverRef.release
    Jetty.release
  }

  private var _s : SeleniumDriver = null
  def s = _s
}