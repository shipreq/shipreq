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
}

/**
 * Brings up Jetty and provides a managed Selenium helper.
 *
 * @since 30/04/2013
 */
trait SeleniumTestSupport extends BeforeAndAfterAll with BeforeAndAfterEach { this: Suite =>

  import SeleniumTestSupport.SeleniumDriver

  override def beforeAll() {
    Jetty.start
    newSelenium
  }

  override def beforeEach() {
    if (newSeleniumPerTest) newSelenium
  }

  override def afterEach() {
    if (newSeleniumPerTest) releaseSelenium
  }

  override def afterAll() {
    releaseSelenium
    Jetty.stop
  }

  var newSeleniumPerTest = false
  private var selenium: SeleniumDriver = null

  def newSelenium() {
    releaseSelenium
    selenium = new FirefoxDriver
  }

  def releaseSelenium() {
    if (selenium != null) {
      selenium.close
      selenium = null
    }
  }

  def s = selenium
}