package com.beardedlogic.usecase.test

import java.util.concurrent.TimeUnit
import org.openqa.selenium.{ HasInputDevices, JavascriptExecutor, WebDriver }
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.internal.{ FindsByCssSelector, FindsById, FindsByLinkText, FindsByName, FindsByTagName, FindsByXPath }
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, Suite }

/**
 * @since 30/04/2013
 */
object SeleniumTestSupport {
  type SeleniumDriver = WebDriver with JavascriptExecutor with FindsById with FindsByLinkText with FindsByXPath with FindsByName with FindsByCssSelector with FindsByTagName with HasInputDevices
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
    selenium.manage.timeouts.implicitlyWait(10, TimeUnit.SECONDS)
  }

  def releaseSelenium() {
    if (selenium != null) {
      selenium.close
      selenium = null
    }
  }

  def s = selenium
}