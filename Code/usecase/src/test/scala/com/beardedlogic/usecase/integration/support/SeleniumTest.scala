package com.beardedlogic.usecase.integration.support

import org.scalatest.{Suite, GivenWhenThen}
import org.scalatest.matchers.ShouldMatchers
import com.beardedlogic.usecase.test.TestHelpers

trait SeleniumTest extends SeleniumTestSupport with ShouldMatchers with TestHelpers with GivenWhenThen {
  this: Suite =>

  def currentUrl = selenium.getCurrentUrl.replaceFirst("^http://[^/]+", "")

  def pageSource = selenium.getPageSource

  def keyboard = selenium.getKeyboard

  def goto = new DslIndex(selenium)
}
