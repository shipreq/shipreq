package shipreq.webapp.integration.support

import org.scalatest.{Informing, Suite, GivenWhenThen}
import shipreq.webapp.test.TestHelpers

trait SeleniumTest extends SeleniumTestSupport with TestHelpers with GivenWhenThen {
  this: Suite with Informing =>

  def currentUrl = selenium.getCurrentUrl.replaceFirst("^http://[^/]+", "")

  def pageSource = selenium.getPageSource

  def keyboard = selenium.getKeyboard

  def goto = new DslIndex(selenium)
}
