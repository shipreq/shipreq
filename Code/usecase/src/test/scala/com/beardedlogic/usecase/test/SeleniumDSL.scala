package com.beardedlogic.usecase.test

import SeleniumTestSupport.SeleniumDriver
import org.openqa.selenium.By
import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.JavaConversions._
import org.openqa.selenium.WebElement
import org.openqa.selenium.Keys

/**
 * Provides tests with Selenium-based DSLs.
 *
 * @since 30/04/2013
 */
trait SeleniumDSL extends SeleniumTestSupport { this: Suite =>
  import SeleniumDSL._

  def uce = new UCEditorDSL(s)
}

/**
 * @since 30/04/2013
 */
object SeleniumDSL {
  import SeleniumTestSupport.SeleniumDriver

  val CTRL_A = Keys.chord(Keys.CONTROL, "a")

  implicit class ObjectExt[T](o: T) {
    def tap(block: (T) => Any) = { block(o); o }
  }

  implicit class WebElementExt[T <: WebElement](e: T) {
    def value = e.getAttribute("value")
    def typeInto(txt: String) = { e.click; e.sendKeys(CTRL_A + txt); e }
  }

  /**
   * Helper methods for all DSLs.
   *
   * @since 1/05/2013
   */
  trait BaseDSL extends ShouldMatchers {
    def waitShort(): this.type = { Thread.sleep(100); this }
    def eventually(cond: => Any): this.type = { TestHelpers.eventually(cond); this }
    def eventually(cond: (this.type) => Any): this.type = { TestHelpers.eventually { cond(this) }; this }
  }

  /**
   * DSL for the Use Case Editor.
   *
   * @since 30/04/2013
   */
  class UCEditorDSL(val s: SeleniumDriver) extends BaseDSL {

    reload

    // Internal --------------------------------------------------------------------------------------------------------

    private val lvlClassPrefix = "lvl-"
    private def titleElem = s.findElementByName("title")
    private def steps = s.findElementsByCssSelector(".step")
    private def stepTextElem(row: Int) = steps(row).findElement(By.cssSelector("textarea"))
    private def addButtons = s.findElementsByCssSelector(".add button")
    private def addButton(row: Int) = steps(row).findElement(By.cssSelector(".add button"))

    // Action ----------------------------------------------------------------------------------------------------------

    def reload = { s.get(Jetty.URL); this }
    def setUseCaseTitle(title: String) = { titleElem.typeInto(title); steps(0).click; this }
    def setStepText(row: Int, title: String) = { stepTextElem(row).typeInto(title); titleElem.click; this }
    def assertStepText(row: Int, txt: String) = eventually { stepText(row) should equal(txt) }
    def assertAddButtonCount(expected: Int) = eventually { addButtonCount should equal(expected) }
    def clickAdd(row: Int) = { addButton(row).click(); this }
    def assertStep(row: Int)(lvl: Int, label: String, txt: String) = eventually {
      stepLevel(row) should equal(lvl)
      stepLabel(row) should equal(label)
      stepText(row) should equal(txt)
    }

    // Inspection ------------------------------------------------------------------------------------------------------

    def useCaseId = s.findElementById("uc_id").getText
    def useCaseTitle = titleElem.value
    def stepCount = steps.size
    def stepText(row: Int) = stepTextElem(row).value
    def stepLabel(row: Int) = steps(row).findElement(By.cssSelector(".label")).getText
    def stepLevel(row: Int) = {
      val lvls = for (
        l <- steps(row).getAttribute("class").split("\\s+") if l.startsWith(lvlClassPrefix)
      ) yield l.replace(lvlClassPrefix, "")
      lvls should have size (1)
      val lvl = lvls(0)
      lvl should fullyMatch regex ("\\d+")
      lvl.toInt
    }
    def addButtonCount = addButtons.size
  }
}

