package com.beardedlogic.usecase.test

import com.beardedlogic.usecase.snippet.UCEditor
import org.openqa.selenium.{ By, Keys, WebElement }
import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.JavaConversions._

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

    private def titleElem = s.findElementByName("title")
    private def steps = s.findElementsByCssSelector(".step")
    private def stepTextElem(row: Int) = steps(row).findElement(By.cssSelector("textarea"))
    private def addButtons = s.findElementsByCssSelector("button.add")
    private def addButton(row: Int) = steps(row).findElement(By.cssSelector("button.add"))
    private def delButton(row: Int) = steps(row).findElement(By.cssSelector("button.delete"))
    private def indentDecButton(row: Int) = steps(row).findElement(By.cssSelector("button.indentDec"))
    private def indentIncButton(row: Int) = steps(row).findElement(By.cssSelector("button.indentInc"))

    // Action ----------------------------------------------------------------------------------------------------------

    def reload = { s.get(Jetty.URL); this }
    def setUseCaseTitle(title: String) = { titleElem.typeInto(title); steps(0).click; this }
    def setStepText(row: Int, text: String) = { stepTextElem(row).typeInto(text); titleElem.click; this }
    def setStepText(args : Tuple2[Int, String]*) = { for ((row,text) <- args) stepTextElem(row).typeInto(text); titleElem.click; this }
    def assertStepText(row: Int, txt: String) = eventually { stepText(row) should equal(txt) }
    def assertStepCount(expected: Int) = eventually { stepCount should equal(expected) }
    def assertAddButtonCount(expected: Int) = eventually { addButtonCount should equal(expected) }
    def clickAdd(row: Int) = { addButton(row).click(); this }
    def clickDelete(row: Int) = { delButton(row).click(); this }
    def assertStep(row: Int)(lvl: Int, label: String, txt: String = "") = eventually {
      stepLevel(row) should equal(lvl)
      stepLabel(row) should equal(label)
      stepText(row) should equal(txt)
    }
    def clickIndentDec(row: Int) = { indentDecButton(row).click(); this }
    def clickIndentInc(row: Int) = { indentIncButton(row).click(); this }

    // Inspection ------------------------------------------------------------------------------------------------------

    def useCaseId = s.findElementById("uc_id").getText
    def useCaseTitle = titleElem.value
    def stepCount = steps.size
    def stepText(row: Int) = stepTextElem(row).value
    def stepLabel(row: Int) = steps(row).findElement(By.cssSelector(".label")).getText
    def stepLevel(row: Int) = {
      val lvl = steps(row).getAttribute(UCEditor.AttrLevel)
      lvl should fullyMatch regex ("^\\d+$")
      lvl.toInt
    }
    def addButtonCount = addButtons.size
    def deleteButtonVisibility(row: Int) = delButton(row).isDisplayed
    def indentDecButtonVisibility(row: Int) = indentDecButton(row).isDisplayed
    def indentIncButtonVisibility(row: Int) = indentIncButton(row).isDisplayed
  }
}

