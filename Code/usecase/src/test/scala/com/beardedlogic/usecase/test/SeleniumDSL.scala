package com.beardedlogic.usecase.test

import com.beardedlogic.usecase.snippet.UCEditor
import org.openqa.selenium.{ By, Keys, WebElement }
import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.JavaConversions._
import com.beardedlogic.usecase.lib.field.CourseAndExceptionFields

/**
 * Provides tests with Selenium-based DSLs.
 *
 * @since 30/04/2013
 */
trait SeleniumDSL extends SeleniumTestSupport { this: Suite =>
  import SeleniumDSL._

  def uce = new UCEditorDSL(s).tap { _.reload }
}

/**
 * @since 30/04/2013
 */
object SeleniumDSL {
  import SeleniumTestSupport._

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
    var expectDelays = true

    def oneOffDelay(waitTime: Long): this.type = { Thread.sleep(waitTime); expectDelays = false; this }
    def expectDelays(v: Boolean): this.type = { expectDelays = v; this }
    def eventually(cond: => Any): this.type = { TestHelpers.eventuallyIf(expectDelays)(cond); this }
    def eventually(cond: (this.type) => Any): this.type = { TestHelpers.eventuallyIf(expectDelays) { cond(this) }; this }
  }

  type Finder = { def findElements(by: By): java.util.List[WebElement]; def findElement(by: By): WebElement }

  /**
   * DSL for the Use Case Editor.
   *
   * @since 30/04/2013
   */
  class UCEditorDSL(val s: SeleniumDriver, private val givenCourseRoot: Option[Finder] = None) extends BaseDSL {

    val courseRoot: Finder = givenCourseRoot getOrElse s
    private def changeRoot(rootId: String) =
      new UCEditorDSL(s, Some(s.findElement(By.id(rootId)))).expectDelays(this.expectDelays)
    def nc = changeRoot(CourseAndExceptionFields.NormalCourseId)
    def ac = changeRoot(CourseAndExceptionFields.AlternateCourseId)
    def ec = changeRoot(CourseAndExceptionFields.ExceptionCourseId)
    def root = new UCEditorDSL(s, None).expectDelays(this.expectDelays)

    // Internal --------------------------------------------------------------------------------------------------------

    private def titleElem = s.findElement(By.name("title"))
    private def steps = courseRoot.findElements(By.cssSelector(".step"))
    private def addButtons = courseRoot.findElements(By.cssSelector("button.add"))
    private def stepTextElem(row: Int) = steps(row).findElement(By.cssSelector("textarea"))
    private def addButton(row: Int) = steps(row).findElement(By.cssSelector("button.add"))
    private def delButton(row: Int) = steps(row).findElement(By.cssSelector("button.delete"))
    private def indentDecButton(row: Int) = steps(row).findElement(By.cssSelector("button.indentDec"))
    private def indentIncButton(row: Int) = steps(row).findElement(By.cssSelector("button.indentInc"))

    // Action ----------------------------------------------------------------------------------------------------------

    def reload = { s.getRel("uce").disableJqueryEffects; this }
    def setUseCaseTitle(title: String) = { titleElem.typeInto(title); steps(0).click; this }
    def setStepText(row: Int, text: String) = { stepTextElem(row).typeInto(text); titleElem.click; this }
    def setStepText(args: Tuple2[Int, String]*) = { for ((row, text) <- args) stepTextElem(row).typeInto(text); titleElem.click; this }
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
    def assertButtons(row: Int, indent: Tuple2[Boolean, Boolean], del: Boolean = true) = {
      indentDecButtonVisibility(row) should be(indent._1)
      indentIncButtonVisibility(row) should be(indent._2)
      deleteButtonVisibility(row) should be(del)
      this
    }

    // Inspection ------------------------------------------------------------------------------------------------------

    def useCaseId = s.findElement(By.id("uc-id")).getText
    def useCaseTitle = titleElem.value
    def stepCount = steps.size
    def stepText(row: Int) = stepTextElem(row).value
    def stepLabel(row: Int) = steps(row).findElement(By.cssSelector(".label span")).getText
    def stepLevel(row: Int) = {
      val lvl = steps(row).getAttribute(CourseAndExceptionFields.AttrLevel)
      lvl should fullyMatch regex ("^\\d+$")
      lvl.toInt
    }
    def addButtonCount = addButtons.size
    def deleteButtonVisibility(row: Int) = delButton(row).isDisplayed
    def indentDecButtonVisibility(row: Int) = indentDecButton(row).isDisplayed
    def indentIncButtonVisibility(row: Int) = indentIncButton(row).isDisplayed
  }
}

