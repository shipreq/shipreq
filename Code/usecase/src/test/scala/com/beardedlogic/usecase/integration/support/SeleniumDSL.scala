package com.beardedlogic.usecase
package integration.support

import org.openqa.selenium.{ By, Keys, WebElement }
import org.scalatest.matchers.ShouldMatchers
import scala.collection.JavaConverters._
import SeleniumTestSupport._
import SeleniumDSL._
import test.TestHelpers

class DslIndex(s: SeleniumDriver) {
  def useCaseEditor = new UseCaseEditorDSL(s).reload
  def useCaseIndex = new UseCaseIndexDSL(s).reload
  def login = new LoginDSL(s).reload
}

/**
 * @since 30/04/2013
 */
object SeleniumDSL {

  val CTRL_A = Keys.chord(Keys.CONTROL, "a")

  implicit class WebElementExt[T <: WebElement](e: T) {
    def value = e.getAttribute("value")
    def typeInto(txt: String) = { e.click; e.sendKeys(CTRL_A + txt); e }
  }

  type Finder = { def findElements(by: By): java.util.List[WebElement]; def findElement(by: By): WebElement }
}

/**
 * Helper methods for all DSLs.
 *
 * @since 1/05/2013
 */
trait BaseDSL extends ShouldMatchers {
  var expectDelays = true

  def s: SeleniumDriver
  def url: String

  def reload(): this.type = { s.getRel(url).ensureNoTestFuncIds.disableJqueryEffects; this }

  def findVisible(css: String): List[WebElement] = s.findElements(By.cssSelector(css)).asScala.filter(_.isDisplayed).toList
  def oneOffDelay(waitTime: Long): this.type = { Thread.sleep(waitTime); expectDelays = false; this }
  def expectDelays(v: Boolean): this.type = { expectDelays = v; this }
  def eventually(cond: => Any): this.type = { TestHelpers.eventuallyIf(expectDelays)(cond); this }
  def eventually(cond: (this.type) => Any): this.type = { TestHelpers.eventuallyIf(expectDelays) { cond(this) }; this }
}

// =====================================================================================================================

/**
 * DSL for the Use Case Editor.
 *
 * @since 30/04/2013
 */
class UseCaseEditorDSL(val s: SeleniumDriver, private val givenCourseRoot: Option[Finder] = None) extends BaseDSL {
  import snippet.uce.Renderer.{AddTailStepClass, StepLevelAttribute}

  override val url = "/uce"

  val courseRoot: Finder = givenCourseRoot getOrElse s
  private def changeRoot(name: String) = new UseCaseEditorDSL(s, Some(s.findElement(By.className(name)))).expectDelays(this.expectDelays)
  def nc = changeRoot("courses-n")
  def ac = changeRoot("courses-a")
  def ec = changeRoot("courses-e")
  def root = new UseCaseEditorDSL(s, None).expectDelays(this.expectDelays)

  // Internal ----------------------------------------------------------------------------------------------------------

  private def titleElem = s.findElement(By.name("title"))
  private def steps = courseRoot.findElements(By.cssSelector(".step")).asScala
  private def addButtons = courseRoot.findElements(By.cssSelector("button.add"))
  private def addButton(row: Int) = steps(row).findElement(By.cssSelector("button.add"))
  private def delButton(row: Int) = steps(row).findElement(By.cssSelector("button.delete"))
  private def indentDecButton(row: Int) = steps(row).findElement(By.cssSelector("button.indentDec"))
  private def indentIncButton(row: Int) = steps(row).findElement(By.cssSelector("button.indentInc"))
  private def addTailStepButtons = courseRoot.findElements(By.cssSelector(s".${AddTailStepClass} button")).asScala
  private def addTailStepButton = addTailStepButtons.head

  // Action ------------------------------------------------------------------------------------------------------------

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
  def click_<<(row: Int) = { indentDecButton(row).click(); this }
  def click_>>(row: Int) = { indentIncButton(row).click(); this }
  def assertButtons(row: Int, indent: Tuple2[Boolean, Boolean], del: Boolean = true) = {
    indentDecButtonVisibility(row) should be(indent._1)
    indentIncButtonVisibility(row) should be(indent._2)
    deleteButtonVisibility(row) should be(del)
    this
  }
  def assertHasAddTailStepButton() = { addTailStepButton.isDisplayed should be(true); this }
  def assertNoAddTailStepButton() = { addTailStepButton.isDisplayed should be(false); this }
  def clickAddTailStepButton() = { addTailStepButton.click(); this }
  def addButtons(count: Int) = {
    var before = stepCount
    for (i <- 1 to count){ clickAdd(before-1) }
    assertStepCount(before + count)
  }

  // Inspection --------------------------------------------------------------------------------------------------------

  def useCaseId = s.findElement(By.id("uc-id")).getText
  def useCaseTitle = titleElem.value
  def stepCount = steps.size
  def stepText(row: Int) = stepTextElem(row).value
  def stepTextElem(row: Int) = steps(row).findElement(By.cssSelector("textarea"))
  def stepLabel(row: Int) = steps(row).findElement(By.cssSelector(".label span")).getText
  def stepLevel(row: Int) = {
    val lvl = steps(row).getAttribute(StepLevelAttribute)
    lvl should fullyMatch regex ("^\\d+$")
    lvl.toInt
  }
  def addButtonCount = addButtons.size
  def deleteButtonVisibility(row: Int) = delButton(row).isDisplayed
  def indentDecButtonVisibility(row: Int) = indentDecButton(row).isDisplayed
  def indentIncButtonVisibility(row: Int) = indentIncButton(row).isDisplayed
}

// =====================================================================================================================

class UseCaseIndexDSL(val s: SeleniumDriver) extends BaseDSL {
  override val url = "/list"

  def assertItemCount(totalCount: Int, editableCount: Int = 0) = eventually {
    val actual = List(".no_ucs", ".some_ucs .uc", ".some_ucs .uc.edit").map(findVisible(_).size)
    actual should be(List(if (totalCount == 0) 1 else 0, totalCount, editableCount))
    this
  }

  def clickNewUc() = { s.findElement(By.cssSelector(".new_uc button")).click; this }

  def row(row: Int) = new Row(row)

  class Row(row: Int) {
    def elem = findVisible(".some_ucs .uc")(row)
    def link = elem.findElement(By.tagName("a"))
    def linkUrl = link.getAttribute("href")
    def editTextarea = elem.findElement(By.tagName("textarea"))
    def assertLinkText(txt: String) = { eventually {link.getText should be(txt)}; this }
    def assertEditText(txt: String) = { eventually {editTextarea.value should be(txt)}; this }
//      def enterText(txt: String) = { println("ready?"); Thread.sleep(3000); new Actions(s).moveToElement(elem).click().perform(); println("done"); Thread.sleep(3000);elem.sendKeys(txt + "\n"); UseCaseIndexDSL.this }
  }
}

// =====================================================================================================================

class LoginDSL(val s: SeleniumDriver) extends BaseDSL {
  override val url = "/login"

  def usernameElem = s.findElement(By.id("who"))
  def passwordElem = s.findElement(By.id("password"))
  def rememberMeElem = s.findElement(By.id("remember"))

  def enterUsername(username: String) = {usernameElem.typeInto(username); this}
  def enterPassword(password: String) = {passwordElem.typeInto(password); this}
  def setRememberMe(on: Boolean) = {if (rememberMeElem.isSelected != on) rememberMeElem.click(); this}

  def login() = s.findElement(By.cssSelector("[type=submit]")).click()
}