package com.beardedlogic.usecase
package snippet

import net.liftweb.http.{ StatefulSnippet, Templates }
import net.liftweb.http.SHtml
import net.liftweb.http.SHtml.ElemAttr.pairToBasic
import net.liftweb.http.js.{ JE, JsCmd, JsCmds }
import net.liftweb.http.js.JsCmds.jsExpToJsCmd
import net.liftweb.http.js.JsExp.strToJsExp
import net.liftweb.http.js.jquery.JqJE
import net.liftweb.util.ClearClearable
import net.liftweb.util.Helpers._
import scala.xml.Text

/**
 * @since 29/04/13
 */
object UCEditor {

  val StepTemplate = {
    val ExtractStepTemplate = ".step ^^" #> ""
    val index = Templates("index" :: Nil).open_!
    ExtractStepTemplate(ClearClearable(index))
  }

  val AttrLevel = "data-lvl"
}

/**
 *
 * @since 26/04/2013
 */
class UCEditor extends StatefulSnippet {
  import UCEditor._
  import lib.StepTree._
  import lib.JsExt._

  override def dispatch = { case _ => render }

  val id = 1
  var title = ""
  var courses: List[StepNode] =
    StepNode(nextFuncName, 0, Some(id + "."), 0, NewStep,
      new StepNode(nextFuncName, 1, 1, NewStep) :: Nil
    ) :: Nil

//  courses =
//    StepNode(nextFuncName, 0, Some(id + "."), 0, NewStep,
//      new StepNode(nextFuncName, 1, 1, NewStep) ::
//        new StepNode(nextFuncName, 1, 2, NewStep, (
//          new StepNode(nextFuncName, 2, 1, NewStep) ::
//          new StepNode(nextFuncName, 2, 2, NewStep) ::
//          new StepNode(nextFuncName, 2, 3, NewStep) ::
//          Nil
//        )) ::
//        new StepNode(nextFuncName, 1, 3, NewStep) ::
//        Nil
//    ) :: Nil

  def render = (
    "#steps *" #> StepTemplate andThen
    ".step" #> renderSteps(courses)
    & "#uc_id_num" #> id
    & "@title" #> SHtml.ajaxText(title, onTitleChange(_))
  )

  private def renderSteps(nodes: List[StepNode]) = flattenNodes(nodes).map(renderStep)

  private def renderStep(n: StepNode) = (
    ".step [id]" #> n.id
    & s".step [$AttrLevel]" #> n.level
    & ".label *" #> n.label
    & ".label [id]" #> n.labelId
    & "@text" #> SHtml.textarea(n.step.text, (_) => (), "rows" -> "2", "id" -> n.stepTextId)
    & ".add" #> SHtml.ajaxButton("Add", () => onAddStep(n.id))
    & ".indentDec" #> SHtml.ajaxButton("<<", () => onIndentDecrease(n.id))
    & ".indentInc" #> SHtml.ajaxButton(">>", () => onIndentIncrease(n.id))
  )

  /**
   * When the Use Case title is changed, this will update the Normal Course title unless the user has overridden it.
   */
  def onTitleChange(newTitle: String): JsCmd = {
    val oldTitle = title
    title = newTitle
    val ncId = courses.head.stepTextId
    (
      JsCmds.JsIf(
        JE.JsEq(oldTitle, JE.ValById(ncId)),
        JsCmds.SetValById(ncId, newTitle))
    )
  }

  /**
   * Adds a new step, shuffling down subsequent steps and renumbering if necessary.
   */
  def onAddStep(preceedingNodeId: String): JsCmd = stepInsert(NewStep, preceedingNodeId, courses) match {
    case (newCourses, Some(newNode)) =>
      courses = newCourses
      val fn = ".step" #> renderStep(newNode)
      (
        JqId(preceedingNodeId) ~> JqAfter(fn(StepTemplate))
        & JqId(newNode.id) ~> JqHide ~> JqSlideDownFast
        & UpdateLabels(flattenNodes(courses))
      )
    case _ => JsCmds.Noop
  }

  /**
   * Decreases the indentation level of a given step.
   */
  def onIndentDecrease(nodeId: String): JsCmd = indentDecrease(nodeId, courses) match {
    case (newCourses, true) =>
      courses = newCourses
      val flattenedCourses = flattenNodes(courses)
      (
        UpdateIndentation(flattenedCourses)
        & UpdateLabels(flattenedCourses)
      )
    case _ => JsCmds.Noop
  }

  /**
   * Increases the indentation level of a given step.
   */
  def onIndentIncrease(nodeId: String): JsCmd = indentIncrease(nodeId, courses) match {
    case (newCourses, true) =>
      courses = newCourses
      val flattenedCourses = flattenNodes(courses)
      (
        UpdateIndentation(flattenedCourses)
        & UpdateLabels(flattenedCourses)
      )
    case _ => JsCmds.Noop
  }

  /**
   * Creates Javascript to update the indentation levels of all given nodes.
   */
  private def UpdateIndentation(nodes: Iterable[StepNode]): JsCmd = JsCmds.Run(
    (for (n <- nodes) yield (
      JqId(n.id) ~> JqJE.JqAttr(AttrLevel, n.level.toString) toJsCmd
    )) mkString ";\n"
  )

  /**
   * Creates Javascript to update the label text of all given nodes.
   */
  private def UpdateLabels(nodes: Iterable[StepNode]): JsCmd = JsCmds.Run(
    (for (n <- nodes) yield (
      JsCmds.SetHtml(n.labelId, Text(n.label)).toJsCmd
    )) mkString "\n"
  )
}
