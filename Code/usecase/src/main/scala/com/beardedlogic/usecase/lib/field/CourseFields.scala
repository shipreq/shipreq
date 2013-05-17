package com.beardedlogic.usecase.lib
package field

import net.liftweb.http.SHtml
import net.liftweb.http.js.{ JsCmd, JsCmds, JE }
import net.liftweb.http.js.JsCmds.jsExpToJsCmd
import net.liftweb.http.js.jquery.JqJE
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._
import scala.annotation.tailrec
import scala.xml._
import JsExt._
import StepTree._
import msg.Messages._

object CourseFields {
  import Fields.Template

  val StepTemplate = Template("template-step")

  val AttrLevel = "data-lvl" // TODO rename this unclear thing

  val AddStepTemplate = ".steps * " #> StepTemplate
  val AddTailStepTemplate = Template("template-courses-addTailStep")
  val AddTailStepClass = "addTailStep"

  def ExprForNodeAndChildren(n: StepNode) = (n :: flattenNodes(n.children)).map("#" + _.id).mkString(",")
  @inline def JqExprForNodeAndChildren(n: StepNode) = JqExpr(ExprForNodeAndChildren(n))

  // TODO Move IfCssSel and PassThru elsewhere
  val PassThru = "dpp_recommends_this_oh_well" #> ""
  def IfCssSel(cond: => Boolean)(expr: => CssSel): CssSel = if (cond) expr else PassThru
}

abstract class CourseFields extends Field {
  import CourseFields._

  val id = 1

  private[this] var _courses: List[StepNode] = Nil
  protected def courses_=(newCourses: List[StepNode]) {
    _courses = newCourses
    _stepLabelMap = null
      msgCentre ! StepChangeMsg
  }
  def courses = _courses

  private[this] var _stepLabelMap: Map[String, String] = Map.empty
  def stepLabelMap = {
    if (_stepLabelMap == null) _stepLabelMap = mapIdsAndFullLabels(courses)
    _stepLabelMap
  }

  private[this] var textFields: Map[String, SmartStepText] = Map.empty

  override def init() {
    for (n <- flattenNodes(courses)) createAndRegisterTextField(n)
  }

  private[this] def createAndRegisterTextField(n:StepNode) {
    val f = new SmartStepText(msgCentre, state.stepLabelMapProvider, n.id, n.stepTextId)
    f.init
    textFields += (n.id -> f)
  }

  //  private[this] def syncTextFieldMap() {
  //    val oldTextFields = textFields
  //    textFields = Map.empty
  //    for (n <- flattenNodes(courses)) {
  //      val id = n.id
  //      val f = if (oldTextFields.contains(id))
  //        oldTextFields(id)
  //      else
  //        newTextField(n, _initCalled)
  //      textFields += (id -> f)
  //    }
  //  }

//  @inline private[this] def newTextField(n:StepNode, init: Boolean) = {
//    val f = new SmartText(msgCentre, state.stepLabelMapProvider, n.stepTextId)
//    if (init) f.init
//    f
//  }

  /**
   * Renders a list of steps and their trees of children.
   */
  protected def renderSteps(steps: List[StepNode]): CssSel =
    ".step" #> flattenNodes(steps).map(renderSingleStep)

  /**
   * Renders a list of steps and their trees of children.
   * Also renders an addTailStep button.
   *
   * @param addTailStepCss The CSS selector that will locate the addTailStep container.
   * @param newStepFn A function that will create a new StepNode when called.
   */
  protected def renderSteps(steps: List[StepNode],
                            addTailStepCss: String,
                            newStepFn: () => StepNode): Function1[NodeSeq, NodeSeq] = {
    val t = "button" #> SHtml.ajaxButton("+", () => onAddTailStep(addTailStepCss, newStepFn))
    val addTailStep = t(AddTailStepTemplate)
    renderSteps(steps) andThen ".steps *+" #> addTailStep // Append to .steps, after all the .step tags
  }

  /**
   * Provides the transformation to render a single step. (Does not render step children.)
   */
  protected def renderSingleStep(n: StepNode) = (
    ".step [id]" #> n.id
    & s".step [$AttrLevel]" #> n.level
    & IfCssSel(prohibitRemoval(n.id)) { ".step [class+]" #> "noDel" }
    & ".label span *" #> n.label
    & ".label span [id]" #> n.labelId
    & "@text" #> textFields(n.id).renderTextarea
    & ".add" #> SHtml.ajaxButton("+", () => onStepAdd(n.id))
    & ".delete" #> SHtml.ajaxButton("-", () => onStepRemove(n.id))
    & ".indentDec" #> SHtml.ajaxButton("«", () => onIndentDecrease(n.id))
    & ".indentInc" #> SHtml.ajaxButton(<span>»</span>, () => onIndentIncrease(n.id))
  )

  /**
   * Renders a single step into XML. (Does not render step children.)
   */
  @inline protected def renderSingleStepXml(n: StepNode) = {
    val fn = ".step" #> renderSingleStep(n)
    fn(StepTemplate)
  }

  /**
   * Adds a new top-level step to the end of the list.
   */
  private def onAddTailStep(addTailStepCss: String, newStepFn: () => StepNode): JsCmd = {
    val newNode = newStepFn()
    courses = courses :+ newNode
    createAndRegisterTextField(newNode)
    (
      JqExpr(addTailStepCss) ~> JqBefore(renderSingleStepXml(newNode))
      & JqId(newNode.id) ~> JqHide ~> JqSlideDownFast
    )
  }

  /**
   * Adds a new step, shuffling down subsequent steps and renumbering if necessary.
   */
  def onStepAdd(preceedingNodeId: String): JsCmd = stepInsert(NewStep, preceedingNodeId, courses) match {
    case (newCourses, Some(newNode)) =>
      courses = newCourses
      createAndRegisterTextField(newNode)
      (
        JqId(preceedingNodeId) ~> JqAfter(renderSingleStepXml(newNode))
        & JqId(newNode.id) ~> JqHide ~> JqSlideDownFast
        & UpdateLabels(flattenNodes(courses))
      )
    case _ => JsCmds.Noop
  }

  def prohibitRemoval(id: String) = false

  /**
   * Removes a new step and all its children, shuffling up following steps and renumbering if necessary.
   */
  def onStepRemove(id: String): JsCmd =
    if (prohibitRemoval(id))
      JsCmds.Noop
    else
      stepRemove(id, courses) match {
        case (newCourses, Some(node)) =>
          courses = newCourses
          FadeOut(JqExprForNodeAndChildren(node), 240)(
            _ ~> JqJE.JqRemove() & UpdateLabels(flattenNodes(courses))
          )
        case _ => JsCmds.Noop
      }

  /**
   * Decreases the indentation level of a given step.
   */
  def onIndentDecrease(nodeId: String): JsCmd = indentDecrease(nodeId, courses) match {
    case (newCourses, Some(_)) =>
      courses = newCourses
      val flattenedCourses = flattenNodes(courses)
      val updateJs = UpdateIndentation(flattenedCourses) & UpdateLabels(flattenedCourses)
      customiseIndentDecreaseJs(nodeId, updateJs)

    case _ => JsCmds.Noop
  }

  /**
   * Allows customisation of the ajax response of a successful indent decrease.
   */
  protected def customiseIndentDecreaseJs(nodeId: String, updateJs: JsCmd): JsCmd = updateJs

  /**
   * Increases the indentation level of a given step.
   */
  def onIndentIncrease(nodeId: String): JsCmd = indentIncrease(nodeId, courses) match {
    case (newCourses, Some(newNode)) =>
      val oldCourses = courses
      courses = newCourses
      val flattenedCourses = flattenNodes(courses)
      val updateJs = UpdateIndentation(flattenedCourses) & UpdateLabels(flattenedCourses)
      customiseIndentIncreaseJs(nodeId, newNode, oldCourses, updateJs)

    case _ => JsCmds.Noop
  }

  /**
   * Allows customisation of the ajax response of a successful indent increase.
   */
  protected def customiseIndentIncreaseJs(nodeId: String,
                                          newNode: StepNode,
                                          oldCourses: List[StepNode],
                                          updateJs: JsCmd): JsCmd = updateJs

  /**
   * Creates Javascript to update the indentation levels of all given nodes.
   */
  protected def UpdateIndentation(nodes: Iterable[StepNode]): JsCmd = JsCmds.Run(
    (for (n <- nodes) yield (
      JqId(n.id) ~> JqJE.JqAttr(AttrLevel, n.level.toString) toJsCmd
    )) mkString ";\n"
  )

  /**
   * Creates Javascript to update the label text of all given nodes.
   */
  protected def UpdateLabels(nodes: Iterable[StepNode]): JsCmd = JsCmds.Run(
    (for (n <- nodes) yield (
      JsCmds.SetHtml(n.labelId, Text(n.label)).toJsCmd
    )) mkString "\n"
  )
}
