package com.beardedlogic.usecase
package lib
package field

import net.liftweb.http.SHtml
import net.liftweb.http.js.{ JsCmd, JsCmds, JE }
import net.liftweb.http.js.JsCmds.jsExpToJsCmd
import net.liftweb.http.js.jquery.JqJE
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers.strToCssBindPromoter
import scala.xml._

import JsExt._
import tree.TreeOps._
import TypeTags._
import CourseFields._
import msg.Messages._
import model._
import FieldValue.FieldValueData

object CourseFields {
  import Fields.Template

  val StepTemplate = Template("template-step")

  val AttrLevel = "data-lvl" // TODO rename this unclear thing

  val AddStepTemplate = ".steps * " #> StepTemplate
  val AddTailStepTemplate = Template("template-courses-addTailStep")
  val AddTailStepClass = "addTailStep"

  def ExprForNodeAndChildren(n: StepNode) = n.map("#" + _.id).mkString(",")
  @inline def JqExprForNodeAndChildren(n: StepNode) = JqExpr(ExprForNodeAndChildren(n))

  // TODO Move IfCssSel and PassThru elsewhere
  val PassThru = "dpp_recommends_this_oh_well" #> ""
  def IfCssSel(cond: => Boolean)(expr: => CssSel): CssSel = if (cond) expr else PassThru

  trait StartingLabelIndices {
    def startingLabelIndex(level: Int): Int
  }

  object StartingRootLabelIndexAt0 extends StartingLabelIndices {
    override def startingLabelIndex(level: Int) = if (level == 0) 0 else 1
  }

  object StartingLabelIndicesAt1 extends StartingLabelIndices {
    override def startingLabelIndex(level: Int) = 1
  }

  /**
   * Compares old and current states. When a difference is discovered a new `value` is inserted and stored in
   * `saveCtx.stepValues`. Where a step can be reused, it is.
   *
   * SIDE EFFECTS:
   * - `saveCtx.stepValues` << new & updated steps
   * - `dao` << new & updated steps
   *
   * @return Whether any changes were discovered.
   */
  def compareAndSaveChanges(
    oldState: CourseFieldState,
    oldStepValues: Map[String @@ LocalStepId, PlainValue[DataType.Step]],
    newState: List[StepState],
    saveCtx: MutableFieldSaveCtx,
    dao: DAO
    ): Boolean = {

    val oldStateMap = oldState.stepMap
    def iter(newState: List[StepState]): Boolean = newState match {
      case curStep :: nextSiblings =>

        // Check children first
        var changeDetected = iter(curStep.children)

        // Check current node
        val oldStep = oldStateMap.get(curStep.id)
        val reusable = !changeDetected && oldStep.isDefined && oldStep.get == curStep
        // println(s"${curStep.text}(${curStep.id}) vs ${oldStep.map(_.text)}(${oldStep.map(_.id).getOrElse("")}) => reusable: ${reusable}")
        if (!reusable) {
          val newValue = oldStep.flatMap(ss => oldStepValues.get(ss.id)) match {
            case Some(oldStepValue) => dao.createValue(oldStepValue, LatestRev) // update
            case None               => dao.createInitialValue(DataType.Step) // insert new
          }
          saveCtx.stepValues += (curStep.id -> newValue)
          changeDetected = true
        }

        // Check next sibling
        if (iter(nextSiblings)) changeDetected = true

        changeDetected

      case Nil => false
    }

    var changedDetected = iter(newState)
    changedDetected || {
      // Top-level order could be different and no changes would otherwise be detected
      oldState.courses != newState
    }
  }
}

// =====================================================================================================================

abstract class CourseFields extends Field[CourseFieldState] {

  private[this] var _courses: List[StepNode] = Nil
  def courses_=(newCourses: List[StepNode]) {
    _courses = newCourses
    _stepLabelMap = null
    msgCentre ! StepChangeMsg
  }
  def courses = _courses

  private[this] var _stepLabelMap: Map[String @@ LocalStepId, String] = Map.empty
  def stepLabelMap = {
    if (_stepLabelMap == null) _stepLabelMap = mapIdsToFullLabels(courses, rootLabelPrefix.getOrElse(""))
    _stepLabelMap
  }

  private[this] var textFields: Map[String @@ LocalStepId, SmartStepText] = Map.empty
  def test__textFields = textFields

  override def init() {
    syncTextFieldMap
  }

  protected def recalcRootLabelPrefix: Option[String]
  private [this] var _rootLabelPrefix = recalcRootLabelPrefix
  final def rootLabelPrefix = _rootLabelPrefix
  @inline def labelPrefixForLevel(level: Int) = if (level == 0) rootLabelPrefix else None
  @inline def labelFor(node: StepNode) = labelPrefixForLevel(node.level).map(_ + node.label).getOrElse(node.label)
  def startingLabelIndices: StartingLabelIndices

  private[this] def createAndRegisterTextField(n: StepNode) {
    val t = new SmartStepText(msgCentre, ucCtx.stepLabelMapProvider, n.id, n.stepTextId)
    t.init
    textFields += (n.id -> t)
  }

  private[this] def syncTextFieldMap() {
    val oldTextFields = textFields
    textFields = Map.empty
    courses.foreachNode{ n =>
      val id = n.id
      if (oldTextFields.contains(id)) {
        textFields += (id -> oldTextFields(id))
      } else {
        createAndRegisterTextField(n)
      }
    }
  }

  /**
   * Renders a list of steps and their trees of children.
   */
  protected def renderSteps(steps: List[StepNode]): CssSel =
    ".step" #> steps.mapEachNode(renderSingleStep)

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
    & ".label span *" #> labelFor(n)
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
  def onStepAdd(preceedingNodeId: String @@ LocalStepId): JsCmd = stepInsert(preceedingNodeId, courses, StepNodeBuilder) match {
    case (newCourses, Some(newNode)) =>
      courses = newCourses
      createAndRegisterTextField(newNode)
      (
        JqId(preceedingNodeId) ~> JqAfter(renderSingleStepXml(newNode))
        & JqId(newNode.id) ~> JqHide ~> JqSlideDownFast
        & UpdateLabels(courses)
      )
    case _ => JsCmds.Noop
  }

  def prohibitRemoval(id: String @@ LocalStepId) = false

  /**
   * Removes a new step and all its children, shuffling up following steps and renumbering if necessary.
   */
  def onStepRemove(id: String @@ LocalStepId): JsCmd =
    if (prohibitRemoval(id))
      JsCmds.Noop
    else
      stepRemove(id, courses) match {
        case (newCourses, Some(node)) =>
          courses = newCourses
          FadeOut(JqExprForNodeAndChildren(node), 240)(
            _ ~> JqJE.JqRemove() & UpdateLabels(courses)
          )
        case _ => JsCmds.Noop
      }

  /**
   * Decreases the indentation level of a given step.
   */
  def onIndentDecrease(nodeId: String @@ LocalStepId): JsCmd = indentDecrease(nodeId, courses) match {
    case (newCourses, Some(_)) =>
      courses = newCourses
      val updateJs = UpdateIndentation(courses) & UpdateLabels(courses)
      customiseIndentDecreaseJs(nodeId, updateJs)

    case _ => JsCmds.Noop
  }

  /**
   * Allows customisation of the ajax response of a successful indent decrease.
   */
  protected def customiseIndentDecreaseJs(nodeId: String @@ LocalStepId, updateJs: JsCmd): JsCmd = updateJs

  /**
   * Increases the indentation level of a given step.
   */
  def onIndentIncrease(nodeId: String @@ LocalStepId): JsCmd = indentIncrease(nodeId, courses) match {
    case (newCourses, Some(newNode)) =>
      val oldCourses = courses
      courses = newCourses
      val updateJs = UpdateIndentation(courses) & UpdateLabels(courses)
      customiseIndentIncreaseJs(nodeId, newNode, oldCourses, updateJs)

    case _ => JsCmds.Noop
  }

  /**
   * Allows customisation of the ajax response of a successful indent increase.
   */
  protected def customiseIndentIncreaseJs(nodeId: String @@ LocalStepId,
                                          newNode: StepNode,
                                          oldCourses: List[StepNode],
                                          updateJs: JsCmd): JsCmd = updateJs

  /**
   * Creates Javascript to update the indentation levels of all given nodes.
   */
  protected def UpdateIndentation(nodes: List[StepNode]): JsCmd = JsCmds.Run(
    nodes.mapEachNode(n =>
      JqId(n.id) ~> JqJE.JqAttr(AttrLevel, n.level.toString) toJsCmd
    ) mkString ";\n"
  )

  /**
   * Creates Javascript to update the label text of all given nodes.
   */
  protected def UpdateLabels(nodes: List[StepNode]): JsCmd = JsCmds.Run(
    nodes.mapEachNode(n =>
      JsCmds.SetHtml(n.labelId, Text(labelFor(n))).toJsCmd
    ) mkString "\n"
  )

  override def setState(newState: CourseFieldState) = {
    _rootLabelPrefix = recalcRootLabelPrefix

    courses = convertNodeTree[StepState, StepNode](newState.courses, { case (node, level, index, children) =>
      StepNode(node.id, level, index, children)
    }, startingLabelIndices.startingLabelIndex _)

    syncTextFieldMap

    () => {
      val stepMap = newState.stepMap
      val savedSteps = ucCtx.savedSteps
      for ((id, tf) <- textFields) {
        val txt = stepMap(id).text
        tf.setTextFromLoad(txt, savedSteps)
      }
    }
  }

  override def save_? : Boolean = courses.nonEmpty

  override def presave(
    lastSave: Option[(FieldSaveCtx, CourseFieldState)],
    saveCtx: MutableFieldSaveCtx,
    dao: DAO
    ): Boolean = {

    recalcCurrentState()

    lastSave match {

      // No previous save, add everything for first time
      case None =>
        courses.foreachNode { n =>
          val v = dao.createInitialValue(DataType.Step)
          saveCtx.stepValues += (n.id -> v)
        }
        true

      // Compare to previous and save deltas
      case Some((oldSaveCtx, oldFieldState)) =>
        compareAndSaveChanges(oldFieldState, oldSaveCtx.stepValues, currentState.courses, saveCtx, dao)
    }
  }

  override def save(
    combinedSaveCtx: FieldSaveCtx,
    newSaveCtx: FieldSaveCtx,
    dao: DAO
    ): (FieldValueData, CourseFieldState) = {

    // Required again because normalised refs may be different after presave
    recalcCurrentState()

    // Create steps
    for {
      (localId, v) <- newSaveCtx.stepValues
      ss <- currentState.stepMap.get(localId)
    } {
      dao.createStep(v, ss.text)
      for {
        (childState, i) <- ss.children.zipWithIndex
        childValue <- combinedSaveCtx.stepValues.get(childState.id)
      } dao.relate_stepParent_has_step(v, i.toShort, childValue)
    }

    // Link FV to top-level
    val fv = newSaveCtx.fieldValues(fieldKey)
    for {
      (ss,i) <- currentState.courses.zipWithIndex
      stepValue <- combinedSaveCtx.stepValues.get(ss.id)
    } dao.relate_stepParent_has_step(fv, i.toShort, stepValue)

    (None, currentState)
  }

  /**
   * A snapshot of the current state. Required in both presave() and save(). Rather than passing it back out and in
   * again we just store it here in presave() and using it during save().
   */
  private[this] var _stateCache: CourseFieldState = null
  @inline final def currentState = _stateCache

  def recalcCurrentState() { _stateCache = CourseFieldState(buildStateList) }

  /** Builds StepStates from a node tree. */
  def buildStateList(): List[StepState] =
    convertNodeTree[StepNode, StepState](courses, { case (ss, level, index, children) =>
      StepState(ss.id, textFields(ss.id).textWithNormalisedRefs(ucCtx), children)
    }, startingLabelIndices.startingLabelIndex _)
}
