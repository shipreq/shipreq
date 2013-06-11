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
import msg._
import Messages.StepChangeMsg
import model._
import FieldValue.FieldValueData
import StepLabels.{MaxStepsPerLevel, MaxStepDepth}

object CourseFields {
  import TemplateCache._

  val StepTemplate = UseCaseEditorTemplate.extract("template-step")

  val AttrLevel = "data-lvl" // TODO rename this unclear thing

  val AddStepTemplate = ".steps * " #> StepTemplate
  val AddTailStepTemplate = UseCaseEditorTemplate.extract("template-courses-addTailStep")
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
    oldStepValues: Map[String @@ LocalId, PlainValue[DataType.Step]],
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

abstract class CourseFields extends Field[CourseFieldState] with SnippetHelpers {

  private[this] var _courses: List[StepNode] = Nil
  private[this] var textFields: Map[String @@ LocalId, SmartStepText] = Map.empty

  def courses = _courses
  def test__textFields = textFields

  val stepLabelMap = CachedFunction.lazy0(mapIdsToFullLabels(courses, rootLabelPrefix.get.getOrElse("")))
  val rootLabelPrefix = CachedFunction.eager0(recalcRootLabelPrefix)

  override def init() {
    syncTextFieldMap
    setCourses(_courses)(NoReactionOrNewMessages)
  }

  protected def recalcRootLabelPrefix: Option[String]
  @inline def labelPrefixForLevel(level: Int) = if (level == 0) rootLabelPrefix.get else None
  @inline def labelFor(node: StepNode) = labelPrefixForLevel(node.level).map(_ + node.label).getOrElse(node.label)
  def startingLabelIndices: StartingLabelIndices

  def setCourses(newCourses: List[StepNode])(implicit reactor: Reactor) {
    _courses = newCourses
    stepLabelMap.invalidate
    ucCtx.stepLabelMap.invalidate
    msgCentre ! StepChangeMsg
  }

  private[this] def createAndRegisterTextField(n: StepNode) {
    val t = new SmartStepText(msgCentre, ucCtx.stepLabelMap, n.id, n.stepTextId)
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
   */
  protected def renderStepsWithAddTailStep(steps: List[StepNode]): NodeSeq => NodeSeq = {
    val t = "button" #> SHtml.ajaxButton("+", jsCallback(addTailStep(_)))
    val addTailStepTmpl = t(AddTailStepTemplate)
    renderSteps(steps) andThen ".steps *+" #> addTailStepTmpl // Append to .steps, after all the .step tags
  }

  /**
   * Provides the transformation to render a single step. (Does not render step children.)
   */
  protected def renderSingleStep(n: StepNode) = (
    ".step [id]" #> n.id
    & s".step [$AttrLevel]" #> n.level
    & IfCssSel(prohibitRemoval_?(n.id)) { ".step [class+]" #> "noDel" }
    & ".label span *" #> labelFor(n)
    & ".label span [id]" #> n.labelId
    & "@text" #> textFields(n.id).renderTextarea
    & ".add" #> SHtml.ajaxButton("+", jsCallback(addStep(n.id)(_)))
    & ".delete" #> SHtml.ajaxButton("-", jsCallback(removeStep(n.id)(_)))
    & ".indentDec" #> SHtml.ajaxButton("«", jsCallback(decreaseIndent(n.id)(_)))
    & ".indentInc" #> SHtml.ajaxButton(<span>»</span>, jsCallback(increaseIndent(n.id)(_)))
  )

  /**
   * Renders a single step into XML. (Does not render step children.)
   */
  @inline protected def renderSingleStepXml(n: StepNode) = {
    val fn = ".step" #> renderSingleStep(n)
    fn(StepTemplate)
  }

  /** Creates a new top-level step to add to the end of the list. */
  protected def buildNewTailStep(): StepNode

  /** A CSS expression that selects the container of the add-tail-step button. */
  def tailStepCss: String

  /** Adds a new top-level step to the end of the list. */
  def addTailStep(implicit reactor: Reactor): Boolean = {
    val newNode = buildNewTailStep()
    val newCourses = courses :+ newNode
    if (validateCourses(newCourses)) {
      setCourses(newCourses)
      createAndRegisterTextField(newNode)
      reactor(JavaScript)(
        JqExpr(tailStepCss) ~> JqBefore(renderSingleStepXml(newNode))
          & JqId(newNode.id) ~> JqHide ~> JqSlideDownFast
      )
      true
    } else false
  }

  /** Adds a new step, shuffling down subsequent steps and renumbering if necessary. */
  def addStep[R](preceedingNodeId: String @@ LocalId)(implicit reactor: Reactor): Option[StepNode] =
    stepInsert(preceedingNodeId, courses, StepNodeBuilder) match {
      case (newCourses, r@Some(newNode)) if (validateCourses(newCourses)) =>
        setCourses(newCourses)
        createAndRegisterTextField(newNode)
        reactor(JavaScript)(
          JqId(preceedingNodeId) ~> JqAfter(renderSingleStepXml(newNode))
            & JqId(newNode.id) ~> JqHide ~> JqSlideDownFast
            & UpdateLabels(courses)
        )
        r
      case _ => None
    }

  /** Removes a new step and all its children, shuffling up following steps and renumbering if necessary. */
  def removeStep(id: String @@ LocalId)(implicit reactor: Reactor): Option[StepNode] =
    if (!prohibitRemoval_?(id))
      stepRemove(id, courses) match {
        case (newCourses, r@Some(node)) =>
          setCourses(newCourses)
          textFields -= id
          reactor(JavaScript)(
            FadeOut(JqExprForNodeAndChildren(node), 240)(_ ~> JqJE.JqRemove() & UpdateLabels(courses))
          )
          r
        case _ => None
      }
    else None

  /** Decreases the indentation level of a given step. */
  def decreaseIndent(nodeId: String @@ LocalId)(implicit reactor: Reactor): Boolean =
    indentDecrease(nodeId, courses) match {
      case (newCourses, Some(_)) if (validateCourses(newCourses)) =>
        setCourses(newCourses)
        reactor(JavaScript)(
          customiseIndentDecreaseJs(nodeId, UpdateIndentation(courses) & UpdateLabels(courses))
        )
        true
      case _ => false
    }

  /** Increases the indentation level of a given step. */
  def increaseIndent(nodeId: String @@ LocalId)(implicit reactor: Reactor): Boolean =
    indentIncrease(nodeId, courses) match {
      case (newCourses, Some(newNode)) if (validateCourses(newCourses)) =>
        val oldCourses = courses
        setCourses(newCourses)
        reactor(JavaScript)(
          customiseIndentIncreaseJs(nodeId, newNode, oldCourses, UpdateIndentation(courses) & UpdateLabels(courses))
        )
        true
      case _ => false
    }

  /** Validates courses are legal. If not, a reaction is provided and `false` returned. */
  def validateCourses(courses: List[StepNode])(implicit reactor: Reactor): Boolean = {
    def invalid(l: List[StepNode]): Boolean = {
      if (l.isEmpty) false
      else if (l.last.labelIndex > MaxStepsPerLevel) {reactToMaxStepViolation; true }
      else if (l.head.level >= MaxStepDepth) {reactToMaxLevelViolation; true }
      else l.exists(n => invalid(n.children))
    }
    !invalid(courses)
  }

  private def reactToMaxStepViolation(implicit reactor: Reactor) {
    reactor(JavaScript)(JsCmds.Alert(s"That would cause you to have ${MaxStepsPerLevel + 1} steps at the same level, which exceeds the maximum allowed."))
  }

  private def reactToMaxLevelViolation(implicit reactor: Reactor) {
    reactor(JavaScript)(JsCmds.Alert(s"That would cause your steps to be ${MaxStepDepth + 1} levels deep, which exceeds the maximum allowed."))
  }

  def prohibitRemoval_?(id: String @@ LocalId) = false

  /** Allows customisation of the ajax response of a successful indent decrease. */
  protected def customiseIndentDecreaseJs(nodeId: String @@ LocalId, updateJs: JsCmd): JsCmd = updateJs


  /** Allows customisation of the ajax response of a successful indent increase. */
  protected def customiseIndentIncreaseJs(nodeId: String @@ LocalId,
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
    rootLabelPrefix.refresh

    val newCourses = convertNodeTree[StepState, StepNode](newState.courses, { case (node, level, index, children) =>
      StepNode(node.id, level, index, children)
    }, startingLabelIndices.startingLabelIndex _)
    setCourses(newCourses)(NoReactionOrNewMessages)

    syncTextFieldMap

    () => {
      val stepMap = newState.stepMap
      val savedSteps = ucCtx.savedSteps.get
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

    state.refresh

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
        compareAndSaveChanges(oldFieldState, oldSaveCtx.stepValues, state.get.courses, saveCtx, dao)
    }
  }

  override def save(
    combinedSaveCtx: FieldSaveCtx,
    newSaveCtx: FieldSaveCtx,
    dao: DAO
    ): (FieldValueData, CourseFieldState) = {

    // Required again because normalised refs may be different after presave
    state.refresh

    // Create steps
    for {
      (localId, v) <- newSaveCtx.stepValues
      ss <- state.get.stepMap.get(localId)
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
      (ss,i) <- state.get.courses.zipWithIndex
      stepValue <- combinedSaveCtx.stepValues.get(ss.id)
    } dao.relate_stepParent_has_step(fv, i.toShort, stepValue)

    (None, state.get)
  }

  val state = CachedFunction.eager0WithInitial({
    val stepStateList = convertNodeTree[StepNode, StepState](
      courses, { (ss, level, index, children) =>
          StepState(ss.id, textFields(ss.id).textWithNormalisedRefs(ucCtx), children)
      }, startingLabelIndices.startingLabelIndex _
    )
    CourseFieldState(stepStateList)
  })(null)
}
