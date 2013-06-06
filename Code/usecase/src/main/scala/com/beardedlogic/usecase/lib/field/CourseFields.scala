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

abstract class CourseFields extends Field[CourseFieldState] {

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
   *
   * @param addTailStepCss The CSS selector that will locate the addTailStep container.
   */
  protected def renderSteps(steps: List[StepNode],
                            addTailStepCss: String): NodeSeq => NodeSeq = {
    val t = "button" #> SHtml.ajaxButton("+", () => onAddTailStep(addTailStepCss))
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

  /** Creates a new top-level step to add to the end of the list. */
  protected def newTailStep(): StepNode

  // TODO change all CourseField step manipulations into pure + web funcs (and rename to improve consistency)

  /** Adds a new top-level step to the end of the list. */
  def addTailStep(implicit reactor: Reactor): StepNode = {
    val newNode = newTailStep()
    setCourses(courses :+ newNode)
    createAndRegisterTextField(newNode)
    newNode
  }

  /** Callback for user to add a new top-level step to the end of the list. */
  protected def onAddTailStep(addTailStepCss: String): JsCmd = JavaScriptReaction{ reactor =>
    val newNode = addTailStep(reactor)
    reactor << JqExpr(addTailStepCss) ~> JqBefore(renderSingleStepXml(newNode))
    reactor << JqId(newNode.id) ~> JqHide ~> JqSlideDownFast
  }

  /**
   * Adds a new step, shuffling down subsequent steps and renumbering if necessary.
   */
  def stepAdd[R](preceedingNodeId: String @@ LocalId)(implicit reactor: Reactor): Option[StepNode] =
    stepInsert(preceedingNodeId, courses, StepNodeBuilder) match {
      case (newCourses, r@Some(newNode)) =>
        setCourses(newCourses)
        createAndRegisterTextField(newNode)
        r
      case _ => None
    }
  protected def onStepAdd(preceedingNodeId: String @@ LocalId): JsCmd = JavaScriptReaction { reactor =>
    stepAdd(preceedingNodeId)(reactor) foreach { newNode =>
      reactor << JqId(preceedingNodeId) ~> JqAfter(renderSingleStepXml(newNode))
      reactor << JqId(newNode.id) ~> JqHide ~> JqSlideDownFast
      reactor << UpdateLabels(courses)
    }
  }

  def prohibitRemoval(id: String @@ LocalId) = false

  /**
   * Removes a new step and all its children, shuffling up following steps and renumbering if necessary.
   */
  protected def onStepRemove(id: String @@ LocalId): JsCmd = JavaScriptReaction { reactor =>
    if (!prohibitRemoval(id))
      stepRemove(id, courses) match {
        case (newCourses, Some(node)) =>
          // TODO fields aren't being removed
          setCourses(newCourses)(reactor)
          reactor << FadeOut(JqExprForNodeAndChildren(node), 240)(_ ~> JqJE.JqRemove() & UpdateLabels(courses))
        case _ =>
      }
  }

  /**
   * Decreases the indentation level of a given step.
   */
  def stepIndentDecrease(nodeId: String @@ LocalId)(implicit reactor: Reactor): Boolean =
    indentDecrease(nodeId, courses) match {
      case (newCourses, Some(_)) => setCourses(newCourses); true
      case _                     => false
    }
  protected def onIndentDecrease(nodeId: String @@ LocalId): JsCmd = JavaScriptReaction { reactor =>
    if (stepIndentDecrease(nodeId)(reactor)) {
      val updateJs = UpdateIndentation(courses) & UpdateLabels(courses)
      reactor << customiseIndentDecreaseJs(nodeId, updateJs)
    }
  }

  /**
   * Allows customisation of the ajax response of a successful indent decrease.
   */
  protected def customiseIndentDecreaseJs(nodeId: String @@ LocalId, updateJs: JsCmd): JsCmd = updateJs

  /**
   * Increases the indentation level of a given step.
   */
  protected def onIndentIncrease(nodeId: String @@ LocalId): JsCmd = JavaScriptReaction { reactor =>
    indentIncrease(nodeId, courses) match {
      case (newCourses, Some(newNode)) =>
        val oldCourses = courses
        setCourses(newCourses)(reactor)
        val updateJs = UpdateIndentation(courses) & UpdateLabels(courses)
        reactor << customiseIndentIncreaseJs(nodeId, newNode, oldCourses, updateJs)
      case _ =>
    }
  }

  /**
   * Allows customisation of the ajax response of a successful indent increase.
   */
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
