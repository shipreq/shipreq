package com.beardedlogic.usecase
package snippet.uce

import scala.xml.{Text, NodeSeq}
import net.liftweb.common.Full
import net.liftweb.http.js.JsExp.strToJsExp
import net.liftweb.http.js.{JsExp, JE, JsCmd, JsCmds}
import net.liftweb.http.js.jquery.JqJE
import net.liftweb.http.js.jquery.JqJsCmds.jsExpToJsCmd
import net.liftweb.http.S
import net.liftweb.http.SHtml.ajaxTextarea
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._
import JsCmds.Noop

import feature.uc.change.UseCaseUpdater
import feature.uc.field._
import feature.uc.step.{StepTree, StepNode, TreeLike}
import lib.Types._
import util.HtmlTransformExt._
import util.HtmlTransformExt.ajaxOnClick
import util.JsExt._
import Renderer._
import UseCaseEditor._

object StepFieldRenderer {
  @inline final def ExprForNodeAndChildren(n: StepNode) = n.mapRecursive("#" + _.id).mkString(",")
  @inline final def JqExprForNodeAndChildren(n: StepNode) = JqExpr(ExprForNodeAndChildren(n))

  /** ID of the HTML attribute that contains a step's label (eg. "1.0.4.b") */
  @inline final def labelId(id: LocalStepId): String = (id: String) + "-l"

  /** ID of the textarea that contains a step's text. */
  @inline final def textareaId(id: LocalStepId): String = (id: String) + "-t"
}

import StepFieldRenderer._

// -------------------------------------------------------------------------------------------------------------------

trait StepFieldRenderConfig {

  /** A CSS expression that selects the container of the add-tail-step button. */
  def tailStepCss: String

  def prohibitRemoval_?(id: LocalStepId, tree: StepTree): Boolean

  def render(r: StepFieldRenderer): NodeSeq

  /** Allows customisation of the ajax response of a successful indent decrease. */
  def customiseDecIndentJs(node: StepNode, newTree: StepTree, updateJs: JsCmd, focusJs: JsCmd): JsCmd

  /** Allows customisation of the ajax response of a successful indent increase. */
  def customiseIncIndentJs(oldTree: StepTree, node: StepNode, updateJs: JsCmd, focusJs: JsCmd): JsCmd
}

// -------------------------------------------------------------------------------------------------------------------

object NormalCourseFieldConfig extends StepFieldRenderConfig {

  override def tailStepCss = AlternateCourseAddTailStepCss

  override def prohibitRemoval_?(id: LocalStepId, tree: StepTree) = (id == tree.head.id)

  override def render(r: StepFieldRenderer) =
    r.renderSteps(r.tree.head)(Templates.NormalCourse) ++
      r.renderStepsWithAddTailStep(r.tree.tailAsTreeLike)(Templates.AlternateCourses)

  override def customiseDecIndentJs(node: StepNode, newTree: StepTree, updateJs: JsCmd, focusJs: JsCmd) =
    newTree.nodes match {
      // Move steps from NC to AC
      case nc :: ac1 :: acN if ac1.id == node.id =>
        JsCmds.Run(s"nc_to_ac('#uce','${node.id}',${JE.AnonFunc(updateJs).toJsCmd},${JE.AnonFunc(focusJs).toJsCmd})")

      // Apply indent decrease normally
      case _ => updateJs & focusJs
    }

  override def customiseIncIndentJs(oldTree: StepTree, node: StepNode, updateJs: JsCmd, focusJs: JsCmd) =
    oldTree.nodes match {
      // Move steps from AC to NC
      case nc :: ac1 :: acN if ac1.id == node.id =>
        JsCmds.Run(s"ac_to_nc('#uce','${ExprForNodeAndChildren(node)}',${JE.AnonFunc(updateJs).toJsCmd},${JE.AnonFunc(focusJs).toJsCmd})")

      // Apply indent normally
      case _ => updateJs & focusJs
    }
}

// -------------------------------------------------------------------------------------------------------------------

object ExceptionCourseFieldConfig extends StepFieldRenderConfig {

  override def tailStepCss = ExceptionCourseAddTailStepCss

  override def prohibitRemoval_?(id: LocalStepId, tree: StepTree) = false

  override def render(r: StepFieldRenderer) =
    r.renderStepsWithAddTailStep(r.tree)(Templates.ExceptionCourses)

  override def customiseDecIndentJs(node: StepNode, newTree: StepTree, updateJs: JsCmd, focusJs: JsCmd) = updateJs & focusJs
  override def customiseIncIndentJs(oldTree: StepTree, node: StepNode, updateJs: JsCmd, focusJs: JsCmd) = updateJs & focusJs
}

// -------------------------------------------------------------------------------------------------------------------

case class StepFieldRenderer(
  f: StepField,
  cfg: StepFieldRenderConfig,
  state: State,
  modifyUC: UcModifier => JsCmd
  ) extends RendererHelper {

  val rootLabelPrefix = f.rootLabelPrefix(ucNumber)
  val tree = f.value.tree

  @inline final def labelPrefixForLevel(level: Int) = if (level == 0) rootLabelPrefix else ""
  @inline final def labelFor(node: StepNode) = labelPrefixForLevel(node.level) + node.label

  /** The text value of a step. */
  @inline final def text(id: LocalStepId): String = f.value.textmap.get(id).map(_.text).getOrElse("")

  // *************************************
  // *             Rendering             *
  // *************************************

  def render = cfg.render(this)

  /** Renders a list of steps and their trees of children. */
  def renderSteps(steps: TreeLike[StepNode]): CssSel =
    ".step" #> steps.mapRecursive(renderSingleStep)

  /**
   * Renders a list of steps and their trees of children.
   * Also renders an addTailStep button.
   */
  def renderStepsWithAddTailStep(steps: TreeLike[StepNode]): NodeSeq => NodeSeq = {
    val t = "button" #> ajaxOnClick(UcModifier(f.addTailStep, None, None))
    val addTailStepTmpl = t(Templates.AddTailStep)
    renderSteps(steps) andThen ".steps *+" #> addTailStepTmpl // Append to .steps, after all the .step tags
  }

  /**
   * Provides the transformation to render a single step. (Does not render step children.)
   */
  def renderSingleStep(n: StepNode) = {
    val id = n.id
    @inline def %%(fn: LocalStepId => UseCaseUpdater => UcUpdateResult) = UcModifier(fn(id), None, None)
    (
      ".step [id]" #> id
        & StepLevelAttributeCss #> n.level
        & IfCssSel(cfg.prohibitRemoval_?(id, tree)) {".step [class+]" #> "noDel"}
        & ".lbl span *" #> labelFor(n)
        & ".lbl span [id]" #> labelId(id)
        & "@text" #> ajaxTextarea(text(id), modText(id)(_), "id" -> textareaId(id))
        & ".add" #> ajaxOnClick(%%(f.addStep))
        & ".delete" #> ajaxOnClick(%%(f.removeStep))
        & ".indentDec" #> ajaxOnClick(%%(f.decreaseIndent))
        & ".indentInc" #> ajaxOnClick(%%(f.increaseIndent))
      )
  }

  /**
   * Renders a single step into XML. (Does not render step children.)
   */
  @inline final def renderSingleStepXml(n: StepNode) = {
    val fn = ".step" #> renderSingleStep(n)
    fn(Templates.Step)
  }

  // **************************************
  // *             Modifiers              *
  // **************************************

  def modText(id: LocalStepId)(input: String) =
    UcModifier(
      f.updateText(id, input),
      Some(_.stepRenderers(f).jsUpdateStepFieldText(id)),
      Some(JqId(textareaId(id))))

  // **************************************
  // *             Javascript             *
  // **************************************

  @inline private def JqLabel(id: LocalStepId): JsExp = JqId(labelId(id))
  @inline private def JqLabel(n: StepNode): JsExp = JqLabel(n.id)
  @inline private def JqStepText(id: LocalStepId): JsExp = JqId(textareaId(id))
  @inline private def JqStepText(n: StepNode): JsExp = JqStepText(n.id)

  /** Creates Javascript to update the indentation levels of all given nodes. */
  protected def jsUpdateIndentation(nodes: StepTree): JsCmd = JsCmds.Run(
    nodes.mapRecursive(n =>
      JqId(n.id) ~> JqJE.JqAttr(StepLevelAttribute, n.level.toString) toJsCmd
    ) mkString ";\n"
  )

  /** Creates Javascript to update the label text of all given nodes. */
  protected def jsUpdateLabels(nodes: StepTree): JsCmd = JsCmds.Run(
    nodes.mapRecursive(n =>
      JqLabel(n) ~> JqHtml(Text(labelFor(n))) toJsCmd
    ) mkString "\n"
  )

  def jsUpdateStepFieldText(id: LocalStepId): JsCmd =
    JqStepText(id) ~> JqSetTextarea(text(id))

  @inline private def jsShowNewStep(node: StepNode) =
    JqId(node.id) ~> JqHide ~> JqSlideDown(Fast).andThen(JqStepText(node) ~> JqFocus)

  def jsAddTailStep(node: StepNode): JsCmd = (
    JqExpr(cfg.tailStepCss) ~> JqBefore(renderSingleStepXml(node))
      & jsShowNewStep(node)
    )

  def jsAddStep(precedingNodeId: LocalStepId, node: StepNode): JsCmd = (
    JqId(precedingNodeId) ~> JqAfter(renderSingleStepXml(node))
      & jsShowNewStep(node)
      & jsUpdateLabels(f.value.tree)
    )

  def jsRemoveStep(node: StepNode): JsCmd =
    FadeOutThen(JqExprForNodeAndChildren(node), 240.ms)(_ ~> JqJE.JqRemove() & jsUpdateLabels(f.value.tree))

  def jsDecIndent(node: StepNode): JsCmd = {
    val newTree = f.value.tree
    val updateJs = jsUpdateIndentation(newTree) & jsUpdateLabels(newTree)
    cfg.customiseDecIndentJs(node, newTree, updateJs, jsFocusIfSpecified(node))
  }

  def jsIncIndent(node: StepNode, oldTree: StepTree): JsCmd = {
    val updateJs = jsUpdateIndentation(f.value.tree) & jsUpdateLabels(f.value.tree)
    cfg.customiseIncIndentJs(oldTree, node, updateJs, jsFocusIfSpecified(node))
  }

  @inline private def jsFocusIfSpecified(node: StepNode): JsCmd =
    S.param("focus") match {
    case Full(_) => JqStepText(node) ~> JqFocus
    case _ => Noop
  }

}