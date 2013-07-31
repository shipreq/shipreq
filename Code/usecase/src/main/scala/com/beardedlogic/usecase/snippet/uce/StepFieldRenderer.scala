package com.beardedlogic.usecase
package snippet.uce

import scala.xml.{Text, NodeSeq}
import net.liftweb.http.js.JsExp.strToJsExp
import net.liftweb.http.js.{JsExp, JE, JsCmd, JsCmds}
import net.liftweb.http.js.jquery.JqJE
import net.liftweb.http.js.jquery.JqJsCmds.jsExpToJsCmd
import net.liftweb.http.SHtml
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._

import com.beardedlogic.usecase.lib.tree.TreeLike
import com.beardedlogic.usecase.lib._
import util.HtmlTransformExt._
import util.JsExt._
import lib.field._
import lib.Types._
import Renderer._

object StepFieldRenderer {
  @inline final def ExprForNodeAndChildren(n: StepNode) = n.mapRecursive("#" + _.id).mkString(",")
  @inline final def JqExprForNodeAndChildren(n: StepNode) = JqExpr(ExprForNodeAndChildren(n))

  /** ID of the HTML attribute that contains a step's label (eg. "1.0.4.b") */
  @inline final def labelId(id: LocalIdStr): String = (id: String) + "-l"

  /** ID of the textarea that contains a step's text. */
  @inline final def textareaId(id: LocalIdStr): String = (id: String) + "-t"
}

import StepFieldRenderer._

// -------------------------------------------------------------------------------------------------------------------

trait StepFieldRenderConfig {

  /** A CSS expression that selects the container of the add-tail-step button. */
  def tailStepCss: String

  def prohibitRemoval_?(id: LocalIdStr, tree: StepTree): Boolean

  def render(r: StepFieldRenderer): NodeSeq

  /** Allows customisation of the ajax response of a successful indent decrease. */
  def customiseDecIndentJs(node: StepNode, newTree: StepTree, js: JsCmd): JsCmd

  /** Allows customisation of the ajax response of a successful indent increase. */
  def customiseIncIndentJs(oldTree: StepTree, node: StepNode, js: JsCmd): JsCmd
}

// -------------------------------------------------------------------------------------------------------------------

object NormalCourseFieldConfig extends StepFieldRenderConfig {

  override def tailStepCss = AlternateCourseAddTailStepCss

  override def prohibitRemoval_?(id: LocalIdStr, tree: StepTree) = (id == tree.head.id)

  override def render(r: StepFieldRenderer) =
    r.renderSteps(r.tree.head)(Templates.NormalCourse) ++
      r.renderStepsWithAddTailStep(r.tree.tailAsTreeLike)(Templates.AlternateCourses)

  override def customiseDecIndentJs(node: StepNode, newTree: StepTree, js: JsCmd) =
    newTree.nodes match {
      // Move steps from NC to AC
      case nc :: ac1 :: acN if ac1.id == node.id =>
        JsCmds.Run(s"nc_to_ac('#uce','${node.id}',${JE.AnonFunc(js).toJsCmd})")

      // Apply indent decrease normally
      case _ => js
    }

  override def customiseIncIndentJs(oldTree: StepTree, node: StepNode, js: JsCmd) =
    oldTree.nodes match {
      // Move steps from AC to NC
      case nc :: ac1 :: acN if ac1.id == node.id =>
        JsCmds.Run(s"ac_to_nc('#uce','${ExprForNodeAndChildren(node)}',${JE.AnonFunc(js).toJsCmd})")

      // Apply indent normally
      case _ => js
    }
}

// -------------------------------------------------------------------------------------------------------------------

object ExceptionCourseFieldConfig extends StepFieldRenderConfig {

  override def tailStepCss = ExceptionCourseAddTailStepCss

  override def prohibitRemoval_?(id: LocalIdStr, tree: StepTree) = false

  override def render(r: StepFieldRenderer) =
    r.renderStepsWithAddTailStep(r.tree)(Templates.ExceptionCourses)

  override def customiseDecIndentJs(node: StepNode, newTree: StepTree, js: JsCmd) = js
  override def customiseIncIndentJs(oldTree: StepTree, node: StepNode, js: JsCmd) = js
}

// -------------------------------------------------------------------------------------------------------------------

case class StepFieldRenderer(
  uce: UseCaseEditor,
  f: StepField,
  cfg: StepFieldRenderConfig
  ) extends RendererHelper {

  val rootLabelPrefix = f.rootLabelPrefix(uch)
  val tree = f.value.tree

  @inline final def labelPrefixForLevel(level: Int) = if (level == 0) rootLabelPrefix else ""
  @inline final def labelFor(node: StepNode) = labelPrefixForLevel(node.level) + node.label

  /** The text value of a step. */
  @inline final def text(id: LocalIdStr): String = f.value.textmap.get(id).map(_.text).getOrElse("")

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
    val t = "button" #> SHtml.ajaxButton("+", =>%(f.addTailStep))
    val addTailStepTmpl = t(Templates.AddTailStep)
    renderSteps(steps) andThen ".steps *+" #> addTailStepTmpl // Append to .steps, after all the .step tags
  }

  /**
   * Provides the transformation to render a single step. (Does not render step children.)
   */
  def renderSingleStep(n: StepNode) = {
    val id = n.id
    @inline def =>%%(fn: LocalIdStr => UseCase => UcUpdateResult) = =>%(fn(id))
    (
      ".step [id]" #> id
        & StepLevelAttributeCss #> n.level
        & IfCssSel(cfg.prohibitRemoval_?(id, tree)) {".step [class+]" #> "noDel"}
        & ".lbl span *" #> labelFor(n)
        & ".lbl span [id]" #> labelId(id)
        & "@text" #> SHtml.ajaxTextarea(text(id), i => %(f.updateText(id, i)), "id" -> textareaId(id))
        & ".add" #> SHtml.ajaxButton("+", =>%%(f.addStep))
        & ".delete" #> SHtml.ajaxButton("-", =>%%(f.removeStep))
        & ".indentDec" #> SHtml.ajaxButton("«", =>%%(f.decreaseIndent))
        & ".indentInc" #> SHtml.ajaxButton(<span>»</span>, =>%%(f.increaseIndent))
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
  // *             Javascript             *
  // **************************************

  @inline private def JqLabel(id: LocalIdStr): JsExp = JqId(labelId(id))
  @inline private def JqLabel(n: StepNode): JsExp = JqLabel(n.id)
  @inline private def JqStepText(id: LocalIdStr): JsExp = JqId(textareaId(id))
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

  def jsUpdateStepFieldText(id: LocalIdStr): JsCmd =
    JqStepText(id) ~> JqSetValue(text(id), false)

  @inline private def jsShowNewStep(node: StepNode) =
    JqId(node.id) ~> JqHide ~> JqSlideDown(Fast).andThen(JqStepText(node) ~> JqFocus)

  def jsAddTailStep(node: StepNode): JsCmd = (
    JqExpr(cfg.tailStepCss) ~> JqBefore(renderSingleStepXml(node))
      & jsShowNewStep(node)
    )

  def jsAddStep(precedingNodeId: LocalIdStr, node: StepNode): JsCmd = (
    JqId(precedingNodeId) ~> JqAfter(renderSingleStepXml(node))
      & jsShowNewStep(node)
      & jsUpdateLabels(f.value.tree)
    )

  def jsRemoveStep(node: StepNode): JsCmd =
    FadeOutThen(JqExprForNodeAndChildren(node), 240.ms)(_ ~> JqJE.JqRemove() & jsUpdateLabels(f.value.tree))

  def jsDecIndent(node: StepNode): JsCmd = {
    val newTree = f.value.tree
    val js = jsUpdateIndentation(newTree) & jsUpdateLabels(newTree)
    cfg.customiseDecIndentJs(node, newTree, js)
  }

  def jsIncIndent(node: StepNode, oldTree: StepTree): JsCmd = {
    val js = jsUpdateIndentation(f.value.tree) & jsUpdateLabels(f.value.tree)
    cfg.customiseIncIndentJs(oldTree, node, js)
  }

}