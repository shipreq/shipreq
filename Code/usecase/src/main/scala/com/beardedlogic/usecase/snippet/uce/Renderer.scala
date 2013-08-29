package com.beardedlogic.usecase
package snippet.uce

import scala.xml.{Text, NodeSeq}
import scalaz.{Memo, NonEmptyList}
import scalaz.syntax.foldable._
import scalaz.syntax.monoid._
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.jquery.JqJE
import net.liftweb.http.js.jquery.JqJsCmds.jsExpToJsCmd
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Helpers._

import lib.change._
import lib.field._
import lib.Types.JsCmdMonoid
import lib.{FlowGraph, UcChangeDomain}
import util.JsExt._
import Changes._
import Renderer._

object Renderer {

  object Templates {
    import util.TemplateCache._

    final val EntirePage = LoadTemplate(List("uce"))

    final val TextField = EntirePage.extract("template-text")
    final val Step = EntirePage.extract("template-step")
    final val AddTailStep = EntirePage.extract("template-courses-addTailStep")

    private def addStepTemplate = ".steps * " #> Templates.Step
    private def extractStepTemplate(name: String) = addStepTemplate(EntirePage.extract(name))
    final val NormalCourse = extractStepTemplate("template-courses-n")
    final val AlternateCourses = extractStepTemplate("template-courses-a")
    final val ExceptionCourses = extractStepTemplate("template-courses-e")
  }

  final val AddTailStepClass = "addTailStep"
  final val AlternateCourseAddTailStepCss = s".courses-a .$AddTailStepClass"
  final val ExceptionCourseAddTailStepCss = s".courses-e .$AddTailStepClass"

  final val StepLevelAttribute = "data-lvl"
  final val StepLevelAttributeCss = s".step [$StepLevelAttribute]"

  final val TitleId = "uc-title"

  final val FlowGraphTrigger = JsTextTrigger("flowgraph-update")
}

case class Renderer(uce: UseCaseEditor) extends RendererHelper {

  // *************************************
  // *             Rendering             *
  // *************************************

  def render = {
    S.appendGlobalJs(JsSetGlobalVar("InitialFlowGraph", flowGraph))
    ".fieldFrame *" #> renderFields andThen (
      ".title .ucid *" #> uch.number.toString
        & ".rev *" #> state.currentRevision
        & ".title @title" #> SHtml.ajaxTextarea(uch.title, i => %(_.updateTitle(i)), "id" -> TitleId, "rows" -> "1")
        & ".saveUseCase" #> SHtml.ajaxButton("Save", daoCallback(uce.onSave))
      )
  }

  def renderFields: NodeSeq =
    (NodeSeq.Empty /: fields.map(renderField))(_ ++: _)

  def renderField(f: Field): NodeSeq = f match {
    case tf: TextField => renderTextField(tf)(Templates.TextField)
    case sf: StepField => stepRenderers(sf).render
    case _ => NodeSeq.Empty
  }

  def renderTextField(f: TextField) = (
    "th *" #> f.defn.title
      & "textarea" #> SHtml.ajaxTextarea(f.value.text, i => %(f.updateText(i)), "id" -> textFieldIds(f))
    )

  val stepRenderers = Memo.immutableListMapMemo[StepField, StepFieldRenderer] {
    case f: NormalCourseField => StepFieldRenderer(uce, f, NormalCourseFieldConfig)
    case f: ExceptionCourseField => StepFieldRenderer(uce, f, ExceptionCourseFieldConfig)
  }

  def flowGraph = FlowGraph.render(uc)

  // **************************************
  // *             Javascript             *
  // **************************************

  def jsRespondChangeFailure(errorMessage: String): JsCmd =
    JsCmds.Alert(errorMessage)

  def jsRespondToChanges(changes: NonEmptyList[(UcChangeDomain, Change)]): JsCmd =
    changes.foldMap(jsRespondToChange) |+| jsRedrawFlowDiagram(changes)

  def jsRespondToChange(change: (UcChangeDomain, Change)): JsCmd = change match {
    case (_,            TitleChanged(_, _))                 => jsUpdateTitle
    case (f: TextField, TextChanged)                        => jsUpdateTextField(f)
    case (f: StepField, StepTextChanged(id))                => stepRenderers(f).jsUpdateStepFieldText(id)
    case (f: StepField, TailStepAdded(node))                => stepRenderers(f).jsAddTailStep(node)
    case (f: StepField, StepAdded(precedingId, node))       => stepRenderers(f).jsAddStep(precedingId, node)
    case (f: StepField, StepRemoved(node))                  => stepRenderers(f).jsRemoveStep(node)
    case (f: StepField, StepIndentIncreased(node, oldTree)) => stepRenderers(f).jsIncIndent(node, oldTree)
    case (f: StepField, StepIndentDecreased(node, _))       => stepRenderers(f).jsDecIndent(node)
    case _                                                  => JsCmds.Noop
  }

  def jsRedrawFlowDiagram(changes: NonEmptyList[(UcChangeDomain, Change)]): JsCmd = {
    def matches(t: (UcChangeDomain, Change)): Boolean = t._2 match {
      case FlowFromChange(_, _)         => true
      case FlowToChange(_, _)           => true
      case TailStepAdded(_)             => true
      case _: ExistingStepLabelsChanged => true
      case TitleChanged(_, _)           => false
      case TextChanged                  => false
      case StepTextChanged(_)           => false
    }
    if (matches(changes.head) || changes.tail.exists(matches)) jsDrawFlowDiagram
    else JsCmds.Noop
  }

  def jsDrawFlowDiagram: JsCmd = FlowGraphTrigger.trigger(flowGraph)

  def jsUpdateRevision: JsCmd =
    JqExpr(".rev") ~> JqJE.JqHtml(Text(state.currentRevision))

  def jsUpdateTitle: JsCmd =
    JqId(TitleId) ~> JqSetTextarea(uch.title)

  def jsUpdateTextField(f: TextField): JsCmd =
    JqId(textFieldIds(f)) ~> JqSetTextarea(f.value.text)
}