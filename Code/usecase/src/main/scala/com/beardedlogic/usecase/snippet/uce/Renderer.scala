package com.beardedlogic.usecase
package snippet.uce

import scala.xml.{Text, NodeSeq}
import scalaz.{Memo, NonEmptyList}
import scalaz.syntax.foldable._
import scalaz.syntax.monoid._
import net.liftweb.common.Logger
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.jquery.JqJE
import net.liftweb.http.js.jquery.JqJsCmds.jsExpToJsCmd
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._

import feature.uc.change._
import feature.uc.field._
import feature.FlowGraph
import lib.Types._
import util.JsExt._
import Changes._
import Renderer._
import UseCaseEditor._

object Renderer {

  object Templates {
    import util.NonEmptyTemplate

    final val EntirePage = NonEmptyTemplate.load("loggedin/uceditor")

    final val TextField = EntirePage.quickExtract("template-text")
    final val Step = EntirePage.quickExtract("template-step")
    final val AddTailStep = EntirePage.quickExtract("template-courses-addTailStep")

    private def addStepTemplate = ".steps * " #> Templates.Step
    private def extractStepTemplate(name: String) = addStepTemplate(EntirePage.quickExtract(name))
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

  final val SaveButtonId = "save"
  final val SaveButtonEnableJs: JsCmd = JqId(SaveButtonId) ~> JqEnable
  final val SaveButtonDisableJs: JsCmd = JqId(SaveButtonId) ~> JqDisable

  final val FlowGraphTrigger = JsTextTrigger("flowgraph-update")

  case object JsUpdatePageTitle extends JsCmd {
    override def toJsCmd = "updatePageTitle()"
  }
}

case class Renderer(
  state: State,
  textFieldIds: Map[Field, LocalTextFieldId],
  modifyUC: UcModifier => JsCmd,
  saveUC: Option[() => JsCmd]
  ) extends RendererHelper with Logger {

  // *************************************
  // *             Rendering             *
  // *************************************

  def render = {
    S.appendGlobalJs(JsSetGlobalVar("InitialFlowGraph", flowGraph))
    ".fieldFrame *" #> renderFields andThen (
      ".title .ucid *" #> ucNumber.toString &
      ".title @title" #> SHtml.ajaxTextarea(uch.title, modTitle(_), "id" -> TitleId, "rows" -> "1", "class" -> "form-control input-lg") &
      renderSaveCont
    )
  }

  def renderSaveCont: CssSel = saveUC match {
    case None =>         ".save-row" #> ""
    case Some(saveFn) => ".save-row button" #> SHtml.ajaxButton("Save", saveFn, "id" -> SaveButtonId, "disabled" -> "disabled")
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
      & "textarea" #> SHtml.ajaxTextarea(f.value.text, modTextField(f)(_), "id" -> textFieldIds(f))
    )

  val stepRenderers = Memo.immutableListMapMemo[StepField, StepFieldRenderer] {
    case f: NormalCourseField => StepFieldRenderer(f, NormalCourseFieldConfig, state, modifyUC)
    case f: ExceptionCourseField => StepFieldRenderer(f, ExceptionCourseFieldConfig, state, modifyUC)
  }

  def flowGraph = FlowGraph.render(uc)

  // **************************************
  // *             Modifiers              *
  // **************************************

  def modTitle(input: String) =
    UcModifier(_.updateTitle(input),
      Some(_.jsUpdateTitle),
      Some(err => jsRespondChangeFailure(err) & JqId(TitleId) ~> JqFocus ~> JqSelect))

  def modTextField(f: TextField)(input: String) =
    UcModifier(f.updateText(input), Some(_.jsUpdateTextField(f)), None)

  // **************************************
  // *             Javascript             *
  // **************************************

  def jsRespondChangeFailure(errorMessage: String): JsCmd =
    JsCmds.Alert(errorMessage) // TODO can do better than this

  def jsRespondToChanges(changes: NonEmptyList[Change]): JsCmd =
    changes.foldMap(jsRespondToChange)    |+|
    jsRedrawFlowDiagram(changes)          |+|
    jsEnableSaveButton(state.saveEnabled)

  def jsRespondToChange(change: Change): JsCmd = change match {
    case TitleChanged(_, _)                    => jsUpdateTitle
    case TextChanged(f)                        => jsUpdateTextField(f)
    case StepTextChanged(f, id)                => stepRenderers(f).jsUpdateStepFieldText(id)
    case TailStepAdded(f, node)                => stepRenderers(f).jsAddTailStep(node)
    case StepAdded(f, precedingId, node)       => stepRenderers(f).jsAddStep(precedingId, node)
    case StepRemoved(f, node)                  => stepRenderers(f).jsRemoveStep(node)
    case StepIndentIncreased(f, node, oldTree) => stepRenderers(f).jsIncIndent(node, oldTree)
    case StepIndentDecreased(f, node, _)       => stepRenderers(f).jsDecIndent(node)
    case FlowToChange(_,_)
       | FlowFromChange(_,_) => JsCmds.Noop
  }

  def jsRedrawFlowDiagram(changes: NonEmptyList[Change]): JsCmd = {
    def matches(c: Change): Boolean = c match {
      case FlowFromChange(_, _)
         | FlowToChange(_, _)
         | TailStepAdded(_, _)
         | _: ExistingStepLabelsChanged => true
      case TitleChanged(_, _)
         | TextChanged(_)
         | StepTextChanged(_, _)        => false
    }
    if (matches(changes.head) || changes.tail.exists(matches))
      jsDrawFlowDiagram
    else
      JsCmds.Noop
  }

  def jsDrawFlowDiagram: JsCmd = FlowGraphTrigger.trigger(flowGraph)

  def jsUpdateRevision: JsCmd =
    JqExpr(".save .rev") ~> JqJE.JqHtml(Text(state.currentRevision.toString))

  def jsEnableSaveButton(enable: Boolean): JsCmd =
    if (enable) SaveButtonEnableJs else SaveButtonDisableJs

  def jsUpdateTitle: JsCmd =
    JqId(TitleId) ~> JqSetTextarea(uch.title) &
    JqExpr(".cur-uc-title") ~> JqHtml(Text(uch.title)) &
    JsUpdatePageTitle

  def jsUpdateTextField(f: TextField): JsCmd =
    JqId(textFieldIds(f)) ~> JqSetTextarea(f.value.text)
}