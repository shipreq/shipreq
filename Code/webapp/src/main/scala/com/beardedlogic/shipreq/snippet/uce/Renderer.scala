package shipreq.webapp
package snippet.uce

import scala.xml.{Text, NodeSeq}
import scalaz.{Memo, NonEmptyList}
import net.liftweb.common.Logger
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.jquery.JqJE
import net.liftweb.http.js.jquery.JqJsCmds.jsExpToJsCmd
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._

import feature.FlowGraph
import feature.uc.change._
import feature.uc.field._
import feature.publish.HtmlFieldValuePublishers
import feature.validation.VFailure
import lib.ScalazSubset._
import lib.Types._
import util.JsExt._
import Changes._
import Renderer._
import UseCaseEditor._

object Renderer {

  object Templates {
    import util.NonEmptyTemplate

    final val EntirePage = NonEmptyTemplate.load("uceditor")

    final val TextField = EntirePage.quickExtractById("template-text")
    final val Step = EntirePage.quickExtractById("template-step")
    final val AddTailStep = EntirePage.quickExtractById("template-courses-addTailStep")

    private def addStepTemplate = ".steps * " #> Templates.Step
    private def extractStepTemplate(name: String) = addStepTemplate(EntirePage.quickExtractById(name))
    final val NormalCourse = extractStepTemplate("template-courses-n")
    final val AlternateCourses = extractStepTemplate("template-courses-a")
    final val ExceptionCourses = extractStepTemplate("template-courses-e")
    final val FlowGraph = extractStepTemplate("template-flowgraph")
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

  final val FlowGraphTrigger = JsTextTrigger("flowgraph-upd")

  case class TextFieldUpdateMsg(taid: String, tav: String, pid: String, pv: NodeSeq)
  final val TextFieldUpdateTrigger = JsJsonTrigger[TextFieldUpdateMsg]("textfield-upd")

  // TODO use trigger for title update
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

  @inline final def textFieldPubId(f: TextField) = textFieldIds(f) + "-p"
  @inline final def renderTextFieldPub(f: TextField) = HtmlFieldValuePublishers.textField(f.value)

  // *************************************
  // *             Rendering             *
  // *************************************

  def render = {
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

  def renderField(ff: Field): NodeSeq = ff match {
    case f: TextField      => renderTextField(f)(Templates.TextField)
    case f: StepField      => stepRenderers(f).render
    case _: FlowGraphField => renderFlowGraphField()
  }

  def renderTextField(f: TextField) = (
    "th *" #> f.defn.title &
    ".fvpub [id]" #> textFieldPubId(f) &
    ".fvpub *" #> renderTextFieldPub(f) &
    "textarea" #> SHtml.ajaxTextarea(f.value.text, modTextField(f)(_), "id" -> textFieldIds(f))
  )

  val stepRenderers = Memo.immutableListMapMemo[StepField, StepFieldRenderer] {
    case f: NormalCourseField => StepFieldRenderer(f, NormalCourseFieldConfig, state, modifyUC)
    case f: ExceptionCourseField => StepFieldRenderer(f, ExceptionCourseFieldConfig, state, modifyUC)
  }

  def renderFlowGraphField(): NodeSeq = {
    S.appendGlobalJs(JsSetGlobalVar("InitialFlowGraph", flowGraphDot))
    Templates.FlowGraph
  }

  def flowGraphDot = FlowGraph.render(uc)

  // **************************************
  // *             Modifiers              *
  // **************************************

  def modTitle(input: String) =
    UcModifier(
      _.updateTitle(input),
      Some(_.jsUpdateTitle),
      Some(JqId(TitleId)))

  def modTextField(f: TextField)(input: String) =
    UcModifier(
      f.updateText(input),
      Some(_.jsUpdateTextField(f)),
      Some(JqId(textFieldIds(f))))

  // **************************************
  // *             Javascript             *
  // **************************************

  def jsRespondChangeFailure(f: VFailure): JsCmd =
    JsCmds.Alert(f.toText) // TODO can do better than this

  def jsRespondToChanges(changes: NonEmptyList[Change]): JsCmd =
    changes.foldMap(jsRespondToChange)    |+|
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
    case FlowGraphChanged                      => jsDrawFlowDiagram // TODO prevent dupls
    case FlowToChange(_,_)
       | FlowFromChange(_,_) => JsCmds.Noop
  }

  def jsDrawFlowDiagram: JsCmd =
    FlowGraphTrigger.trigger(flowGraphDot)

  def jsUpdateRevision: JsCmd =
    JqExpr(".save .rev") ~> JqJE.JqHtml(Text(state.currentRevision.toString))

  def jsEnableSaveButton(enable: Boolean): JsCmd =
    if (enable) SaveButtonEnableJs else SaveButtonDisableJs

  def jsUpdateTitle: JsCmd =
    JqId(TitleId) ~> JqSetTextarea(uch.title) &
    JqExpr(".cur-uc-title") ~> JqHtml(Text(uch.title)) &
    JsUpdatePageTitle

  def jsUpdateTextField(f: TextField): JsCmd =
    TextFieldUpdateTrigger.trigger(
      TextFieldUpdateMsg(
        textFieldIds(f), f.value.text,
        textFieldPubId(f), renderTextFieldPub(f)))

}