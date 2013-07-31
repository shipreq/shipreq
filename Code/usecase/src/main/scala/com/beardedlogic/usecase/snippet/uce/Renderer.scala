package com.beardedlogic.usecase
package snippet.uce

import scala.xml.{Text, NodeSeq}
import scalaz.{Memo, NonEmptyList}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.jquery.JqJE
import net.liftweb.http.js.jquery.JqJsCmds.jsExpToJsCmd
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import JsCmds.Noop

import lib.change._
import lib.field._
import lib.UcChangeSource
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
}

case class Renderer(uce: UseCaseEditor) extends RendererHelper {

  // *************************************
  // *             Rendering             *
  // *************************************


  def render = (
    ".fieldFrame *" #> renderFields andThen
      ".title .ucid *" #> uch.number.toString
        & ".rev *" #> state.currentRevision
        & ".title @title" #> SHtml.ajaxText(uch.title, i => %(_.updateTitle(i)), "id" -> TitleId)
        & ".saveUseCase" #> SHtml.ajaxButton("Save", daoCallback(uce.onSave))
    )

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


  // **************************************
  // *             Javascript             *
  // **************************************

  def jsRespondChangeFailure(errorMessage: String): JsCmd =
    JsCmds.Alert(errorMessage)

  def jsRespondToChanges(changes: NonEmptyList[(UcChangeSource, Change)]): JsCmd = {
    var js = Noop
    for (c <- changes.list) {
      c match {
        case (_,            TitleChanged(_, _))                 => js &= jsUpdateTitle
        case (f: TextField, TextChanged)                        => js &= jsUpdateTextField(f)
        case (f: StepField, StepTextChanged(id))                => js &= stepRenderers(f).jsUpdateStepFieldText(id)
        case (f: StepField, TailStepAdded(node))                => js &= stepRenderers(f).jsAddTailStep(node)
        case (f: StepField, StepAdded(precedingId, node))       => js &= stepRenderers(f).jsAddStep(precedingId, node)
        case (f: StepField, StepRemoved(node))                  => js &= stepRenderers(f).jsRemoveStep(node)
        case (f: StepField, StepIndentIncreased(node, oldTree)) => js &= stepRenderers(f).jsIncIndent(node, oldTree)
        case (f: StepField, StepIndentDecreased(node, _))       => js &= stepRenderers(f).jsDecIndent(node)
        case _ =>
      }
    }
    js
  }

  def jsUpdateRevision: JsCmd =
    JqExpr(".rev") ~> JqJE.JqHtml(Text(state.currentRevision))

  def jsUpdateTitle: JsCmd =
    JqId(TitleId) ~> JqSetValue(uch.title, false)

  def jsUpdateTextField(f: TextField): JsCmd =
    JqId(textFieldIds(f)) ~> JqSetValue(f.value.text, false)
}