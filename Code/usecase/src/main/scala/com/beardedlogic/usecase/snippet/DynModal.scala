package com.beardedlogic.usecase.snippet

import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import com.beardedlogic.usecase.feature.validation.Validator
import com.beardedlogic.usecase.lib.StaticSnippetHelpers
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.util.HtmlTransformExt._
import com.beardedlogic.usecase.util.JsExt._
import com.beardedlogic.usecase.util.NonEmptyTemplate

/**
 * Generate dynamic modal dialogs.
 */
object DynModal extends StaticSnippetHelpers {

  implicit val innerNoticesCont: NoticeContainerExp = "#dynmodal-notices".tag
  implicit val innerErrorAlertId: ErrorAlertId = "d--e".tag

  object JqModalHide extends JsMethod {def toJsCmd: String = "modal('hide')"}
  val JqModal = JqId("dynmodal")
  val JsModalHide = JqModal ~> JqModalHide

  val Trigger = JsHtmlTrigger("dynmodal")

  // -------------------------------------------------------------------------------------------------------------------

  val ChangePasswordTemplate = NonEmptyTemplate.load("templates-hidden/dynmodal-change_password").get

  /**
   * Opens a modal dialog that prompts the user to enter a new password.
   *
   * @param title The dialog title.
   * @param successFn Callback that reacts to a successful password submission.
   */
  def passwordChanger(title: String)(successFn: String @@ Validated => JsCmd): JsCmd = {
    var password1Input = ""
    var password2Input = ""

    def onSubmit(): JsCmd = {
      val v = Validator.passwords.correctAndValidate(password1Input, password2Input)
      ifValid(v)(newPassword =>
        JsModalHide & successFn(newPassword))
    }

    val transform = (
      ".modal-title *" #> title
      & "#dynmodal-password1" #> SHtml.onSubmit(password1Input = _)
      & "#dynmodal-password2" #> SHtml.onSubmit(password2Input = _)
      & ":submit" #> ajaxSubmitOnClick(onSubmit)
    )
    val html = transform(ChangePasswordTemplate)

    Trigger.trigger(html)
  }

  // -------------------------------------------------------------------------------------------------------------------

  val ConfirmDangerTemplate = NonEmptyTemplate.load("templates-hidden/dynmodal-confirm_danger").get

  /**
   * Opens a modal dialog that prompts the user to confirm a dangerous operation.
   *
   * @param body The modal body.
   * @param buttonLabel The label that appears on the button to proceed with the dangerous operation.
   * @param successFn Callback that reacts to successful confirmation.
   */
  def confirmDanger(title: Option[String], body: NodeSeq, buttonLabel: String)(successFn: => JsCmd): JsCmd = {

    def onSubmit(): JsCmd = JsModalHide & successFn

    val titleTransform = title match {
      case None    => ".modal-header" #> ""
      case Some(t) => ".modal-title *" #> t
    }
    val transform = (
        titleTransform
        & ".modal-body *" #> body
        & ".btn-danger" #> ("* *" #> buttonLabel & ajaxOnClick(onSubmit))
      )
    val html = transform(ConfirmDangerTemplate)

    Trigger.trigger(html)
  }
}
