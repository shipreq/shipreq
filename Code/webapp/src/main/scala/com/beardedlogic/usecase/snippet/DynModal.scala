package com.beardedlogic.usecase.snippet

import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import com.beardedlogic.usecase.feature.validation.Validator
import com.beardedlogic.usecase.lib.StaticSnippetHelpers
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.security.PasswordAndSalt
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

  private def run(template: NodeSeq)(transform: NodeSeq => NodeSeq): JsCmd =
    Trigger.trigger(transform(template))

  // -------------------------------------------------------------------------------------------------------------------

  val ChangePasswordTemplate = NonEmptyTemplate.load("templates-hidden/dynmodal-change_password").get

  /**
   * Opens a modal dialog that prompts the user to enter a new password.
   *
   * @param title The dialog title.
   * @param current If provided, the user must enter a "Current Password" which must unlock this argument in order for
   *                a password change to be permitted.
   * @param successFn Callback that reacts to a successful password submission.
   */
  def passwordChanger(title: String, current: Option[PasswordAndSalt])(successFn: String @@ Validated => JsCmd): JsCmd = {
    var passwordCInput = ""
    var password1Input = ""
    var password2Input = ""

    def onSubmit(): JsCmd = {
      val vn = Validator.passwords.correctAndValidate(password1Input, password2Input)
      val v: ValidationResult[String] = current match {
        case None     => vn
        case Some(ps) => Validator.Ap.apply2(Validator.currentPassword(ps).correctAndValidate(passwordCInput), vn)((_,n) => n)
      }
      ifValid(v)(newPassword =>
        JsModalHide & successFn(newPassword))
    }

    val currentPasswordTransform = current match {
      case None    => ".curpw" #> ""
      case Some(_) => "#dynmodal-passwordC" #> SHtml.onSubmit(passwordCInput = _)
    }
    run(ChangePasswordTemplate)(
      ".modal-title *" #> title
      & currentPasswordTransform
      & "#dynmodal-password1" #> SHtml.onSubmit(password1Input = _)
      & "#dynmodal-password2" #> SHtml.onSubmit(password2Input = _)
      & ":submit" #> ajaxSubmitOnClick(onSubmit)
    )
  }

  /** Typical use: invoked via onclick event + ajax, process a PasswordAndSalt on success. */
  def passwordChangerT(title: String, current: Option[PasswordAndSalt])(successFn: PasswordAndSalt => JsCmd) =
    ajaxOnClick(() =>
      passwordChanger(title, current)(newPassword =>
        successFn(PasswordAndSalt.createWithRandomSalt(newPassword))))

  // -------------------------------------------------------------------------------------------------------------------

  val ConfirmDangerTemplate = NonEmptyTemplate.load("templates-hidden/dynmodal-confirm_danger").get

  /**
   * Opens a modal dialog that prompts the user to confirm a dangerous operation.
   *
   * @param body The modal body.
   * @param footerButtonLabel If provided, the label that appears on the button in the footer that triggers the
   *                          dangerous operation. If None, then the entire footer will be omitted.
   * @param successFn Callback that reacts to successful confirmation.
   */
  def confirmDanger(dlgClass: String, title: Option[String], body: NodeSeq, footerButtonLabel: Option[String])(successFn: => JsCmd): JsCmd = {

    def onSubmit(): JsCmd = JsModalHide & successFn

    val titleTransform = title match {
      case None    => ".modal-header" #> ""
      case Some(t) => ".modal-title *" #> t
    }
    val footerTransform = footerButtonLabel match {
      case None    => ".modal-footer" #> ""
      case Some(l) => ".modal-footer .btn-danger *" #> l
    }

    run(ConfirmDangerTemplate)(
      ( "#dynmodal [class+]" #> dlgClass
        & titleTransform
        & ".modal-body *" #> body
        & footerTransform
      ) andThen ".btn-danger" #> ajaxOnClick(onSubmit)
    )
  }

  /** Typical use: invoked via onclick event + ajax. */
  def confirmDangerT(dlgClass: String, title: Option[String], body: NodeSeq, footerButtonLabel: Option[String])(successFn: => JsCmd) =
    ajaxOnClick(() => confirmDanger(dlgClass, title, body, footerButtonLabel)(successFn))
}
