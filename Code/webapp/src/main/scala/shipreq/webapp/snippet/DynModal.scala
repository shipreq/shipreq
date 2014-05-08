package shipreq.webapp.snippet

import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import shipreq.webapp.feature.validation.{ValidationResultT, Validators}
import shipreq.webapp.lib.{FormVar, StaticSnippetHelpers}
import shipreq.webapp.lib.Types._
import shipreq.webapp.security.PasswordAndSalt
import shipreq.webapp.util.HtmlTransformExt._
import shipreq.webapp.util.JsExt._
import shipreq.webapp.util.NonEmptyTemplate

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

    val curPasswordV = current.map(ps => FormVar.strOnSubmit(Validators.currentPassword(ps), "#dynmodal-passwordC")(""))
    val passwordV = FormVar.passwordPair("#dynmodal-password1", "#dynmodal-password2")

    def onSubmit(): JsCmd = {
      val vn = passwordV.validate
      val v: ValidationResultT[String] = curPasswordV match {
        case None     => vn
        case Some(fv) => Validators.Ap.apply2(fv.validate, vn)((_,n) => n)
      }
      ifValid(v)(newPassword =>
        JsModalHide & successFn(newPassword))
    }

    val currentPasswordTransform = curPasswordV.fold(".curpw" #> "")(_.csssel)

    run(ChangePasswordTemplate)(
      ".modal-title *" #> title
      & currentPasswordTransform
      & passwordV.csssel
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
