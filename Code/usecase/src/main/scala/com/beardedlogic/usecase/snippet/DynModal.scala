package com.beardedlogic.usecase.snippet

import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._
import com.beardedlogic.usecase.feature.validation.Validator
import com.beardedlogic.usecase.lib.StaticSnippetHelpers
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.util.HtmlTransformExt.ajaxSubmitOnClick
import com.beardedlogic.usecase.util.JsExt._
import com.beardedlogic.usecase.util.NonEmptyTemplate

/**
 * Generate dynamic modal dialogs.
 */
object DynModal extends StaticSnippetHelpers {

  val DialogTemplate = NonEmptyTemplate.load("templates-hidden/change_password_dialog").get

  implicit val innerNoticesCont: NoticeContainerExp = "#dynmodal-notices".tag
  implicit val innerErrorAlertId: ErrorAlertId = "d--e".tag

  object JqModalHide extends JsMethod {def toJsCmd: String = "modal('hide')"}
  val JqModal = JqId("dynmodal")
  val JsModalHide = JqModal ~> JqModalHide

  val Trigger = JsHtmlTrigger("dynmodal")

  /**
   * Opens a modal dialog that prompts the user to enter a new password.
   *
   * @param title The dialog title.
   * @param successFn Callback that reacts to a successful password submission.
   * @return
   */
  def passwordChanger(title: String, successFn: String @@ Validated => JsCmd): JsCmd = {
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
      & ":submit" #> ajaxSubmitOnClick(() => onSubmit())
    )
    val html = transform(DialogTemplate)

    Trigger.trigger(html)
  }
}
