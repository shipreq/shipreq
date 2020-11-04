package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{Element, document, html}
import shipreq.base.util.{Allow, Deny, Disabled, Enabled, ErrorMsg, Permission}
import shipreq.webapp.base.config.GlobalSettings
import shipreq.webapp.base.data.{PlainTextPassword, Username}
import shipreq.webapp.base.lib.ModalForm
import shipreq.webapp.base.protocol.ajax.CommonProtocols.Login
import shipreq.webapp.base.protocol.ajax.{AjaxClient, CommonProtocols}
import shipreq.webapp.base.ui.semantic.{Colour, Icon, Label, UsesSemanticUiManually}
import shipreq.webapp.base.util.Accessibility

/** Pops up a modal that asks a user to re-authenticate.
  *
  * On success, this modal simply closes. It's expected that the login attempt will make an AJAX call that will update
  * the JWT on success.
  *
  * Usage:
  *
  * 1. Add `.render` to the root view.
  *    It will be hidden.
  *    It has reusability so as to only evaluate once.
  *
  * 2. Call `.run` to display the modal and collect a conclusion.
  */
final case class ReauthenticationModal(id    : String,
                                       render: VdomElement,
                                       run   : AsyncCallback[Permission])

@UsesSemanticUiManually
object ReauthenticationModal {

  type AttemptLogin = Login.Request => AsyncCallback[ErrorMsg \/ Permission]

  def apply(username: Username): ReauthenticationModal =
    apply(username, AjaxClient.Binary)

  def apply(username: Username, ajaxClient: AjaxClient.Binary): ReauthenticationModal = {
    val sspLogin = ajaxClient.invoker(CommonProtocols.Login.ajax)
    apply(username, sspLogin(_))
  }

  def apply(username: Username, attemptLogin: AttemptLogin): ReauthenticationModal =
    apply(username, attemptLogin, document.body, 280)

  private[ui] final val header =
    "Session Expired"

  def apply(username    : Username,
            attemptLogin: AttemptLogin,
            rootDom     : Element,
            delayMs     : Double): ReauthenticationModal = {

    import ModalForm.SetState

    val errorInvalidPassword = ErrorMsg("Invalid password.")
    val errorLabel           = Label.Style(Label.Type.PointingUp, Colour.Red).div

    object modalForm extends ModalForm[Permission]("ReauthModal", Deny, "Login", rootDom, extraModalClasses = "mini") {

      val passwordDom    = getDom[html.Input]("input[type=password]")
      val passwordGet    = passwordDom.map(i => PlainTextPassword(i.value))
      val loginButtonDom = getDom[html.Button](".button.primary")
      val errorLabelDom  = getDom[html.Div](".label")

      override def setState(s: SetState): Callback =
        for {
          pd <- passwordDom
          lb <- loginButtonDom
          el <- errorLabelDom
        } yield {
          for (d <- Option(pd)) {
            d.readOnly = s.form.is(Disabled)
          }
          for (d <- Option(el)) {
            d.style.display = if (s.error.isDefined) null else "none"
            d.innerHTML = s.error.fold("")(_.value)
          }
          GeneralTheme.nonReact.setStateOfSubmitButton(lb)(s.form, inFlight = s.inFlight)
        }

      override val clearFormData: Callback =
        passwordDom.map(_.value = "")

      override val header: TagMod =
        ReauthenticationModal.header

      override val content = TagMod(
        <.p("You must login again to be able to save changes or receive updates."),
        <.form(
          ^.cls := "ui left icon input",
          ^.display.flex,
          Accessibility.hiddenUsernameField(username),
          <.input.password(
            ^.autoComplete.currentPassword,
            ^.autoFocus := true,
            ^.onChange --> setState(SetState(Enabled, None, inFlight = false)),
            GeneralTheme.submitOnEnter(submit(None))
          ),
          Icon.Lock.tag
        ),
        errorLabel
      )

      override val justSubmit: AsyncCallback[SetState \/ Permission] =
        passwordGet.map(Login.Request.validate(-\/(username), _)).asAsyncCallback.flatMap {
          case \/-(req) =>
            attemptLogin(req).flatMap {
              case ok@ \/-(Allow) => GlobalSettings.SessionExpired.remove.asAsyncCallback.ret(ok)
              case \/-(Deny)      => AsyncCallback pure -\/(SetState(Enabled, Some(errorInvalidPassword), inFlight = false))
              case -\/(err)       => AsyncCallback pure -\/(SetState(Enabled, Some(err), inFlight = false))
            }
          case -\/(_) =>
            AsyncCallback.pure(-\/(SetState(Enabled, Some(errorInvalidPassword), inFlight = false))).delayMs(delayMs)
        }

      /** Check if re-authorisation has occurred in a different tab */
      lazy val checkBackgroundReauthorisation: Callback = {
        Callback.byName {
          if (isModalOpen()) {
            val sessionExpired = GlobalSettings.SessionExpired.get.runNow().contains(true)
            if (sessionExpired)
              checkBackgroundReauthorisation
            else
              complete(Allow)
          } else
            Callback.empty
        }.delayMs(1000).toCallback
      }

      override def run: AsyncCallback[Permission] =
        checkBackgroundReauthorisation.asAsyncCallback >> super.run
    }

    ReauthenticationModal(modalForm.id, modalForm.component(), modalForm.run)
  }
}
