package shipreq.webapp.client.public.pages

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data.{Disabled, Enabled}
import shipreq.webapp.base.lib.KeyHandler.Criterion
import shipreq.webapp.base.lib.{KeyHandlers, ValidationUX}
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon, Message, Size}
import shipreq.webapp.client.public.Styles.{common => *}

private[pages] object Common {

  def renderTokenInvalid: VdomElement =
    <.div(*.tokenInvalidCont,
      Message(
        Message.Style(Message.Type.Error),
        Icon.Warning,
        "Invalid token",
        "The link emailed to you is no longer valid."))

  def renderTokenExpired = renderTokenInvalid

  def submitButton(title: String, submitCB: Option[Callback]) =
    Button(
      state = Button.State.enabledWhen(submitCB.isDefined),
      colour = Colour.Blue,
      size = Size.Large).tag(
      *.submitButton,
      title,
      ^.onClick -->? submitCB)

  def submitOnEnter(submit: Callback): KeyHandlers =
    Criterion.Enter.handle(submit) + Criterion.CtrlEnter.handle(submit)

  def validationOffUntilFirstSubmit(formEnabled  : Enabled,
                                    currentVUX   : ValidationUX,
                                    changeVUX    : => Callback,
                                    submitIfValid: => Option[Callback]): Option[Callback] =
    if (formEnabled.is(Disabled))
      None
    else {
      val submit = submitIfValid
      (submit, currentVUX) match {
        case (valid@ Some(_), _)      => valid
        case (None, ValidationUX.Off) => Some(changeVUX)
        case (None, _)                => None
      }
    }

  // TODO Delete after upgrade to scalajs-react 1.1.0
  def a_toNewWindow(href: String,
                    noopener: Boolean = true,
                    noreferrer: Boolean = false) = {
    val a = <.a(^.target.blank, ^.href := href)
    (noopener, noreferrer) match {
      case (true, false) => a(^.rel := "noopener")
      case (true, true) => a(^.rel := "noopener noreferrer")
      case (false, true) => a(^.rel := "noreferrer")
      case (false, false) => a
    }
  }

}
