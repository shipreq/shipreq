package shipreq.webapp.client.public.pages

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data.{Disabled, Enabled}
import shipreq.webapp.base.lib.ValidationUX
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

  def submitButton(title: String, submitCB: Option[Callback]) = {

    // Call preventDefault so underlying form onSubmit handlers don't reload the page
    val onClick: ReactEvent => Callback =
      _.preventDefaultCB >> submitCB.getOrEmpty

    Button(
      state = Button.State.enabledWhen(submitCB.isDefined),
      colour = Colour.Blue,
      size = Size.Large,
    ).tag(
      *.submitButton,
      title,
      ^.onClick ==> onClick)
  }

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

}
