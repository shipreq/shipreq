package shipreq.webapp.client.public.pages

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util.{Disabled, Enabled}
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.ui.semantic.{Icon, Message}
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
