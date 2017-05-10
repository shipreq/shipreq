package shipreq.webapp.client.base.ui.semantic

import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import shipreq.base.util.{Invalid, Validity}

object Input {
  val Base        = divCls("ui input")
  val Error       = Base(^.cls := "error")

  val Action      = Base(^.cls := "action")
  val ActionError = Action(^.cls := "error")

  def IconTextRight(icon: VdomTag, input: VdomTagOf[html.Input], right: TagMod, validity: Validity): VdomTag = {
    var r = Base(^.cls := "left icon right action", icon, input, right)
    if (validity is Invalid)
      r = r(^.cls := "error")
    r
  }

  def loadingDisabled(value: String, icon: Icon = Icon.Search) =
    Base(^.cls := "loading icon",
      <.input.text(^.value := value, ^.disabled := true),
      icon.tag)
}
