package shipreq.webapp.client.base.ui.semantic

import japgolly.scalajs.react.vdom.html_<^._

object Input {
  val Base        = divCls("ui input")
  val Error       = Base(^.cls := "error")

  val Action      = Base(^.cls := "action")
  val ActionError = Action(^.cls := "error")

  def loadingDisabled(value: String, icon: Icon = Icon.Search) =
    Base(^.cls := "loading icon",
      <.input.text(^.value := value, ^.disabled := true),
      icon.tag)
}
