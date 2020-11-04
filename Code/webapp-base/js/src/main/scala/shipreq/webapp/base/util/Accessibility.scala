package shipreq.webapp.base.util

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data.Username

object Accessibility {

  def hiddenUsernameField(username: Username): VdomTag =
    <.input.text(
      ^.autoComplete.username,
      ^.display.none,
      ^.readOnly := true,
      ^.value := username.value)

}
