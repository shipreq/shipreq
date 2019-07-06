package shipreq.webapp.client.project.app.issues

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.ui.semantic.{Icon, Message}

object EmptyBody {

  @inline def render =
    Message(
      Message.Style(Message.Type.Info),
      Icon.Trophy,
      "No outstanding issues",
      "Congratulations!! Keep up the good work.")
}
