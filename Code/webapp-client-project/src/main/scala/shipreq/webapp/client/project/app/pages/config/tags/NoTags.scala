package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.ui.semantic.{Icon, Message}

object NoTags {

  val render =
    Message(
      Message.Style(Message.Type.Info),
      Icon.InfoCircle,
      "No tags",
      "Create new tags using the button above.")

}
