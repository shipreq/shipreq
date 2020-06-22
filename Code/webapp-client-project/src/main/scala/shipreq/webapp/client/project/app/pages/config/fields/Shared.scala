package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.ui.semantic.{ColourPlus, Icon, Message}
import shipreq.webapp.client.project.app.pages.root.Routes

object Shared {

  def noSourcesMsg(sourceName   : String,
                   fieldTypeName: String,
                   routerCtl    : Routes.RouterCtl,
                   page         : Routes.Page) =
    Message(
      style   = Message.Style(colour = ColourPlus.Negative),
      icon    = Icon.Ban,
      header  = s"No ${sourceName.toLowerCase}s available",
      content = TagMod(
        s"In order to create a new $fieldTypeName field, you need a ${sourceName.toLowerCase} that hasn't been assigned to a field yet. You can create one on the ",
        routerCtl.link(page),
        " page."))

}
