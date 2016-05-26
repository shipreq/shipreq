package shipreq.webapp.client.home

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.protocol.InitDataForHomeSpa
import shipreq.webapp.client.home.ui.{ProjectItem, TopMenu}

object Home {

  type Props = InitDataForHomeSpa

  private def render(p: Props): ReactElement =
    <.div(
      TopMenu.Component(p.username),
      <.div(
        ^.cls := "ui container",
        ^.marginTop := "5rem",
        p.projects.items.map(ProjectItem.Component(_))))


  val Component = FunctionalComponent(render)
}
