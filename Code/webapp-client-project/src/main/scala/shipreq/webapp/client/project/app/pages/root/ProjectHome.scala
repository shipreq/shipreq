package shipreq.webapp.client.project.app.pages.root

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.ui.{BaseStyles, ProjectItem}
import shipreq.webapp.client.project.app.Style

object ProjectHome {

  final case class Props(item : ProjectItem.WithEditableName.Props,
                         index: ProjectIndex.Props) {
    @inline def render = Component(this)
  }

  private def render(p: Props): VdomElement =
    <.main(BaseStyles.containerLarge,
      <.section(Style.home.projectHeader, p.item.render),
      ProjectIndex.Component(p.index))

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .build
}
