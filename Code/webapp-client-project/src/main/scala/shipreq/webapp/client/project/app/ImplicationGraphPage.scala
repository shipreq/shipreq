package shipreq.webapp.client.project.app

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.FilterDead
import shipreq.webapp.client.base.lib.DataReusability._
import shipreq.webapp.client.base.ui.BaseStyles
import shipreq.webapp.client.project.widgets.FilterDeadButton
import shipreq.webapp.client.project.widgets.high.ImplicationGraph
import Style.{impgraphPage => *}

object ImplicationGraphPage {

  case class Props(graph: ImplicationGraph.Props, setFilterDead: FilterDead ~=> Callback) {
    @inline def render = Component(this)
  }

  def render(p: Props) =
    <.div(BaseStyles.fullWidthContainer,

      <.div(*.filterDeadButton,
        FilterDeadButton.Component(ReusableVar(p.graph.filterDead)(p.setFilterDead))),

      <.div(*.graph,
        p.graph.render))

  val Component = FunctionalComponent(render)
}
