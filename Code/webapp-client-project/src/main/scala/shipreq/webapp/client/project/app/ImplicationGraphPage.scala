package shipreq.webapp.client.project.app

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.FilterDead
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.client.project.widgets.FilterDeadButton
import shipreq.webapp.client.project.widgets.ImplicationGraph
import Style.{impgraphPage => *}

object ImplicationGraphPage {

  final case class Props(graph: ImplicationGraph.Props, setFilterDead: Reusable[StateSnapshot.SetFn[FilterDead]]) {
    @inline def render = Component(this)
  }

  def render(p: Props) =
    <.div(BaseStyles.containerFull,

      <.div(*.filterDeadButton,
        FilterDeadButton.Component(StateSnapshot.withReuse(p.graph.filterDead)(p.setFilterDead))),

      <.div(*.graph,
        p.graph.render))

  val Component = ScalaFnComponent(render)
}
