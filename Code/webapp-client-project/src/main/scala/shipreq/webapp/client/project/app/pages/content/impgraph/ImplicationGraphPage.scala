package shipreq.webapp.client.project.app.pages.content.impgraph

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.{FilterDead, HideDead, ShowDead}
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.ui.{BaseStyles, NoContentMessage}
import shipreq.webapp.client.project.app.Style.{impgraphPage => *}
import shipreq.webapp.client.project.widgets.FilterDeadButton
import shipreq.webapp.client.project.widgets.ImplicationGraph
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.client.project.app.pages.root.ProjectIndex

object ImplicationGraphPage {

  final case class Props(graph: ImplicationGraph.Props, setFilterDead: Reusable[StateSnapshot.SetFn[FilterDead]]) {
    @inline def render = Component(this)
  }

  def render(p: Props) = {

    val filterDeadButton =
      <.div(*.filterDeadButton,
        FilterDeadButton.Component(StateSnapshot.withReuse(p.graph.filterDead)(p.setFilterDead)))

    def noContentMessage =
      if (p.graph.filterDead.is(HideDead) && !p.graph.copy(filterDead = ShowDead).isEmpty)
        NoContentMessage.becauseAllDead(
          TagMod(
            "Enable display of dead content (via the ",
            Icon.TrashOutline.tag,
            "button in the top-right)."))
      else
        NoContentMessage(
          "You don't have any requirements yet.",
          s"Head over to the ${ProjectIndex.Item.ReqTable.title} page to get started.")

    val content =
      if (p.graph.isEmpty)
        <.div(*.noContent, noContentMessage)
      else
        <.div(*.graph, p.graph.render)

    <.div(BaseStyles.containerFull,
      filterDeadButton,
      content)
  }

  val Component = ScalaFnComponent(render)
}
