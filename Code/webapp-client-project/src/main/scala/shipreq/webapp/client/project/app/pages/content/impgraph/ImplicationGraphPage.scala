package shipreq.webapp.client.project.app.pages.content.impgraph

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.ui.{BaseStyles, NoContentMessage}
import shipreq.webapp.client.project.app.Style.{impgraphPage => *}
import shipreq.webapp.client.project.widgets.FilterDeadButton
import shipreq.webapp.client.project.widgets.ImplicationGraph
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.app.pages.root.ProjectIndex
import shipreq.webapp.client.project.feature.SavedViewFeature

object ImplicationGraphPage {

  final case class Props(imps            : Implications.BiDir,
                         reqs            : Requirements,
                         reqTypes        : ReqTypes,
                         plainText       : PlainText.ForProject.AnyCtx,
                         reqDetailRC     : RouterCtl[ExternalPubid],
                         webWorker       : WebWorkerClient,
                         savedViewFeature: SavedViewFeature,
                         setFilterDead   : Reusable[StateSnapshot.SetFn[FilterDead]]) {
    @inline def render: VdomElement = Component(this)
  }

  def render(p: Props) = {
    val filterDead = p.savedViewFeature.filterDead

    val impGraph = ImplicationGraph.Props.All(
      reqWhitelist = p.savedViewFeature.reqWhitelist,
      config       = ImpGraphConfig.default,
      imps         = p.imps,
      reqs         = p.reqs,
      reqTypes     = p.reqTypes,
      plainText    = p.plainText,
      reqDetailRC  = p.reqDetailRC,
      webWorker    = p.webWorker,
    )

    val filterDeadButton =
      <.div(*.filterDeadButton,
        FilterDeadButton.Component(StateSnapshot.withReuse(filterDead)(p.setFilterDead)))

    def noContentMessage =
      if (filterDead.is(HideDead) && !impGraph.copy(reqWhitelist = p.savedViewFeature.pxReqWhitelistIgnoringFilterDead).isEmpty)
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
      if (impGraph.isEmpty)
        <.div(*.noContent, noContentMessage)
      else
        <.div(*.graph, impGraph.render)

    <.div(BaseStyles.containerFull,
      filterDeadButton,
      p.savedViewFeature.renderSavedViewManager,
      p.savedViewFeature.renderFilterEditor,
      content)
  }

  val Component = ScalaFnComponent(render)
}
