package shipreq.webapp.client.project.app.pages.content.reqgraph

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.event.ProjectAndOrd
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.ui.NoContentMessage
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.client.project.app.Style.{reqgraphPage => *}
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.app.pages.root.ProjectIndex
import shipreq.webapp.client.project.feature.SavedViewFeature
import shipreq.webapp.client.project.widgets.ImplicationGraph

object ReqGraphPage {

  final case class Props(projectAndOrd   : ProjectAndOrd,
                         plainText       : PlainText.ForProject.AnyCtx,
                         reqDetailRC     : RouterCtl[ExternalPubid],
                         webWorker       : WebWorkerClient.Instance,
                         savedViewFeature: SavedViewFeature) {
    @inline def project = projectAndOrd.project
    @inline def render: VdomElement = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {

    private val setConfigFn: Reusable[StateSnapshot.SetFn[ImpGraphConfig]] =
      Reusable.byRef((oc, cb) =>
        $.props.flatMap(_.savedViewFeature.static.modifyViewFn.modStateOption(
          s => oc.map(c => s.copy(impGraphConfig = Some(c))),
          cb)))

    def render(p: Props) = {
      val filterDead = p.savedViewFeature.filterDead

      val impGraphConfig: ImpGraphConfig =
        p.savedViewFeature.activeView.impGraphConfig.getOrElse(ImpGraphConfig.default)

      val impGraph = ImplicationGraph.Props.All(
        ord          = p.projectAndOrd.ord,
        reqWhitelist = p.savedViewFeature.reqWhitelist,
        filterDead   = filterDead,
        config       = impGraphConfig,
        project      = p.project,
        plainText    = p.plainText,
        reqDetailRC  = p.reqDetailRC,
        webWorker    = p.webWorker,
      )

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

      val config =
        ConfigEditor.Props(
          state         = StateSnapshot.withReuse(impGraphConfig)(setConfigFn),
          filterDead    = filterDead,
          projectConfig = p.project.config,
        ).render

      val content =
        if (impGraph.isEmpty)
          <.div(*.noContent, noContentMessage)
        else
          <.div(*.graph, impGraph.render)

      <.div(*.container,
        p.savedViewFeature.renderSavedViewsAndFilterDeadButton,
        <.div(*.controlsRow2,
          config,
          <.div(*.controlsFilter, p.savedViewFeature.renderFilterEditor)),
        content)
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .build
}
