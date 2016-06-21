package shipreq.webapp.client.project.app.root

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.base.util.{Allow, Intersection}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{FilterDead, ReqId}
import shipreq.webapp.base.protocol.InitDataForProjectSpa
import shipreq.webapp.base.text.{PlainText, ProjectText, TextSearch}
import shipreq.webapp.client.base.feature._
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.project.app.state._
import shipreq.webapp.client.project.app.{cfg, reqdetail, reqtable, WebWorkerClient}
import shipreq.webapp.client.project.app.reqdetail.ReqDetail
import shipreq.webapp.client.project.app.reqtable.ReqTable
import shipreq.webapp.client.project.app.cfg.shared.Usage
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.high.{ImplicationGraph, ProjectWidgets}
import Routes.{Page, RouterCtl}
import LoadedRoot._

object LoadedRoot {
  case class Props(page: Page, routerCtl: RouterCtl)
}

final class LoadedRoot(val initData: InitDataForProjectSpa, cp: ClientProtocol, cd: ClientData) {

  final class Backend($: BackendScope[Props, State]) extends OnUnmount {
    import cd.pxProject

    // This never changes
    val routerCtl = $.props.runNow().routerCtl
    val reqDetailRC = routerCtl.contramap(Page.ReqDetail.apply)

    val setFilterDead: FilterDead ~=> Callback =
      ReusableFn(fd => $.modState(
        State.filterDead.set(fd) compose
        State.reqTableVS.modify(_ setFilterDead fd)))

    val pxPlainText      = pxProject.map(PlainText(_, ProjectText.Context.None))
    val pxTextSearch     = Px.apply2(pxProject, pxPlainText)(TextSearch.apply)
    val pxProjectWidgets = Px.apply2(pxProject, pxPlainText)(ProjectWidgets(_, _, reqDetailRC))

    val asyncFeature: AsyncActionFeature.D2.Feature[reqtable.Row.SourceId, AsyncKey, String] =
      AsyncActionFeature.D2.Feature($ zoomL State.asyncStates)

    val previewFeature = new PreviewFeature($, State.previewState)

    def initReqTableEditor: ReqTable.InitEditor = {
      import ContentEditorFeature._
      new D2.InitChild[reqtable.Row, reqtable.Column, reqtable.FocusId] {
        override type Parent    = State
        override val parent     = $: CompState.Access[Parent]
        override val preview    = previewFeature.mapK(FocusId.ToReqTable)
        override val editorLens =
          (r: reqtable.Row, c: reqtable.Column) =>
            reqtable.Column.EditFieldKeyIntersection.getOption(c).map(efk =>
              State.editStates ^|-> D2.State.at(r.sourceId) ^|-> D1.State.at(efk))
      }
    }

    val reqTable = ReqTable(ReqTable.StaticProps(
      cd, cp, initData.createContent, initData.updateContent,
      pxPlainText, pxTextSearch, pxProjectWidgets,
      initReqTableEditor,
      asyncFeature.mapK1(AsyncKey.ToReqTable),
      reqDetailRC,
      $ zoomL State.reqTable))

    val pxReqDetailId = Px(None: Option[ReqId])

    val pxReqDetailReqProps: Px[Option[State => ReqDetail.ReqProps]] =
      pxReqDetailId.map(_.map { id =>
        val r = reqtable.Row.ReqRowSourceId(id)

        val focusIdToCell = Intersection[FocusId, reqdetail.Cell] {
          case FocusId.Content(rs, f) =>
            if (r ==* rs)
              reqdetail.Cell.EditFieldKeyIntersection.reverse.getOption(f)
            else
              None
          case FocusId.ReqTableCI(_) => None
        }(c => reqdetail.Cell.EditFieldKeyIntersection.getOptionMap(c, FocusId.Content(r, _)))

        import ContentEditorFeature._
        val initEditor =
          new D1.InitChild[reqdetail.Cell, reqdetail.Cell] {
            override type Parent    = State
            override val parent     = $: CompState.Access[Parent]
            override val preview    = previewFeature.mapK(focusIdToCell)
            override val editorLens =
              (c: reqdetail.Cell) =>
                reqdetail.Cell.EditFieldKeyIntersection.getOption(c).map(efk =>
                  State.editStates ^|-> D2.State.at(r) ^|-> D1.State.at(efk))
          }

        val asyncF1 = asyncFeature(r).mapK(AsyncKey.ToReqDetail)

        (s: State) =>
          ReqDetail.ReqProps(
            initEditor,
            asyncF1,
            s.editStates(r).mapK(reqdetail.Cell.EditFieldKeyIntersection.reverse),
            s.asyncStates(r).mapK(AsyncKey.ToReqDetail))
      })

    def reqDetailReqPropsFn(s: State) = (id: ReqId) => {
      pxReqDetailId.set(Some(id))
      pxReqDetailReqProps.value().get(s)
    }

    def ww = WebWorkerClient.Instance

    val reqDetail = ReqDetail(ReqDetail.StaticProps(
      cd, cp, reqDetailRC, ww, initData.updateContent,
      pxPlainText, pxTextSearch, pxProjectWidgets))

    val reqDetailSetState: ReqDetail.State ~=> Callback =
      ReusableFn($.zoomL(State.reqDetail) setState _)

    val usageShow =
      Usage.Show((fd, fs) =>
        routerCtl
          .onSet($.modState(State.reqTable.modify(_.setFilterDead(fd).setFilterSpec(fs()))) >> _)
          .link(Page.ReqTable))

    def render(p: Props, s: State): ReactElement = {
      def fd = ReusableVar(s.filterDead)(setFilterDead)

      def layout(content: ReactElement, backPage: Page = Page.Index) =
        <.div(
          <.div(
            ^.textAlign.right,
            ^.paddingRight := "0.6ex",
            ^.marginTop := "-14px",
            routerCtl.link(backPage)("← Back")),
          content)

      p.page match {

        case Page.Index =>
          val rl = ReqLookupPrompt.Props(
            ExternalVar(s.reqLookup)($.zoomL(State.reqLookup) setState _),
            Allow <~ _.lookup(cd.project()).isRight,
            e => routerCtl.set(Page.ReqDetail(e)))

          ProjectIndex.Props(rl, cd.projectSummary(), routerCtl).render

        case Page.CfgFields =>
          layout(cfg.fields.CfgFields.Props(cp, initData.fieldCrud, cd, fd).component)

        case Page.CfgIssues =>
          layout(cfg.issues.CfgIssues.Props(cp, initData.issueTypeCrud, initData.reqTypeImpMod, initData.fieldMandMod, cd, fd, usageShow).component)

        case Page.CfgReqTypes =>
          layout(cfg.reqtypes.CfgReqTypes.Props(cp, initData.reqTypeCrud, cd, fd, usageShow).component)

        case Page.CfgTags =>
          layout(cfg.tags.CfgTags.Props(cp, initData.tagCrud, cd, fd).component)

        case Page.ReqTable =>
          layout(reqTable(ReqTable.DynamicProps(
            s.editStates.mapK1(reqtable.Column.EditFieldKeyIntersection.reverse),
            s.asyncStates.mapK1(AsyncKey.ToReqTable),
            s.previewState.mapK(FocusId.ToReqTable),
            s.reqTable)))

        case Page.ReqDetail(pubid) =>
          val props = ReqDetail.DynamicProps(
            pubid,
            fd,
            reqDetailReqPropsFn(s),
            ReusableVar(s.reqDetail)(reqDetailSetState))
          layout(reqDetail(props), Page.ReqTable)

        case Page.ImpGraph =>
          val p = cd.project()
          val ig = ImplicationGraph.Props(
            None, s.filterDead,
            p.implications, p.reqs, p.config.reqTypes,
            pxPlainText.value(),
            reqDetailRC,
            ww)
          layout(ig.render)
      }
    }

    def onProjectChange(c: Changes): Callback =
      $.modState(State.reqTable.modify(_ updateProject c.p2))
  }

  val Component = ReactComponentB[Props]("LoadedRoot")
    .initialState(State.init(cd))
    .renderBackend[Backend]
    .configure(Listenable.install(_ => cd, _.backend.onProjectChange))
    .build
}
