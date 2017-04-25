package shipreq.webapp.client.project.app.root

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.Implicits._
import japgolly.scalajs.react.vdom.VdomElement
import shipreq.base.util.{Allow, Intersection}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{FilterDead, ReqId}
import shipreq.webapp.base.protocol.InitDataForProjectSpa
import shipreq.webapp.base.text.{PlainText, ProjectText, TextSearch}
import shipreq.webapp.client.base.feature._
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.base.ui.ProjectItem
import shipreq.webapp.client.project.app.state._
import shipreq.webapp.client.project.app._
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

final class LoadedRoot(initData: InitDataForProjectSpa, cp: ClientProtocol, cd: ClientData) {

  final class Backend($: BackendScope[Props, State]) extends OnUnmount {
    import cd.pxProject

    // This never changes
    val routerCtl = $.props.runNow().routerCtl
    val reqDetailRC = routerCtl.contramap(Page.ReqDetail.apply)

    val setFilterDead: FilterDead ~=> Callback =
      Reusable.fn(fd => $.modState(
        State.filterDead.set(fd) compose
        State.reqTableVS.modify(_ setFilterDead fd)))

    val pxPlainText      = pxProject.map(PlainText(_, ProjectText.Context.None))
    val pxTextSearch     = Px.apply2(pxProject, pxPlainText)(TextSearch.apply)
    val pxProjectWidgets = Px.apply2(pxProject, pxPlainText)(ProjectWidgets(_, _, reqDetailRC))

    val asyncFeature: AsyncActionFeature.D2.Feature[reqtable.Row.SourceId, AsyncKey, String] =
      AsyncActionFeature.D2.Feature($ zoomStateL State.asyncStates)

    val previewFeature: PreviewFeature.Feature.Composite[FocusId] =
      PreviewFeature.Feature.Composite.init($ zoomStateL State.previewState)

    def initReqTableEditor: ReqTable.InitEditor = {
      import ContentEditorFeature._
      new D2.InitChild[reqtable.Row, reqtable.Column, reqtable.FocusId] {
        override type Parent    = State
        override val parent     = $
        override val preview    = previewFeature.mapId(FocusId.ToReqTable)
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
      asyncFeature.mapKey1(AsyncKey.ToReqTable),
      asyncFeature.mapKey1(AsyncKey.ToReqTable2),
      reqDetailRC,
      $ zoomStateL State.reqTable))

    val pxReqDetailId = Px[Option[ReqId]](None).withReuse.manualUpdate

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
            override val parent     = $
            override val preview    = previewFeature.mapId(focusIdToCell)
            override val editorLens =
              (c: reqdetail.Cell) =>
                reqdetail.Cell.EditFieldKeyIntersection.getOption(c).map(efk =>
                  State.editStates ^|-> D2.State.at(r) ^|-> D1.State.at(efk))
          }

        val asyncF1 = asyncFeature(r).mapKey(AsyncKey.ToReqDetail)

        (s: State) =>
          ReqDetail.ReqProps(
            initEditor,
            asyncF1,
            s.editStates(r).mapKey(reqdetail.Cell.EditFieldKeyIntersection.reverse),
            s.asyncStates(r).mapKey(AsyncKey.ToReqDetail))
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
      Reusable.fn.state($ zoomStateL State.reqDetail).set

    val usageShow =
      Usage.Show((fd, fs) =>
        routerCtl
          .onSet($.modState(State.reqTable.modify(_.setFilterDead(fd).setFilterSpec(fs()))) >> _)
          .link(Page.ReqTable))

    val projectNameAAF =
      AsyncActionFeature.D0.Feature.fn[String](s =>
        $.modState(State.projectName.modify(
          ProjectItem.WithEditableName.State setAsync s)))

    val setProjectNameIO: String => Callback =
      newName => {
        def close = $.modState(State.projectName set None)
        def save = projectNameAAF((onSuccess, onFailure) =>
          cp.call(initData.projectNameSet)(
            newName,
            cd.applyEventsS(_) >> onSuccess >> close,
            _ consumeAnd onFailure))
        cd.projectCB >>= (p => if (p.name ==* newName) close else save)
      }

    def render(p: Props, s: State): VdomElement = {
      def fd = StateSnapshot.withReuse(s.filterDead)(setFilterDead)

      val content: VdomElement = p.page match {

        case Page.Index =>
          val lookup = ReqLookupPrompt.Props(
            StateSnapshot.zoomL(State.reqLookup)(s).setStateVia($),
            Allow when _.lookup(cd.project()).isRight,
            e => routerCtl.set(Page.ReqDetail(e)))

          val index = ProjectIndex.Props(lookup, routerCtl)

          val pname = ProjectItem.WithEditableName.Props(
            cd.projectSummary(),
            StateSnapshot.zoomL(State.projectName)(s).setStateVia($),
            setProjectNameIO)

          ProjectHome.Props(pname, index).render

        case Page.CfgFields =>
          cfg.fields.CfgFields.Props(cp, initData.fieldCrud, cd, fd).component

        case Page.CfgIssues =>
          cfg.issues.CfgIssues.Props(
            cp, initData.issueTypeCrud, initData.reqTypeImpMod, initData.fieldMandMod, cd, fd, usageShow)
            .component

        case Page.CfgReqTypes =>
          cfg.reqtypes.CfgReqTypes.Props(cp, initData.reqTypeCrud, cd, fd, usageShow).component

        case Page.CfgTags =>
          cfg.tags.CfgTags.Props(cp, initData.tagCrud, cd, fd).component

        case Page.ReqTable =>
          reqTable(ReqTable.DynamicProps(
            s.editStates.mapKey1(reqtable.Column.EditFieldKeyIntersection.reverse),
            s.asyncStates.mapKey1(AsyncKey.ToReqTable2),
            previewFeature.toProps(s.previewState).mapId(FocusId.ToReqTable),
            s.reqTable))

        case Page.ReqDetail(pubid) =>
          val props = ReqDetail.DynamicProps(
            pubid,
            fd,
            reqDetailReqPropsFn(s),
            StateSnapshot.withReuse(s.reqDetail)(reqDetailSetState))
          reqDetail(props)

        case Page.ImpGraph =>
          val p = cd.project()
          val g = ImplicationGraph.Props(
            None, s.filterDead,
            p.implications, p.reqs, p.config.reqTypes,
            pxPlainText.value(),
            reqDetailRC,
            ww)
          ImplicationGraphPage.Props(g, setFilterDead).render
      }

      Layout.Props(initData.username, cd.projectSummary(), routerCtl, p.page, content).render
    }

    def onProjectChange(c: Changes): Callback =
      $.modState(State.reqTable.modify(_ updateProject c.p2))
  }

  val Component = ScalaComponent.builder[Props]("LoadedRoot")
    .initialState(State.init(cd))
    .renderBackend[Backend]
    .configure(Listenable.listen(_ => cd, _.backend.onProjectChange))
    .build
}
