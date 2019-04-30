package shipreq.webapp.client.project.app.root

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.VdomElement
import shipreq.base.util.{Allow, ErrorMsg}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{FilterDead, ReqId}
import shipreq.webapp.base.feature._
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.ProjectSpaProtocols.{InitPageData, WsReqRes}
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.base.ui.ProjectItem
import shipreq.webapp.client.project.app.state._
import shipreq.webapp.client.project.app._
import shipreq.webapp.client.project.app.reqdetail.ReqDetail
import shipreq.webapp.client.project.app.reqtable.ReqTablePage
import shipreq.webapp.client.project.app.cfg.shared.Usage
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.{ImplicationGraph, ProjectWidgets}
import AsyncFeature.Implicits._
import LoadedRoot._
import Routes.{Page, RouterCtl}

object LoadedRoot {
  case class Props(page: Page, routerCtl: RouterCtl)
}

final class LoadedRoot(initPageData: InitPageData, global: Global) {

  val pxProject = global.pxProject

  final class Backend($: BackendScope[Props, State]) extends OnUnmount {
    import global.{cbProjectMetaData, wsClient}

    private val sspInitApp               = wsClient.invoker(WsReqRes.InitApp              ).mergeFailure
    private val sspCreateContent         = wsClient.invoker(WsReqRes.CreateContent        ).mergeFailure
    private val sspUpdateContent         = wsClient.invoker(WsReqRes.UpdateContent        ).mergeFailure
    private val sspProjectNameSet        = wsClient.invoker(WsReqRes.ProjectNameSet       ).mergeFailure
    private val sspUpdateSavedViews      = wsClient.invoker(WsReqRes.UpdateSavedViews     ).mergeFailure
    private val sspFieldMandatorinessMod = wsClient.invoker(WsReqRes.FieldMandatorinessMod).mergeFailure
    private val sspReqTypeImplicationMod = wsClient.invoker(WsReqRes.ReqTypeImplicationMod).mergeFailure
    private val sspCustomIssueTypeCrud   = wsClient.invoker(WsReqRes.CustomIssueTypeCrud  ).mergeFailure
    private val sspCustomReqTypeCrud     = wsClient.invoker(WsReqRes.CustomReqTypeCrud    ).mergeFailure
    private val sspFieldMod              = wsClient.invoker(WsReqRes.FieldMod             ).mergeFailure
    private val sspTagMod                = wsClient.invoker(WsReqRes.TagMod               ).mergeFailure

    // This never changes
    private val routerCtl = $.props.runNow().routerCtl
    private val reqDetailRC = routerCtl.contramap(Page.ReqDetail.apply)

    private val setFilterDead: Reusable[SetStateFnPure[FilterDead]] =
      Reusable.fn.state($ zoomStateL State.filterDead).setStateFn

    private val pxPlainText =
      pxProject.map(PlainText.ForProject.noCtx)

    private val pxTextSearch =
      Px.apply2(pxProject, pxPlainText)(TextSearch.apply)

    private val pxProjectWidgets =
      Reusable byRef Px.apply2(pxProject, pxPlainText)(ProjectWidgets(_, _, reqDetailRC))

    private val pxCreateEditability =
      pxProject.map(p => CreateFeature.Editability(p.config))

    private val pxEditEditability =
      pxProject.map(EditorFeature.Editability.apply)

    private val previewW: PreviewFeature.Write.Composite[PreviewId] =
      PreviewFeature.Write.Composite($ zoomStateL State.preview)

    private val createAsyncW: AsyncFeature.Write.D1[CreateFeature.RowKey, ErrorMsg] =
      AsyncFeature.Write.D1.init($ zoomStateL State.createAsync)

    private val createW: CreateFeature.Write.ForProject =
      CreateFeature.Write.ForProject(
        CreateFeature.Static(
          previewW.mapId(PreviewId.ToCreate),
          pxProject,
          pxProjectWidgets,
          pxTextSearch),
        $ zoomStateL State.create,
        createAsyncW,
        sspCreateContent)

    private val editAsyncW: AsyncFeature.Write.D2[EditorFeature.RowKey, AsyncKey, ErrorMsg] =
      AsyncFeature.Write.D2.init($ zoomStateL State.editAsync)

    private val editW: EditorFeature.Write.ForProject =
      EditorFeature.Write.ForProject(
        EditorFeature.Static(
          previewW.mapId(PreviewId.ToEditor),
          pxProject,
          pxPlainText,
          pxTextSearch,
          sspUpdateContent),
        $ zoomStateL State.edit,
        editAsyncW.mapKey1(AsyncKey.ToEditor))

    private val rowAsyncW: AsyncFeature.Write.D1[EditorFeature.RowKey, ErrorMsg] =
      editAsyncW.withKey1(AsyncKey.WholeReq)

    private val savedViewAsyncW: AsyncFeature.Write.D0[ErrorMsg] =
      AsyncFeature.Write.D0.init($ zoomStateL State.savedViewAsync)

    private val reqTable = ReqTablePage(
      ReqTablePage.StaticProps(
        $ zoomStateL State.reqTable,
        pxProject,
        pxTextSearch, pxProjectWidgets,
        reqDetailRC,
        sspUpdateContent,
        sspUpdateSavedViews,
        rowAsyncW.mapKey(reqtable.Row.SourceId.ToEditorRow.reverse),
        savedViewAsyncW))

    private val pxReqDetailId = Px[Option[ReqId]](None).withReuse.manualUpdate

    private val pxReqDetailReqProps: Px[Option[State => ReqDetail.ReqProps]] =
      for {
        editability <- pxEditEditability
        reqDetailId <- pxReqDetailId
      } yield reqDetailId.map { id =>
        val row = EditorFeature.RowKey.req(id)
        val aw = editAsyncW(row).mapKey(AsyncKey.ToReqDetail)
        val ew = editW.forReq(id)

        (s: State) => {
          val as = s.editAsync.toRead
          val ar = as(row).mapKey(AsyncKey.ToReqDetail)
          val af = aw.toReadWrite(ar)
          val er = EditorFeature.Read.ForProject(s.edit, editability, as.mapKey1(AsyncKey.ToEditor)).forReq(id)
          val ef = EditorFeature.ReadWrite.ForFields(er, ew)
          ReqDetail.ReqProps(ef, af)
        }
      }

    def reqDetailReqPropsFn(s: State) = (id: ReqId) => {
      pxReqDetailId.set(Some(id))
      pxReqDetailReqProps.value().get(s)
    }

    def ww = WebWorkerClient.Instance

    private val reqDetail = ReqDetail(ReqDetail.StaticProps(
      sspUpdateContent, reqDetailRC, ww, pxProject, pxTextSearch, pxProjectWidgets))

    private val reqDetailSetState: Reusable[SetStateFnPure[ReqDetail.State]] =
      Reusable.fn.state($ zoomStateL State.reqDetail).setStateFn

    def setReqTableView(fd: FilterDead, filter: Filter.Valid): Callback =
      for {
        p ← pxProject.toCallback
        f = ReqTablePage.State.modifyView(p, fd, true)(_.withFilter(Some(filter)))
        _ ← $.modState(State.reqTable.modify(f) compose State.filterDead.set(fd))
      } yield ()

    private val usageShow =
      Usage.Show((filterDead, filter) =>
        routerCtl
          .onSet(setReqTableView(filterDead, filter()) >> _)
          .link(Page.ReqTable))

    lazy val projectNameAF =
      AsyncFeature.Write.D0[ErrorMsg](
        Reusable.fn((s: AsyncFeature.State.D0[ErrorMsg]) =>
          $.modState(State.projectName.modify(ProjectItem.WithEditableName.State setAsync s))))

    private val setProjectNameIO: String => Callback = {
      newName => {
        def close = $.modState(State.projectName set None)
        def save = projectNameAF((s, f) => sspProjectNameSet(newName, _ => s >> close, f))
        pxProject.toCallback >>= (p => if (p.name ==* newName) close else save)
      }
    }

    def render(p: Props, s: State): VdomElement = {
      lazy val editAsyncState = s.editAsync.toRead
      def createR      = CreateFeature.Read.ForProject(s.create, pxCreateEditability.value(), s.createAsync.toRead)
      def createRW     = createW.toReadWrite(createR)
      def editR        = EditorFeature.Read.ForProject(s.edit, pxEditEditability.value(), editAsyncState.mapKey1(AsyncKey.ToEditor))
      def editRW       = editW.toReadWrite(editR)
      def filterDeadSS = StateSnapshot.withReuse(s.filterDead)(setFilterDead)
      // def previewRW = previewW.toReadWrite(s.preview)

      val content: VdomElement = p.page match {

        case Page.Index =>
          val lookup = ReqLookupPrompt.Props(
            StateSnapshot.zoomL(State.reqLookup)(s).setStateVia($),
            Allow when _.lookup(pxProject.value()).isRight,
            e => routerCtl.set(Page.ReqDetail(e)))

          val index = ProjectIndex.Props(lookup, routerCtl)

          val pname = ProjectItem.WithEditableName.Props(
            cd.projectMetaData(),
            StateSnapshot.zoomL(State.projectName)(s).setStateVia($),
            setProjectNameIO)

          ProjectHome.Props(pname, index).render

        case Page.CfgFields =>
          cfg.fields.CfgFields.Props(sspFieldMod, cd, filterDeadSS).component

        case Page.CfgIssues =>
          cfg.issues.CfgIssues.Props(
            sspCustomIssueTypeCrud, sspReqTypeImplicationMod, sspFieldMandatorinessMod, cd, filterDeadSS, usageShow)
            .component

        case Page.CfgReqTypes =>
          cfg.reqtypes.CfgReqTypes.Props(sspCustomReqTypeCrud, cd, filterDeadSS, usageShow).component

        case Page.CfgTags =>
          cfg.tags.CfgTags.Props(sspTagMod, cd, filterDeadSS).component

        case Page.ReqTable =>
          val rowAsync = editAsyncState
            .mapKey2(reqtable.Row.SourceId.ToEditorRow.reverse)
            .withKey1(AsyncKey.WholeReq)
          reqTable(
            ReqTablePage.Props(
              createRW,
              editRW,
              rowAsync,
              s.savedViewAsync,
              filterDeadSS,
              s.reqTable))

        case Page.ReqDetail(pubid) =>
          val props = ReqDetail.DynamicProps(
            pubid,
            filterDeadSS,
            reqDetailReqPropsFn(s),
            editRW.forUseCaseSteps,
            StateSnapshot.withReuse(s.reqDetail)(reqDetailSetState))
          reqDetail(props)

        case Page.ImpGraph =>
          val p = pxProject.value()
          val g = ImplicationGraph.Props(
            None, s.filterDead,
            p.content.implications, p.content.reqs, p.config.reqTypes,
            pxPlainText.value(),
            reqDetailRC,
            ww)
          ImplicationGraphPage.Props(g, setFilterDead).render
      }

      Layout.Props(initPageData.username, cbProjectMetaData.runNow(), routerCtl, p.page, content).render
    }

    def onProjectChange(c: Changes): Callback = // TODO I don't like this
      $.forceUpdate
  }

  val Component = ScalaComponent.builder[Props]("LoadedRoot")
    .initialState(State.init)
    .renderBackend[Backend]
    .configure(Listenable.listen(_ => global, _.backend.onProjectChange))
    .build
}
