package shipreq.webapp.client.project.app.root

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.Implicits._
import japgolly.scalajs.react.vdom.VdomElement
import shipreq.base.util.{Allow, ErrorMsg, Intersection}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{FilterDead, ReqId}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.protocol.{ProjectSpaProtocols, UpdateContentCmd}
import shipreq.webapp.base.text.{PlainText, ProjectText, TextSearch}
import shipreq.webapp.base.feature._
import shipreq.webapp.base.protocol.{ClientProtocol, ServerSideProcInvoker}
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
import Routes.{Page, RouterCtl}
import LoadedRoot._

object LoadedRoot {
  case class Props(page: Page, routerCtl: RouterCtl)
}

final class LoadedRoot(initData: ProjectSpaProtocols.InitData, cp: ClientProtocol, val cd: ClientData) {

  final class Backend($: BackendScope[Props, State]) extends OnUnmount {
    import cd.pxProject

    // This never changes
    val routerCtl = $.props.runNow().routerCtl
    val reqDetailRC = routerCtl.contramap(Page.ReqDetail.apply)

    val setFilterDead: FilterDead ~=> Callback =
      Reusable.fn.state($ zoomStateL State.filterDead).set

    val pxPlainText =
      pxProject.map(PlainText.ForProject.noCtx)

    val pxTextSearch =
      Px.apply2(pxProject, pxPlainText)(TextSearch.apply)

    val pxProjectWidgets =
      Reusable byRef Px.apply2(pxProject, pxPlainText)(ProjectWidgets(_, _, reqDetailRC))

    val pxCreateEditability =
      pxProject.map(p => CreateFeature.Editability(p.config))

    val pxEditEditability =
      pxProject.map(EditorFeature.Editability.apply)

    val updateIO: ServerSideProcInvoker[UpdateContentCmd, ErrorMsg, VerifiedEvent.Seq] =
      cd.serverSideProcToEvents(cp, initData.updateContent)

    val previewW: PreviewFeature.Write.Composite[PreviewId] =
      PreviewFeature.Write.Composite($ zoomStateL State.preview)

    val createAsyncW: AsyncFeature.Write.D1[CreateFeature.RowKey, ErrorMsg] =
      AsyncFeature.Write.D1.init($ zoomStateL State.createAsync)

    val createW: CreateFeature.Write.ForProject =
      CreateFeature.Write.ForProject(
        CreateFeature.Static(
          previewW.mapId(PreviewId.ToCreate),
          pxProject,
          pxProjectWidgets,
          pxTextSearch),
        $ zoomStateL State.create,
        createAsyncW,
        cd.serverSideProcToEvents(cp, initData.createContent))

    val editAsyncW: AsyncFeature.Write.D2[EditorFeature.RowKey, AsyncKey, ErrorMsg] =
      AsyncFeature.Write.D2.init($ zoomStateL State.editAsync)

    val editW: EditorFeature.Write.ForProject =
      EditorFeature.Write.ForProject(
        EditorFeature.Static(
          previewW.mapId(PreviewId.ToEditor),
          pxProject,
          pxPlainText,
          pxTextSearch,
          updateIO),
        $ zoomStateL State.edit,
        editAsyncW.mapKey1(AsyncKey.ToEditor))

    val rowAsyncW: AsyncFeature.Write.D1[EditorFeature.RowKey, ErrorMsg] =
      editAsyncW.withKey1(AsyncKey.WholeReq)

    val reqTable = ReqTablePage(
      ReqTablePage.StaticProps(
        $ zoomStateL State.reqTable,
        cd,
        pxTextSearch, pxProjectWidgets,
        reqDetailRC,
        updateIO,
        rowAsyncW.mapKey(reqtable.Row.SourceId.ToEditorRow.reverse)))

    val pxReqDetailId = Px[Option[ReqId]](None).withReuse.manualUpdate

    val pxReqDetailReqProps: Px[Option[State => ReqDetail.ReqProps]] =
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

    val reqDetail = ReqDetail(ReqDetail.StaticProps(
      updateIO, reqDetailRC, ww, initData.updateContent,
      pxProject, pxTextSearch, pxProjectWidgets))

    val reqDetailSetState: ReqDetail.State ~=> Callback =
      Reusable.fn.state($ zoomStateL State.reqDetail).set

    def setReqTableView(fd: FilterDead, f: Filter.Valid): Callback =
      pxProject.toCallback.flatMap(project =>
        $.modState(s => s.copy(
          filterDead = fd,
          reqTable = s.reqTable.setFilter(f, project.config))))

    val usageShow =
      Usage.Show((filterDead, filter) =>
        routerCtl
          .onSet(setReqTableView(filterDead, filter()) >> _)
          .link(Page.ReqTable))

    lazy val projectNameAF =
      AsyncFeature.Write.D0[ErrorMsg](
        Reusable.fn(
          $.modStateFn[AsyncFeature.State.D0[ErrorMsg]](s =>
            State.projectName.modify(ProjectItem.WithEditableName.State setAsync s))))

    val setProjectNameIO: String => Callback = {
      val proc = cd.serverSideProcToEvents(cp, initData.projectNameSet)
      newName => {
        def close = $.modState(State.projectName set None)
        def save = projectNameAF((s, f) => proc(newName, _ => s >> close, f))
        cd.projectCB >>= (p => if (p.name ==* newName) close else save)
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
            Allow when _.lookup(cd.project()).isRight,
            e => routerCtl.set(Page.ReqDetail(e)))

          val index = ProjectIndex.Props(lookup, routerCtl)

          val pname = ProjectItem.WithEditableName.Props(
            cd.projectMetaData(),
            StateSnapshot.zoomL(State.projectName)(s).setStateVia($),
            setProjectNameIO)

          ProjectHome.Props(pname, index).render

        case Page.CfgFields =>
          cfg.fields.CfgFields.Props(cp, initData.fieldCrud, cd, filterDeadSS).component

        case Page.CfgIssues =>
          cfg.issues.CfgIssues.Props(
            cp, initData.issueTypeCrud, initData.reqTypeImpMod, initData.fieldMandMod, cd, filterDeadSS, usageShow)
            .component

        case Page.CfgReqTypes =>
          cfg.reqtypes.CfgReqTypes.Props(cp, initData.reqTypeCrud, cd, filterDeadSS, usageShow).component

        case Page.CfgTags =>
          cfg.tags.CfgTags.Props(cp, initData.tagCrud, cd, filterDeadSS).component

        case Page.ReqTable =>
          val rowAsync = editAsyncState
            .mapKey2(reqtable.Row.SourceId.ToEditorRow.reverse)
            .withKey1(AsyncKey.WholeReq)
          reqTable(
            ReqTablePage.Props(
              createRW,
              editRW,
              rowAsync,
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
          val p = cd.project()
          val g = ImplicationGraph.Props(
            None, s.filterDead,
            p.implications, p.reqs, p.config.reqTypes,
            pxPlainText.value(),
            reqDetailRC,
            ww)
          ImplicationGraphPage.Props(g, setFilterDead).render
      }

      Layout.Props(initData.username, cd.projectMetaData(), routerCtl, p.page, content).render
    }

    def onProjectChange(c: Changes): Callback = // TODO I don't like this
      $.forceUpdate
  }

  val Component = ScalaComponent.builder[Props]("LoadedRoot")
    .initialState(State.init(cd))
    .renderBackend[Backend]
    .configure(Listenable.listen(_ => cd, _.backend.onProjectChange))
    .build
}
