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
import shipreq.webapp.client.project.app.reqtable2.ReqTablePage
import shipreq.webapp.client.project.app.cfg.shared.Usage
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.{ImplicationGraph, ProjectWidgets}
import AsyncFeature.Implicits._
import Routes.{Page, RouterCtl}
import LoadedRoot._
import shipreq.webapp.client.project.protocol.ServerCall

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
      Reusable.fn.state($ zoomStateL State.filterDead).set

    val pxPlainText      = pxProject.map(PlainText(_, ProjectText.Context.None))
    val pxTextSearch     = Px.apply2(pxProject, pxPlainText)(TextSearch.apply)
    val pxProjectWidgets = Px.apply2(pxProject, pxPlainText)(ProjectWidgets(_, _, reqDetailRC))
    val pxEditability    = pxProject.map(EditorFeature.Editability.apply)

    val previewW: PreviewFeature.Write.Composite[PreviewId] =
      PreviewFeature.Write.Composite.init($ zoomStateL State.preview)

    val asyncW: AsyncFeature.Write.D2[EditorFeature.RowKey, AsyncKey, String] =
      AsyncFeature.Write.D2.init($ zoomStateL State.async)

    val editorW: EditorFeature.Write.ForProject =
      EditorFeature.Write.ForProject(
        EditorFeature.Static(
          previewW.mapId(PreviewId.ToEditor),
          pxProject,
          pxPlainText,
          pxProjectWidgets,
          pxTextSearch,
          ServerCall.to(initData.updateContent, cp, cd)),
        $ zoomStateL State.editors,
        asyncW.mapKey1(AsyncKey.ToEditor))

    val reqTable = ReqTablePage(
      ReqTablePage.StaticProps(
        $ zoomStateL State.reqTable,
        cd,
        pxPlainText, pxTextSearch, pxProjectWidgets,
        reqDetailRC))

    val pxReqDetailId = Px[Option[ReqId]](None).withReuse.manualUpdate

    val pxReqDetailReqProps: Px[Option[State => ReqDetail.ReqProps]] =
      for {
        editability <- pxEditability
        reqDetailId <- pxReqDetailId
      } yield reqDetailId.map { id =>
        val row = EditorFeature.RowKey.req(id)
        val aw = asyncW(row).mapKey(AsyncKey.ToReqDetail)
        val ew = editorW.forReq(id)

        (s: State) => {
          val as = s.async.toRead
          val ar = as(row).mapKey(AsyncKey.ToReqDetail)
          val af = aw.toReadWrite(ar)
          val er = EditorFeature.Read.ForProject(s.editors, editability, as.mapKey1(AsyncKey.ToEditor)).forReq(id)
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
      cd, cp, reqDetailRC, ww, initData.updateContent,
      pxPlainText, pxTextSearch, pxProjectWidgets))

    val reqDetailSetState: ReqDetail.State ~=> Callback =
      Reusable.fn.state($ zoomStateL State.reqDetail).set

    val usageShow =
      Usage.Show((fd, fs) =>
        routerCtl
          // TODO .onSet($.modState(State.reqTable.modify(_.setFilterDead(fd).setFilterSpec(fs()))) >> _)
          .link(Page.ReqTable))

    lazy val projectNameAF =
      AsyncFeature.Write.D0[String](
        Reusable.fn(
          $.modStateFn[AsyncFeature.State.D0[String]](s =>
            State.projectName.modify(ProjectItem.WithEditableName.State setAsync s))))


    val setProjectNameIO: String => Callback =
      newName => {
        def close = $.modState(State.projectName set None)
        def save = projectNameAF((onSuccess, onFailure) =>
          cp.call(initData.projectNameSet)(
            newName,
            cd.applyEventsS(_) >> onSuccess >> close,
            _ consumeAnd onFailure))
        cd.projectCB >>= (p => if (p.name ==* newName) close else save)
      }

    def render(p: Props, s: State): VdomElement = {
      def filterDeadSnapshot = StateSnapshot.withReuse(s.filterDead)(setFilterDead)

      lazy val asyncState = s.async.toRead
      def editorR = EditorFeature.Read.ForProject(s.editors, pxEditability.value(), asyncState.mapKey1(AsyncKey.ToEditor))
      def editorRW = editorW.toReadWrite(editorR)
      def previewRW = previewW.toReadWrite(s.preview)

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
          cfg.fields.CfgFields.Props(cp, initData.fieldCrud, cd, filterDeadSnapshot).component

        case Page.CfgIssues =>
          cfg.issues.CfgIssues.Props(
            cp, initData.issueTypeCrud, initData.reqTypeImpMod, initData.fieldMandMod, cd, filterDeadSnapshot, usageShow)
            .component

        case Page.CfgReqTypes =>
          cfg.reqtypes.CfgReqTypes.Props(cp, initData.reqTypeCrud, cd, filterDeadSnapshot, usageShow).component

        case Page.CfgTags =>
          cfg.tags.CfgTags.Props(cp, initData.tagCrud, cd, filterDeadSnapshot).component

        case Page.ReqTable =>
          val rowAsync = asyncW
            .toReadWrite(asyncState)
            .mapKey2(reqtable2.Row.SourceId.ToEditorRow.reverse)
            .withKey1(AsyncKey.WholeReq)
          reqTable(
            ReqTablePage.Props(
              editorRW,
              rowAsync,
              filterDeadSnapshot,
              s.reqTable))

        case Page.ReqDetail(pubid) =>
          val props = ReqDetail.DynamicProps(
            pubid,
            filterDeadSnapshot,
            reqDetailReqPropsFn(s),
            editorRW.forUseCaseSteps,
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

    def onProjectChange(c: Changes): Callback = // TODO I don't like this
      Callback.empty
//      $.modState(State.reqTable.modify(_ updateProject c.p2))
  }

  val Component = ScalaComponent.builder[Props]("LoadedRoot")
    .initialState(State.init(cd))
    .renderBackend[Backend]
    .configure(Listenable.listen(_ => cd, _.backend.onProjectChange))
    .build
}
