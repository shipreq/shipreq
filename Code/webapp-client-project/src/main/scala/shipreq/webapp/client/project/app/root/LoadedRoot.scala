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
      Reusable.fn(fd => $.modState(
        State.filterDead.set(fd) compose
        State.reqTableVS.modify(_ setFilterDead fd)))

    val pxPlainText      = pxProject.map(PlainText(_, ProjectText.Context.None))
    val pxTextSearch     = Px.apply2(pxProject, pxPlainText)(TextSearch.apply)
    val pxProjectWidgets = Px.apply2(pxProject, pxPlainText)(ProjectWidgets(_, _, reqDetailRC))
    val pxEditability    = pxProject.map(EditorFeature.Editability.apply)

    val asyncFeature: AsyncFeature.Feature.D2[EditorFeature.RowKey, AsyncKey, String] =
      AsyncFeature.Feature.D2.init($ zoomStateL State.async)

    val previewFeature: PreviewFeature.Feature.Composite[FocusId] =
      PreviewFeature.Feature.Composite.init($ zoomStateL State.preview)

    val editorFeature: EditorFeature.Write.ForProject =
      EditorFeature.Write.ForProject(
        EditorFeature.Static(
          previewFeature.mapId(FocusId.ToEditor),
          pxProject,
          pxPlainText,
          pxProjectWidgets,
          pxTextSearch,
          ServerCall.to(initData.updateContent, cp, cd)),
        $ zoomStateL State.editors,
        asyncFeature.mapKey1(AsyncKey.ToEditor))

    val reqTable = ReqTable(ReqTable.StaticProps(
      cd, cp, initData.createContent, initData.updateContent,
      pxPlainText, pxTextSearch, pxProjectWidgets,
      asyncFeature.mapKey2(reqtable.Row.SourceIdToEditorRow.reverse).mapKey1(AsyncKey.ToReqTable2),
      reqDetailRC,
      $ zoomStateL State.reqTable))

    val pxReqDetailId = Px[Option[ReqId]](None).withReuse.manualUpdate

    val pxReqDetailReqProps: Px[Option[State => ReqDetail.ReqProps]] =
      for {
        editability <- pxEditability
        reqDetailId <- pxReqDetailId
      } yield reqDetailId.map { id =>
        val ew = editorFeature.forReq(id)
        val row = EditorFeature.RowKey.Req(id)
        val af = asyncFeature(row).mapKey(AsyncKey.ToReqDetail)

        (s: State) => {
          val es = EditorFeature.Read.ForProject(s.editors, editability, s.async.toReadOnly.mapKey1(AsyncKey.ToEditor))
          val er = es.forReq(id)
          val ep = EditorFeature.Props.ForRow(er, ew)
          val as = s.async.toReadOnly(row).mapKey(AsyncKey.ToReqDetail)
          val ap = af.toProps(as)
          ReqDetail.ReqProps(ep, ap)
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
          .onSet($.modState(State.reqTable.modify(_.setFilterDead(fd).setFilterSpec(fs()))) >> _)
          .link(Page.ReqTable))

    lazy val projectNameAF =
      AsyncFeature.Feature.D0[String](
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
      def fd = StateSnapshot.withReuse(s.filterDead)(setFilterDead)

      lazy val asyncState = s.async.toReadOnly
      def editorState = EditorFeature.Read.ForProject(s.editors, pxEditability.value(), asyncState.mapKey1(AsyncKey.ToEditor))
      def editorProps = editorFeature.toProps(editorState)
      def previewProps = previewFeature.toProps(s.preview)

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
          reqTable(
            ReqTable.DynamicProps(
              editorProps,
              asyncState.mapKey2(reqtable.Row.SourceIdToEditorRow.reverse).mapKey1(AsyncKey.ToReqTable2),
              previewProps.mapId(FocusId.ToReqTable),
              s.reqTable))

        case Page.ReqDetail(pubid) =>
          val props = ReqDetail.DynamicProps(
            pubid,
            fd,
            reqDetailReqPropsFn(s),
            editorProps.forUseCaseSteps,
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
