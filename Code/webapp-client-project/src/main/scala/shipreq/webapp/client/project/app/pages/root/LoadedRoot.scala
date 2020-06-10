package shipreq.webapp.client.project.app.pages.root

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.Implicits._
import japgolly.scalajs.react.vdom.VdomElement
import monocle.Lens
import org.scalajs.dom.window
import scalaz.{-\/, \/-}
import shipreq.base.util.univeq._
import shipreq.base.util.{Allow, ErrorMsg}
import shipreq.webapp.base.data.{FilterDead, HideDead, Project, ProjectConfig, ReqId}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.feature.AsyncFeature.Implicits._
import shipreq.webapp.base.feature._
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.lib.{ConfirmJs, PromptJs}
import shipreq.webapp.base.protocol.ajax.CommonProtocolsJs
import shipreq.webapp.base.protocol.entrypoint.ProjectSpaEntryPoint
import shipreq.webapp.base.protocol.websocket._
import shipreq.webapp.base.text.{PlainText, ProjectText, TextSearch}
import shipreq.webapp.base.ui.{FeedbackModal, ProjectItem, Toast}
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.client.project.app._
import shipreq.webapp.client.project.app.pages._
import shipreq.webapp.client.project.app.pages.content.reqdetail.ReqDetail
import shipreq.webapp.client.project.app.pages.content.reqtable.ReqTablePage
import shipreq.webapp.client.project.app.pages.root.LoadedRoot._
import shipreq.webapp.client.project.app.pages.root.Routes.{Page, RouterCtl}
import shipreq.webapp.client.project.app.state._
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.lib.Usage
import shipreq.webapp.client.project.widgets.{ProjectWidgets, ViewReqCache, ViewReqDataCache}
import shipreq.webapp.client.ww.api.WebWorkerCmd

object LoadedRoot {
  case class Props(page: Page, routerCtl: RouterCtl)
}

final class LoadedRoot(initPageData: ProjectSpaEntryPoint.InitData,
                       global      : Global,
                       confirmJs   : ConfirmJs,
                       promptJs    : PromptJs) {

  val pxProjectAndOrd = global.pxProjectAndOrd
  val pxProject       = global.pxProject
  def unsafeProject() = global.unsafeProject()

  private val stateLensFilterDead =
    Lens[State, FilterDead](_._filterDead)(fd => _.setFilterDead(fd, unsafeProject()))

  private val stateLensSavedViewStateAndFilterDead =
    Lens[State, (SavedViewFeature.State, FilterDead)](
      s => (s.savedViews, s.filterDead))(
      n => _.copy(savedViews = n._1).setFilterDead(n._2, unsafeProject()))

  final class Backend($: BackendScope[Props, State]) extends OnUnmount {
    import global.cbProjectMetaData

    private val sspCreateContent       = global.sspCreateContent
    private val sspUpdateConfig        = global.sspUpdateConfig
    private val sspUpdateConfigE       = global.sspUpdateConfig.map(_.events)
    private val sspUpdateContent       = global.sspUpdateContent.map(_.events)
    private val sspProjectNameSet      = global.sspProjectNameSet.map(_.events)
    private val sspUpdateSavedViews    = global.sspUpdateSavedViews.map(_.events)
    private val sspUpdateManualIssues  = global.sspUpdateManualIssues
    private val sspUpdateManualIssuesE = global.sspUpdateManualIssues.map(_.events)

    private val feedbackModal: FeedbackModal = {
      val projectMetadata = global.projectMetadata(initPageData.projectId)
      val clientMetadata  = CommonProtocolsJs.Metadata.client(initPageData.username, projectMetadata)
      FeedbackModal(clientMetadata)
    }

    // This never changes
    private val routerCtl = $.props.runNow().routerCtl
    private val reqDetailRC = routerCtl.contramap(Page.ReqDetail.apply)

    private val toast = Toast($.zoomStateL(State.toast))

    private val pxState =
      Px.state($).withReuse.autoRefresh

    private val pxFilterDead =
      pxState.map(_.filterDead).withReuse

    private val pxUseCases =
      pxProject.map(_.content.reqs.useCases).withReuse

    private val pxProjectName: Px[Project.Name] =
      pxProject.map(_.name).withReuse

    private val pxProjectConfig: Px[ProjectConfig] =
      pxProject.map(_.config).withReuse

    private val pxEditEditability: Px[EditorFeature.Editability.ForProject] =
      pxProject.map(EditorFeature.Editability.apply)

    private val pxUnsavedChangesInput: Px[UnsavedChanges.Input] =
      Px.apply5(pxState, pxEditEditability, pxProjectName, pxProjectConfig, pxUseCases)(UnsavedChanges.Input.apply)

    private val pxUnsavedChanges: Px[UnsavedChanges] =
      pxUnsavedChangesInput.map(UnsavedChanges.determine).flatMap(Px.callback(_).withReuse.autoRefresh)

    private val pxLayoutUnsavedChangeData: Px[Layout.UnsavedChangeData] =
      for {
        c <- pxUnsavedChanges
        p <- pxProject
      } yield Layout.UnsavedChangeData.derive(c, p, routerCtl)

    private val setFilterDead: Reusable[SetStateFnPure[FilterDead]] =
      Reusable.fn.state($ zoomStateL stateLensFilterDead).setStateFn

    private val pxPlainText: Px[PlainText.ForProject.NoCtx] =
      pxProject.map(PlainText.ForProject.noCtx.apply)

    private val pxTextSearch: Px[TextSearch] =
      Px.apply2(pxProject, pxPlainText)(TextSearch.apply)

    private val pxProjectWidgets: Reusable[Px[ProjectWidgets.NoCtx]] =
      Reusable byRef Px.apply2(pxProject, pxPlainText)(ProjectWidgets(_, _, reqDetailRC))

    private val pxViewReqDataCache: Px[ViewReqDataCache] =
      pxProject.map(ViewReqDataCache.apply)

    private val pxViewReqCache: Px[ViewReqCache.ToVdom[ProjectText.Context.None]] =
      Px.apply2(pxViewReqDataCache, pxProjectWidgets)(ViewReqCache.apply)

    private val pxViewReqCacheText: Px[ViewReqCache[ProjectText.Context.None, String]] =
      Px.apply2(pxViewReqDataCache, pxPlainText)(ViewReqCache.apply)

    private val pxRenderFeature: Px[FilterDead => RenderFeature.ToVdom.NoCtx.IfApplicable.ForProject] =
      Px.apply3(pxProject, pxViewReqCache, pxProjectWidgets)(RenderFeature.ToVdom.NoCtx.IfApplicable.prepare)

    private val pxRenderFeatureText: Px[FilterDead => RenderFeature.ToText.NoCtx.ApplicableOption.ForProject] =
      Px.apply3(pxProject, pxViewReqCacheText, pxPlainText)(RenderFeature.ToText.NoCtx.ApplicableOption.prepare)

    private val pxCreateEditability =
      pxProject.map(p => CreateFeature.Editability(p.config))

    private val pxFilterCompilerFromFilterDead: Px[FilterDead => Filter.Valid.Compiler] =
      for {
        p  <- pxProject
        pt <- pxPlainText
        ts <- pxTextSearch
      } yield FilterDead.memoLazy(Filter.Valid.compiler(p, pt, ts, _, applyFilterDeadToReqs = false))

    private val pxFilterCompilerHideDead: Px[Filter.Valid.Compiler] =
      pxFilterCompilerFromFilterDead.map(_(HideDead))

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
        sspCreateContent,
        sspUpdateManualIssues)

    private val editAsyncW: AsyncFeature.Write.D2[EditorFeature.RowKey, AsyncKey, ErrorMsg] =
      AsyncFeature.Write.D2.init($ zoomStateL State.editAsync)

    private val editW: EditorFeature.Write.ForProject =
      EditorFeature.Write.ForProject(
        EditorFeature.Static(
          previewW.mapId(PreviewId.ToEditor),
          pxProject,
          pxPlainText,
          pxTextSearch,
          sspUpdateContent,
          sspUpdateManualIssuesE,
        ),
        $ zoomStateL State.edit,
        editAsyncW.mapKey1(AsyncKey.ToEditor))

    private val rowAsyncW: AsyncFeature.Write.D1[EditorFeature.RowKey, ErrorMsg] =
      editAsyncW.withKey1(AsyncKey.WholeReq)

    private val savedViewAsyncW: AsyncFeature.Write.D0[ErrorMsg] =
      AsyncFeature.Write.D0.init($ zoomStateL State.savedViewAsync)

    private val fieldConfigAsyncW: AsyncFeature.Write.D0[ErrorMsg] =
      AsyncFeature.Write.D0.init($ zoomStateL State.fieldConfigAsync)

    private val customIssueTypeConfigAsyncW: AsyncFeature.Write.D0[ErrorMsg] =
      AsyncFeature.Write.D0.init($ zoomStateL State.customIssueTypeConfigAsync)

    private val reqTypeConfigAsyncW: AsyncFeature.Write.D0[ErrorMsg] =
      AsyncFeature.Write.D0.init($ zoomStateL State.reqTypeConfigAsync)

    private val tagConfigAsyncW: AsyncFeature.Write.D0[ErrorMsg] =
      AsyncFeature.Write.D0.init($ zoomStateL State.tagConfigAsync)

    private val updateConfigCmdAsyncW: AsyncFeature.Write.D1[UpdateConfigCmd, ErrorMsg] =
      AsyncFeature.Write.D1.init($ zoomStateL State.updateConfigCmdAsync)

    private val updateContentCmdAsyncW: AsyncFeature.Write.D1[UpdateContentCmd, ErrorMsg] =
      AsyncFeature.Write.D1.init($ zoomStateL State.updateContentCmdAsync)

    private val manualIssueCmdAsyncW: AsyncFeature.Write.D1[ManualIssueCmd, ErrorMsg] =
      AsyncFeature.Write.D1.init($ zoomStateL State.manualIssueCmdAsync)

    private val updateConfigCmdInvoker: UpdateConfigCmd ~=> Callback =
      Reusable.fn(cmd => updateConfigCmdAsyncW(cmd)(sspUpdateConfigE(cmd)))

    private val updateContentCmdInvoker: UpdateContentCmd ~=> Callback =
      Reusable.fn(cmd => updateContentCmdAsyncW(cmd)(sspUpdateContent(cmd)))

    private val manualIssueCmdInvoker: ManualIssueCmd ~=> Callback =
      Reusable.fn(cmd => manualIssueCmdAsyncW(cmd)(sspUpdateManualIssuesE(cmd)))

    private val updateConfigOrContentCmdInvoker: content.issues.Action.Cmd ~=> Callback =
      Reusable.fn {
        case \/-(cmd)      => updateContentCmdInvoker(cmd)
        case -\/(-\/(cmd)) => manualIssueCmdInvoker(cmd)
        case -\/(\/-(cmd)) => updateConfigCmdInvoker(cmd)
      }

    private val savedViewFeatureStatic: SavedViewFeature.Static =
      SavedViewFeature.Static(
        stateAccess                    = $.zoomStateL(stateLensSavedViewStateAndFilterDead),
        pxProject                      = pxProject,
        pxFilterDead                   = pxFilterDead,
        pxFilterCompilerFromFilterDead = pxFilterCompilerFromFilterDead,
        confirmJs                      = confirmJs,
        promptJs                       = promptJs,
        savedViewAsyncW                = savedViewAsyncW,
        savedViewIO                    = sspUpdateSavedViews,
      )

    private val issuesPage = content.issues.IssuesPage.StaticProps(
      pxProject,
      pxRenderFeature,
      pxPlainText,
      pxProjectWidgets,
      pxFilterCompilerHideDead,
      routerCtl,
      updateConfigOrContentCmdInvoker)

    private val issuesPageSS =
      StateSnapshot.withReuse.zoomL(State.issuesPage).prepareVia($)

    private val reqTable = ReqTablePage(
      ReqTablePage.StaticProps(
        stateAccess            = $ zoomStateL State.reqTable,
        savedViewStatic        = savedViewFeatureStatic,
        pxTextSearch           = pxTextSearch,
        pxProjectWidgets       = pxProjectWidgets,
        pxFilterCompilerFromFD = pxFilterCompilerFromFilterDead,
        reqDetailRC            = reqDetailRC,
        toast                  = toast,
        updateIO               = sspUpdateContent,
        rowAsyncW              = rowAsyncW.mapKey(content.reqtable.Row.SourceId.ToEditorRow.reverse),
      ))

    private val pxReqDetailId = Px[Option[ReqId]](None).withReuse.manualUpdate

    private val pxReqDetailReqProps: Px[Option[State => ReqDetail.ReqProps]] =
      for {
        editability   <- pxEditEditability
        reqDetailId   <- pxReqDetailId
        project       <- pxProject
        vrdc          <- pxViewReqDataCache
      } yield reqDetailId.map { id =>
        val row = EditorFeature.RowKey.req(id)
        val aw  = editAsyncW(row).mapKey(AsyncKey.ToReqDetail)
        val ew  = editW.forReq(id)
        val ctx = ProjectText.Context.Req(id)
        val pt  = PlainText.ForProject(project, ctx)
        val vrc = ViewReqCache(vrdc, pt)
        val rff = RenderFeature.ToText.ReqCtx.ApplicableOption.prepare(project, vrc, pt)

        (s: State) => {
          val as = s.editAsync.toRead
          val ar = as(row).mapKey(AsyncKey.ToReqDetail)
          val af = aw.toReadWrite(ar)
          val rf = rff(s.filterDead)
          val er = EditorFeature.Read.ForProject(s.edit, rf, editability, as.mapKey1(AsyncKey.ToEditor)).forReq(id)
          val ef = EditorFeature.ReadWrite.ForFields(er, ew)
          ReqDetail.ReqProps(ef, af)
        }
      }

    def reqDetailReqPropsFn(s: State) = (id: ReqId) => {
      pxReqDetailId.set(Some(id))
      pxReqDetailReqProps.value().get(s)
    }

    private val webWorkerClient: WebWorkerClient.Instance =
      WebWorkerClient.Instance

    private val reqDetail = ReqDetail(ReqDetail.StaticProps(
      updateIO              = sspUpdateContent,
      reqDetailRC           = reqDetailRC,
      webWorker             = webWorkerClient,
      pxProjectAndOrd       = pxProjectAndOrd,
      pxViewReqDataCache    = pxViewReqDataCache,
      pxTextSearch          = pxTextSearch,
      pxProjectWidgetsNoCtx = pxProjectWidgets,
    ))

    private val reqDetailSetState: Reusable[SetStateFnPure[ReqDetail.State]] =
      Reusable.fn.state($ zoomStateL State.reqDetail).setStateFn

    private object specialRouterCtl extends SpecialRouterCtl {
      override val general = routerCtl

      override def reqTableWithFilter(fd: FilterDead, filter: => Filter.Valid): router.RouterCtl[Unit] = {
        def mod(p: Project, s: State): State =
          State.savedViews.modify(
            _.modifyView(p, fd, updateFilterText = true)(_.withFilter(Some(filter)))
          )(s).setFilterDead(fd, p)
        def setReqTableView: Callback =
          for {
            p <- pxProject.toCallback
            _ <- $.modState(mod(p, _))
          } yield ()
        routerCtl.onSet(setReqTableView >> _).contramap(_ => Page.ReqTable)
      }
    }

    private val pxUsage: Px[Usage] =
      pxProject.map(new Usage(_, specialRouterCtl))

    lazy val projectNameAF =
      AsyncFeature.Write.D0[ErrorMsg](
        Reusable.fn((s: AsyncFeature.State.D0[ErrorMsg]) =>
          $.modState(State.projectName.modify(ProjectItem.WithEditableName.State setAsync s))))

    private val setProjectNameIO: String => Callback = {
      newName => {
        def close = $.modState(State.projectName set None)
        def save = projectNameAF(sspProjectNameSet(newName).rightFlatTap(_ => close.asAsyncCallback))
        pxProject.toCallback >>= (p => if (p.name ==* newName) close else save)
      }
    }

    def render(p: Props, s: State): VdomElement = {
      lazy val editAsyncState = s.editAsync.toRead
      def createR          = CreateFeature.Read.ForProject(s.create, pxCreateEditability.value(), s.createAsync.toRead)
      def createRW         = createW.toReadWrite(createR)
      def editR            = EditorFeature.Read.ForProject(s.edit, renderFeature, pxEditEditability.value(), editAsyncState.mapKey1(AsyncKey.ToEditor))
      def editRW           = editW.toReadWrite(editR)
      def filterDeadSS     = StateSnapshot.withReuse(s.filterDead)(setFilterDead)
      def project          = unsafeProject()
      def projectAndOrd    = pxProjectAndOrd.value()
      def projectWidgets   = pxProjectWidgets.value.value()
      def renderFeature    = pxRenderFeatureText.value()(s.filterDead)
      def savedViewFeature = SavedViewFeature(savedViewFeatureStatic, s.savedViews, project, s.filterDead)
      def usage            = pxUsage.value()
      // def previewRW = previewW.toReadWrite(s.preview)

      val body: VdomElement = p.page match {

        case Page.Index =>
          val lookup = ReqLookupPrompt.Props(
            StateSnapshot.zoomL(State.reqLookup)(s).setStateVia($),
            Allow when _.lookup(project).isRight,
            e => routerCtl.set(Page.ReqDetail(e)))

          val index = ProjectIndex.Props(project.issues.count, lookup, routerCtl)

          val pname = ProjectItem.WithEditableName.Props(
            cbProjectMetaData.runNow(),
            StateSnapshot.zoomL(State.projectName)(s).setStateVia($),
            setProjectNameIO)

          ProjectHome.Props(pname, index).render

        case Page.Issues =>
          val state    = issuesPageSS(s)
          val creator  = createRW(CreateFeature.RowKey.ManualIssue)
          val cmdAsync = s.manualIssueCmdAsync.toRead
                           .either(s.updateConfigCmdAsync.toRead)
                           .either(s.updateContentCmdAsync.toRead)
          val p = content.issues.IssuesPage.Props(state, creator, editRW, cmdAsync)
          issuesPage.component(p)

        case Page.CfgFields =>
          config.fields.FieldConfig.Props(
            project = project,
            pw      = projectWidgets,
            state   = StateSnapshot.zoomL(State.fieldConfig)(s).setStateVia($),
            ssp     = sspUpdateConfig,
            async   = AsyncFeature.ReadWrite.D0(fieldConfigAsyncW, s.fieldConfigAsync),
            router  = routerCtl,
            toast   = toast,
            usage   = usage,
          ).render

        case Page.CfgIssues =>
          config.issues.IssueConfig.Props(
            project = project,
            pw      = projectWidgets,
            state   = StateSnapshot.zoomL(State.customIssueTypeConfig)(s).setStateVia($),
            ssp     = sspUpdateConfig,
            async   = AsyncFeature.ReadWrite.D0(customIssueTypeConfigAsyncW, s.customIssueTypeConfigAsync),
            router  = routerCtl,
            toast   = toast,
            usage   = usage,
          ).render

        case Page.CfgReqTypes =>
          config.reqtypes.ReqTypeConfig.Props(
            project = project,
            pw      = projectWidgets,
            state   = StateSnapshot.zoomL(State.reqTypeConfig)(s).setStateVia($),
            ssp     = sspUpdateConfig,
            async   = AsyncFeature.ReadWrite.D0(reqTypeConfigAsyncW, s.reqTypeConfigAsync),
            confirm = confirmJs,
            toast   = toast,
            usage   = usage,
          ).render

        case Page.CfgTags =>
          config.tags.TagConfig.Props(
            project = project,
            pw      = projectWidgets,
            state   = StateSnapshot.zoomL(State.tagConfig)(s).setStateVia($),
            ssp     = sspUpdateConfig,
            async   = AsyncFeature.ReadWrite.D0(tagConfigAsyncW, s.tagConfigAsync),
            toast   = toast,
            usage   = usage,
          ).render

        case Page.ReqTable =>
          val rowAsync = editAsyncState
            .mapKey2(content.reqtable.Row.SourceId.ToEditorRow.reverse)
            .withKey1(AsyncKey.WholeReq)
          reqTable(
            ReqTablePage.Props(
              create     = createRW,
              editor     = editRW,
              savedViews = savedViewFeature,
              rowAsync   = rowAsync,
              filterDead = s.filterDead,
              state      = s.reqTable))

        case Page.ReqDetail(pubid) =>
          val props = ReqDetail.DynamicProps(
            extPubid   = pubid,
            filterDead = filterDeadSS,
            reqProps   = reqDetailReqPropsFn(s),
            editorUCS  = editRW.forUseCaseSteps,
            state      = StateSnapshot.withReuse(s.reqDetail)(reqDetailSetState))
          reqDetail(props)

        case Page.ReqGraph =>
          content.reqgraph.ReqGraphPage.Props(
            projectAndOrd    = projectAndOrd,
            plainText        = pxPlainText.value(),
            reqDetailRC      = reqDetailRC,
            webWorker        = webWorkerClient,
            savedViewFeature = savedViewFeature,
          ).render
      }

      State.recorder.record(s)

      Layout.Props(
        username            = initPageData.username,
        project             = cbProjectMetaData.runNow(),
        unsavedChanges      = pxLayoutUnsavedChangeData.value(),
        connectionStatus    = global.connectedStatusHub.unsafeGet(),
        setConnectionStatus = global.setConnectionStatus,
        reauthModal         = global.reauthModal,
        feedbackModal       = feedbackModal,
        toast               = StateSnapshot.zoomL(State.toast)(s).setStateVia($),
        rc                  = routerCtl,
        page                = p.page,
        content             = body,
      ).render
    }

    def onMount: Callback = {
      val sendProjectToWebWorker: Callback =
        pxProjectAndOrd.toCallback.asAsyncCallback
          .flatMap(p => webWorkerClient.post(WebWorkerCmd.SetProject(p)))
          .toCallback

      val installHooks: Callback =
        Callback {
          window.onbeforeunload = event => {
            val u = pxUnsavedChanges.value()
            if (u.nonEmpty) {
              event.preventDefault()
              event.returnValue = ""
              ""
            } else
              ()
          }
        }

      sendProjectToWebWorker >> installHooks
    }

    def onProjectChange(u: ProjectState.Update): Callback = {
      val updateWebWorker: Callback =
        Callback.traverseOption(VerifiedEvent.NonEmptySeq.maybe(u.newlyAppliedEvents))(ves =>
          webWorkerClient.post(WebWorkerCmd.UpdateProject(ves)).toCallback)

      updateWebWorker >> $.forceUpdate
    }

    def onConnectionStatusChange(c: ConnectionStatus): Callback = {
      val msg = c match {
        case ConnectionStatus.Connected    => "Connection established"
        case ConnectionStatus.Disconnected => "Connection lost"
      }
      $.forceUpdate(toast.add(msg))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .initialStateCallback(State.recorder.getOrElseCB(pxProject.toCallback.map(State.init)))
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount)
    .configure(Listenable.listen(_ => global, _.backend.onProjectChange))
    .configure(Listenable.listen(_ => global.connectedStatusHub, _.backend.onConnectionStatusChange))
    .build
}
