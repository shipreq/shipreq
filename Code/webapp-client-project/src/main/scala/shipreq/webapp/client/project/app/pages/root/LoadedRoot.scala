package shipreq.webapp.client.project.app.pages.root

import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.PackageBase._
import monocle.Lens
import org.scalajs.dom.window
import shipreq.base.util.{Allow, ErrorMsg}
import shipreq.webapp.base.data.ProjectRole
import shipreq.webapp.base.feature.AsyncFeature.Implicits._
import shipreq.webapp.base.feature._
import shipreq.webapp.base.lib.{ConfirmJs, PromptJs}
import shipreq.webapp.base.protocol.ajax.CommonProtocolsJs
import shipreq.webapp.base.ui.semantic.Menu
import shipreq.webapp.base.ui.widgets.FeedbackModal
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.client.project.app._
import shipreq.webapp.client.project.app.pages._
import shipreq.webapp.client.project.app.pages.content.reqdetail.ReqDetail
import shipreq.webapp.client.project.app.pages.content.reqtable.ReqTablePage
import shipreq.webapp.client.project.app.pages.root.LoadedRoot._
import shipreq.webapp.client.project.app.pages.root.Routes.{Page, RouterCtl}
import shipreq.webapp.client.project.app.state._
import shipreq.webapp.client.project.feature.{Usage, _}
import shipreq.webapp.client.project.util.DataReusability._
import shipreq.webapp.client.project.widgets._
import shipreq.webapp.member.feature.PreviewFeature
import shipreq.webapp.member.project.data.{FilterDead, HideDead, Project, ProjectConfig, ReqId}
import shipreq.webapp.member.project.filter.Filter
import shipreq.webapp.member.project.library.ProjectLibrary
import shipreq.webapp.member.project.protocol.websocket._
import shipreq.webapp.member.project.text.{PlainText, ProjectText, TextSearch}
import shipreq.webapp.member.protocol.entrypoint.ProjectSpaEntryPoint
import shipreq.webapp.member.ui.{OptionalFullscreen, ProjectItem, Toast}

object LoadedRoot {
  final case class Props(page: Page, routerCtl: RouterCtl)
}

final class LoadedRoot(initPageData      : ProjectSpaEntryPoint.InitDataWithoutEncKey,
                       global            : Global,
                       confirmJs         : ConfirmJs,
                       promptJs          : PromptJs,
                       optionalFullscreen: OptionalFullscreen,
                       webWorkerClient   : WebWorkerClient.Instance,
                       accessHandler     : AccessHandler,
                      ) {

  implicit def webStorage = global.localStorage

  val pxProject       = global.pxProject
  def unsafeProject() = global.unsafeProject()
  def unsafeSupp()    = global.unsafeSupp()

  private val stateLensFilterDead =
    Lens[State, FilterDead](_._filterDead)(fd => _.setFilterDead(fd, unsafeProject()))

  private val stateLensSavedViewStateAndFilterDead =
    Lens[State, (SavedViewFeature.State, FilterDead)](
      s => (s.savedViews, s.filterDead))(
      n => _.copy(savedViews = n._1).setFilterDead(n._2, unsafeProject()))

  final class Backend($: BackendScope[Props, State]) extends OnUnmount {
    import global.cbProjectMetaData

    private val sspCreateContent       = Reusable.byRef(global.sspCreateContent)
    private val sspUpdateConfig        = global.sspUpdateConfig
    private val sspUpdateConfigE       = global.sspUpdateConfig.map(_.events)
    private val sspUpdateContent       = global.sspUpdateContent.map(_.events)
    private val sspProjectNameSet      = global.sspProjectNameSet.map(_.events)
    private val sspUpdateSavedViews    = global.sspUpdateSavedViews.map(_.events)
    private val sspUpdateManualIssues  = Reusable.byRef(global.sspUpdateManualIssues)
    private val sspUpdateManualIssuesE = global.sspUpdateManualIssues.map(_.events)
    private val sspUpdateAccess        = global.sspUpdateAccess

    private val feedbackModal: FeedbackModal = {
      val projectMetadata = global.projectMetadata(initPageData.projectId)
      val clientMetadata  = CommonProtocolsJs.Metadata.client(initPageData.username, projectMetadata)
      FeedbackModal(clientMetadata)
    }

    // This never changes
    private val routerCtl = $.props.runNow().routerCtl
    private val routerCtlEP = routerCtl.contramap(Page.ReqDetail.apply)

    private val toast = Toast($.zoomStateL(State.toast))

    private val pxState =
      Px.state($).withReuse.autoRefresh

    private val pxFilterDead =
      pxState.map(_.filterDead).withReuse

    private val pxProjectAccess =
      pxProject.map(_.access).withReuse

    private val pxProjectRole =
      pxProjectAccess.map(_(initPageData.userId)).withReuse

    private val pxUseCases =
      pxProject.map(_.content.reqs.useCases).withReuse

    private val pxProjectConfig: Px[ProjectConfig] =
      pxProject.map(_.config).withReuse

    private val pxPlainText: Px[PlainText.ForProject.NoCtx] =
      pxProject.map(PlainText.ForProject.noCtx.apply)

    private val pxTextSearch: Px[TextSearch] =
      Px.apply2(pxProject, pxPlainText)(TextSearch.apply)

    private val pxViewTags: Px[ViewTags] =
      pxProject.map(ViewTags.apply)

    private val pxViewTagsForReq: Px[Reusable[FilterDead => ReqId => ViewTags.ForReq[VdomTag]]] =
      pxViewTags.map { vt =>
        Reusable.byRef(vt.forReq)
      }

    private val pxProjectWidgets: Reusable[Px[ProjectWidgets.NoCtx]] =
      Reusable byRef Px.apply3(pxProject, pxPlainText, pxViewTags)(ProjectWidgets(_, _, _, routerCtlEP, webWorkerClient))

    private val pxEditEditability: Px[EditorFeature.Editability.ForProject] =
      pxProject.map(EditorFeature.Editability.apply)

    private val pxUnsavedChangesInput: Px[UnsavedChanges.Input] =
      Px.apply6(
        pxState,
        pxEditEditability,
        pxProject,
        pxTextSearch,
        pxProjectWidgets,
        pxUseCases)(
        UnsavedChanges.Input.apply)

    private val pxUnsavedChanges: Px[UnsavedChanges] =
      pxUnsavedChangesInput.map(UnsavedChanges.determine).flatMap(Px.callback(_).withReuse.autoRefresh)

    private val pxLayoutUnsavedChangeData: Px[Layout.UnsavedChangeData] =
      for {
        c <- pxUnsavedChanges
        p <- pxProject
      } yield Layout.UnsavedChangeData.derive(c, p, routerCtl)

    private val setFilterDead: Reusable[SetStateFnPure[FilterDead]] =
      Reusable.fn.state($ zoomStateL stateLensFilterDead).setStateFn

    private val setNewReqButton: Reusable[SetStateFnPure[NewReqButton.State]] =
      Reusable.fn.state($ zoomStateL State.newReqButton).setStateFn

    private val pxViewReqDataCache: Px[ViewReqDataCache] =
      pxProject.map(ViewReqDataCache.apply)

    private val pxViewReqCache: Px[ViewReqCache.ToVdom[ProjectText.Context.None]] =
      Px.apply3(pxViewReqDataCache, pxProjectWidgets, pxViewTagsForReq)(ViewReqCache.apply)

    private val pxViewReqCacheText: Px[ViewReqCache[ProjectText.Context.None, String]] = {
      for {
        c  <- pxViewReqDataCache
        pt <- pxPlainText
        vt <- pxViewTags
      } yield ViewReqCache(c, pt, vt.forPlainTextViewReqCache)
    }

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
      PreviewFeature.Write.Composite(Reusable.byRef($ zoomStateL State.preview))

    private val previewCreateW =
      previewW.mapId(PreviewId.ToCreate)

    private val previewEditorW =
      previewW.mapId(PreviewId.ToEditor)

    private val newReqAsyncW: AsyncFeature.Write.D0[ErrorMsg] =
      AsyncFeature.Write.D0.init($ zoomStateL State.newReqAsync)

    private val createW: CreateFeature.Write.ForProject =
      CreateFeature.Write.ForProject(
        Reusable.byRef($ zoomStateL State.create),
        newReqAsyncW,
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
          confirmJs,
          sspUpdateContent,
          sspUpdateManualIssuesE,
          optionalFullscreen,
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

    private val accessPageAsyncW: AsyncFeature.Write.D1[admin.access.AccessPage.AsyncKey, ErrorMsg] =
      AsyncFeature.Write.D1.init($ zoomStateL State.accessPageAsync)

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

    private val reqSearch =
      new ReqSearch(
        ReqSearch.StaticProps(
          pxProject,
          pxProjectConfig,
          pxFilterCompilerFromFilterDead,
          routerCtlEP,
        )
      )

    private val reqSearchSS: Reusable[StateSnapshot.SetFn[ReqSearch.State]] =
      Reusable.byRef((os, cb) => $.modStateOption(s => os.map(r => s.copy(reqSearch = r)), cb))

    private val pxReqSearchState: Px[ReqSearch.State] =
      Px.state($).map(_.reqSearch).withReuse.autoRefresh

    private val pxReqSearchProps: Px[ReqSearch.Props] =
      for {
        s  <- pxReqSearchState
        fd <- pxFilterDead
        pw <- pxProjectWidgets.value
      } yield ReqSearch.Props(
        state      = StateSnapshot.withReuse(s)(reqSearchSS),
        filterDead = fd,
        pw         = pw,
      )

    private val pxLayoutMenuMiddle: Px[Reusable[List[Menu.Item]]] =
      pxReqSearchProps.map { p =>
        val items = reqSearch.menuItem(p) :: Nil
        Reusable.byRef(items)
      }

    private val issuesPage = content.issues.IssuesPage.StaticProps(
      pxProject,
      pxRenderFeature,
      pxPlainText,
      pxProjectWidgets,
      pxTextSearch,
      pxFilterCompilerHideDead,
      routerCtl,
      updateConfigOrContentCmdInvoker)

    private val issuesPageSS =
      StateSnapshot.withReuse.zoomL(State.issuesPage).prepareVia($)

    private val reqTable = ReqTablePage(
      ReqTablePage.StaticProps(
        stateAccess            = $ zoomStateL State.reqTable,
        savedViewStatic        = savedViewFeatureStatic,
        pxPlainText            = pxPlainText,
        pxTextSearch           = pxTextSearch,
        pxProjectWidgets       = pxProjectWidgets,
        pxFilterCompilerFromFD = pxFilterCompilerFromFilterDead,
        assetManifest          = initPageData.assetManifest,
        reqDetailRC            = routerCtlEP,
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
        vt            <- pxViewTags
        vrdc          <- pxViewReqDataCache
      } yield reqDetailId.map { id =>
        val row = EditorFeature.RowKey.req(id)
        val aw  = editAsyncW(row).mapKey(AsyncKey.ToReqDetail)
        val ew  = editW.forReq(id)
        val ctx = ProjectText.Context.Req(id)
        val pt  = PlainText.ForProject(project, ctx)
        val vrc = ViewReqCache(vrdc, pt, vt.forPlainTextViewReqCache)
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

    private val reqDetail = ReqDetail(ReqDetail.StaticProps(
      sspUpdateContent      = sspUpdateContent,
      sspCreateContent      = sspCreateContent,
      reqDetailRC           = routerCtlEP,
      webWorker             = webWorkerClient,
      pxProject             = pxProject,
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

    private val pxPreviewState =
      pxState.map(_.preview).withReuse

    private val pxCreatePreviewRW =
      pxPreviewState.map { s =>
        val read = PreviewFeature.Read.Composite(s).mapId(PreviewId.ToCreate)
        previewCreateW.toReadWrite(read)
      }.withReuse

    private val pxEditorPreviewRW =
      pxPreviewState.map { s =>
        val read = PreviewFeature.Read.Composite(s).mapId(PreviewId.ToEditor)
        previewEditorW.toReadWrite(read)
      }.withReuse

    private val pxEditorArgs: Px[EditorFeature.EditorArgs.ForAny] =
      for {
        previewRW      <- pxEditorPreviewRW
        project        <- pxProject
        projectWidgets <- pxProjectWidgets
        textSearch     <- pxTextSearch
      } yield EditorFeature.EditorArgs.ForAny(
        previewRW      = previewRW,
        project        = project,
        plainTextNoCtx = projectWidgets.plainText,
        projectWidgets = projectWidgets,
        textSearch     = textSearch,
    )

    lazy val projectNameAF =
      AsyncFeature.Write.D0[ErrorMsg](
        Reusable.fn((s: AsyncFeature.State.D0[ErrorMsg]) =>
          $.modState(State.projectName.modify(ProjectItem.WithEditableName.State setAsync s))))

    private val setProjectNameIO: String => Callback = {
      newName => {
        def close = $.modState(State.projectName replace None)
        def save = projectNameAF(sspProjectNameSet(newName).rightFlatTap(_ => close.asAsyncCallback))
        pxProject.toCallback >>= (p => if (p.name ==* newName) close else save)
      }
    }

    private val someEdgeEditorArgs: Some[ImplicationGraph.EdgeEditor.Args] =
      Some(ImplicationGraph.EdgeEditor.Args(
        ssp    = sspUpdateContent,
        asyncW = updateContentCmdAsyncW,
        asyncR = Reusable.callbackByRef($.state.map(_.updateContentCmdAsync.toRead)),
      ))

    def render(p: Props, s: State): VdomElement = {
      lazy val editAsyncState = s.editAsync.toRead
      def createR          = CreateFeature.Read.ForProject(s.create, pxCreateEditability.value(), s.newReqAsync)
      def createRW         = createW.toReadWrite(createR)
      def editR            = EditorFeature.Read.ForProject(s.edit, renderFeature, pxEditEditability.value(), editAsyncState.mapKey1(AsyncKey.ToEditor))
      def editRW           = editW.toReadWrite(editR)
      def filterDeadSS     = StateSnapshot.withReuse(s.filterDead)(setFilterDead)
      def project          = unsafeProject()
      def projectWidgets   = pxProjectWidgets.value.value()
      def renderFeature    = pxRenderFeatureText.value()(s.filterDead)
      def savedViewFeature = SavedViewFeature(savedViewFeatureStatic, s.savedViews, project, s.filterDead)
      def usage            = pxUsage.value()
      def createPreviewRW  = pxCreatePreviewRW.value()
      def editorArgs       = pxEditorArgs.value()
      def onlyAdminCanEdit = ProjectRole.Admin.isSatisfiedBy(pxProjectRole.value())

      val body: VdomElement = p.page match {

        case Page.Index =>
          val lookup = ReqLookupPrompt.Props(
            StateSnapshot.zoomL(State.reqLookup)(s).setStateVia($),
            Allow when _.lookup(project).isRight,
            e => routerCtl.set(Page.ReqDetail(e)))

          val index = ProjectIndex.Props(project.issues.count, lookup, routerCtl)

          val pname = ProjectItem.WithEditableName.Props(
            cbProjectMetaData.runNow(),
            onlyAdminCanEdit,
            StateSnapshot.zoomL(State.projectName)(s).setStateVia($),
            setProjectNameIO)

          ProjectHome.Props(pname, index).render

        case Page.Issues =>
          val state    = issuesPageSS(s)
          val creator  = createRW(CreateFeature.RowKey.ManualIssue)
          val cmdAsync = s.manualIssueCmdAsync.toRead
                           .either(s.updateConfigCmdAsync.toRead)
                           .either(s.updateContentCmdAsync.toRead)
          val p = content.issues.IssuesPage.Props(state, creator, editRW, editorArgs, createPreviewRW, cmdAsync)
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
              create          = createRW,
              createPreviewRW = createPreviewRW,
              editor          = editRW,
              editorArgs      = editorArgs,
              savedViews      = savedViewFeature,
              rowAsync        = rowAsync,
              filterDead      = s.filterDead,
              state           = s.reqTable))

        case Page.ReqDetail(pubid) =>
          val props = ReqDetail.DynamicProps(
            extPubid    = pubid,
            filterDead  = filterDeadSS,
            reqProps    = reqDetailReqPropsFn(s),
            editorUCS   = editRW.forUseCaseSteps,
            editorArgs  = editorArgs,
            state       = StateSnapshot.withReuse(s.reqDetail)(reqDetailSetState),
            newReqState = StateSnapshot.withReuse(s.newReqButton)(setNewReqButton),
            newReqAsync = AsyncFeature.ReadWrite.D0(newReqAsyncW, s.newReqAsync),
          )
          reqDetail(props)

        case Page.ReqGraph =>
          content.reqgraph.ReqGraphPage.Props(
            project          = project,
            plainText        = pxPlainText.value(),
            reqDetailRC      = routerCtlEP,
            webWorker        = webWorkerClient,
            savedViewFeature = savedViewFeature,
            edgeEditorArgs   = someEdgeEditorArgs,
          ).render

        case Page.Access =>
          admin.access.AccessPage.Props(
            userId          = initPageData.userId,
            access          = project.access,
            rolodex         = unsafeSupp().rolodex,
            editability     = onlyAdminCanEdit,
            state           = StateSnapshot.zoomL(State.access)(s).setStateVia($),
            confirmJs       = confirmJs,
            sspUpdateAccess = sspUpdateAccess,
            async           = AsyncFeature.ReadWrite.D1(accessPageAsyncW, s.accessPageAsync.toRead),
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
        assetManifest       = initPageData.assetManifest,
        feedbackModal       = feedbackModal,
        toast               = StateSnapshot.zoomL(State.toast)(s).setStateVia($),
        rc                  = routerCtl,
        menuMiddle          = pxLayoutMenuMiddle.value(),
        page                = p.page,
        content             = body,
      ).render
    }

    def onMount: Callback =
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

        window.onunload = _ => {
          webWorkerClient.close.runNow()
        }
      }

    def onProjectChange(u: ProjectLibrary.Update): Callback = {
      val access = u.newLibrary.latest.access(initPageData.userId)
      if (access.isEmpty)
        accessHandler.onRevoke
      else
        $.forceUpdate
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
