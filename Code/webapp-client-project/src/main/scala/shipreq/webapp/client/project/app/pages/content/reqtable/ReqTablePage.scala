package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.univeq._
import monocle.Optional
import monocle.std.option.pSome
import monocle.macros.Lenses
import org.scalajs.dom.document
import scalacss.ScalaCssReact._
import shipreq.base.util.{Allow, ErrorMsg, Optics, Valid}
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.{SavedViewCmd, UpdateContentCmd}
import shipreq.webapp.base.text.TextSearch
import shipreq.webapp.base.ui.{BaseStyles, NoContentMessage, Toast}
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.client.project.app.Style.reqtable.{page => *}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.widgets.{FilterDeadButton, FilterEditor, ProjectWidgets}

object ReqTablePage {

  def apply(staticProps: StaticProps) =
    ScalaComponent.builder[Props]
      .backend(new Backend(staticProps, _))
      .renderBackend
      .componentDidMount(_.backend.unfocus)
      .build

  final case class StaticProps(stateAccess           : StateAccessPure[(FilterDead, State)],
                               pxProject             : Px[Project],
                               pxTextSearch          : Px[TextSearch],
                               pxProjectWidgets      : Reusable[Px[ProjectWidgets.NoCtx]],
                               pxFilterCompilerFromFD: Px[FilterDead => Filter.Valid.Compiler],
                               reqDetailRC           : RouterCtl[ExternalPubid],
                               toast                 : Toast,
                               updateIO              : ServerSideProcInvoker[UpdateContentCmd, ErrorMsg, Any],
                               savedViewIO           : ServerSideProcInvoker[SavedViewCmd, ErrorMsg, VerifiedEvent.Seq],
                               rowAsyncW             : AsyncFeature.Write.D1[Row.SourceId, ErrorMsg],
                               savedViewAsyncW       : AsyncFeature.Write.D0[ErrorMsg]) {

    val stateAccessS: StateAccessPure[State] =
      stateAccess.zoomStateL(Optics.lensTuple2_2)
  }

  final case class Props(create        : CreateFeature.ReadWrite.ForProject,
                         editor        : EditorFeature.ReadWrite.ForProject,
                         rowAsync      : AsyncFeature.Read.D1[Row.SourceId, ErrorMsg],
                         savedViewAsync: AsyncFeature.Read.D0[ErrorMsg],
                         filterDead    : FilterDead,
                         state         : State)

  @Lenses
  final case class State(view     : SavedViewLogic.State,
                         filter   : FilterEditor.State,
                         selection: RowSelection,
                         newStuff : NewStuff.State,
                         modal    : Modal.State) {

    def setFilterDead(fd: FilterDead, p: Project): State = {
      val showingBuiltInDefault = p.reqtableViews.isEmpty && view.manualView.isEmpty
      if (showingBuiltInDefault)
        this
      else {
        val v1 = view.activeView(p.reqtableViews, fd)
        val v2 = v1.copy(filterDead = fd)
        State.setModifiedView(p, updateFilterText = false)(v2)(this)
      }
    }
  }

  object State {
    private def init: State =
      State(
        SavedViewLogic.State.init,
        FilterEditor.State.init,
        Selection.empty,
        NewStuff.State.init,
        Modal.none)

    def init(p: Project): State =
      updateFilterText(p)(init)

    val manualView: Optional[State, View] =
      view ^|-> SavedViewLogic.State.manualView ^<-? pSome

    def modifyView(project           : Project,
                   filterDeadFallback: FilterDead,
                   updateFilterText  : Boolean)
                  (modify            : View => View): State => State =
      s => {
        val newView = modify(s.view.activeView(project.reqtableViews, filterDeadFallback))
        setModifiedView(project, updateFilterText)(newView)(s)
      }

    def setModifiedView(project         : Project,
                        updateFilterText: Boolean)
                       (newView         : View): State => State =
      runSavedViewAction(project, updateFilterText)(SavedViewLogic.Action.Modify(newView))

    def runSavedViewAction(project         : Project,
                           updateFilterText: Boolean)
                          (action          : SavedViewLogic.Action): State => State = {
      val modSVS   = SavedViewLogic.Action.interpret(project.reqtableViews)(action)
      val modState = view.modify(modSVS)
      if (updateFilterText)
        modState andThen this.updateFilterText(project)
      else
        modState
    }

    def updateFilterText(project: Project): State => State = s => {
      val filterDeadFallback = ShowDead // This doesn't impact filter text
      val v = s.view.activeView(project.reqtableViews, filterDeadFallback)
      val txt = v.filter.fold("")(Filter.Valid.toText(project.config, _))
      filter.set(FilterEditor.State(txt, Valid))(s)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed abstract class Mode
  object Mode {
    case object EmptyProject         extends Mode
    case object NoContentCosHideDead extends Mode
    case object NoContentCosFilter   extends Mode
    case object HasContent           extends Mode
    implicit def univEq: UnivEq[Mode] = UnivEq.derive
  }

  final class Backend(sp: StaticProps, $: BackendScope[Props, Unit]) {
    import sp._

    val modNewStuff : ModFn[NewStuff.State] = Reusable.fn.state(stateAccessS zoomStateL State.newStuff).modStateFn
    val setSelection: SetFn[RowSelection  ] = Reusable.fn.state(stateAccessS zoomStateL State.selection).setStateFn
    val setModal    : SetFn[Modal.State   ] = Reusable.fn.state(stateAccessS zoomStateL State.modal).setStateFn

    private val manualPxs = Px.ManualCollection()

    private def pxProps[A: Reusability](f: Props => A): Px.ThunkM[A] = {
      val px = Px.props($).map(f).withReuse.manualRefresh
      manualPxs.add(px)
      px
    }

    private def pxState[A: Reusability](f: ((FilterDead, State)) => A): Px.ThunkM[A] = {
      val px = Px.state(stateAccess).map(f).withReuse.manualRefresh
      manualPxs.add(px)
      px
    }

    val pxViewState : Px[SavedViewLogic.State] = pxProps(_.state.view)
    val pxFilterDead: Px[FilterDead]           = pxState(_._1)
    val pxSelection : Px[RowSelection]         = pxProps(_.state.selection)

    val pxSavedViews: Px[SavedViews.Optional] =
      pxProject.map(_.reqtableViews).withReuse

    val pxColumnPlusAll: Px[ColumnPlus.All] =
      (for {
        p  <- pxProject
        fd <- pxFilterDead
      } yield ColumnPlus.All(p, fd))
        .withReuse

    val pxActiveView: Px[View] =
      (for {
        vs <- pxViewState
        sv <- pxSavedViews
        fd <- pxFilterDead
        cs <- pxColumnPlusAll
      } yield vs.activeView(sv, fd).filterColumns(cs.containsColumn)
        ).withReuse

    val activeViewCB: Reusable[CallbackTo[View]] = {
      val f = pxActiveView.toCallback.toScalaFn
      Reusable.byRef(f).map(CallbackTo.lift)
    }

    val pxActiveColumns: Px[NonEmptyVector[Column]] =
      pxActiveView.map(_.columns).withReuse

    val pxActiveOrder: Px[SortCriteria] =
      pxActiveView.map(_.order).withReuse

    val modifyViewFn: ModFn[View] =
      Reusable.byRef(ModStateFn((mod, cb) =>
        for {
          p       <- pxProject.toCallback
          (_, s1) <- stateAccess.state
          fd      <- pxFilterDead.toCallback
          v1      = s1.view.activeView(p.reqtableViews, fd)
          v2      = mod(v1) getOrElse v1
          s2      = State.setModifiedView(p, updateFilterText = false)(v2)(s1)
          _       <- stateAccess.setState((v2.filterDead, s2), cb)
        } yield ()))

    val setFilterDeadFn: SetFn[FilterDead] =
      Reusable.byRef(SetStateFn((fdO, cb) =>
        fdO.fold(cb)(fd =>
          for {
            p       <- pxProject.toCallback
            (_, s1) <- stateAccess.state
            s2      = s1.setFilterDead(fd, p)
            _       <- stateAccess.setState((fd, s2), cb)
          } yield ())))

    val pxRows: Px[Vector[Row]] =
      for {
        p  <- pxProject
        v  <- pxActiveView
        pw <- pxProjectWidgets
        ts <- pxTextSearch
        fc <- pxFilterCompilerFromFD
      } yield Logic.rowsForTable(p, v, pw.plainText, ts, fc(v.filterDead))

    val pxRowIdsWithWholeRowAsync: Px[Set[Row.SourceId]] =
      pxProps(_.rowAsync.keySet)

    /** Rows which the user has selected that:
      * - are currently visible (i.e. ignoring filtered out)
      * - aren't currently busy with some async action (in which case the selection checkbox is replaced with a spinner)
      */
    val pxRowSelectionVisible: Px[RowSelectionVisible] =
      for {
        rs <- pxRows
        wr <- pxRowIdsWithWholeRowAsync
        s  <- pxSelection
      } yield
        s.updateBy(setSelection.map(_.setState)).legal(rs.iterator.map(_.sourceId).toSet &~ wr)

    val pxActiveColumnsPlus: Px[NonEmptyVector[ColumnPlus]] =
      (for {
        p  <- pxProject
        cs <- pxActiveColumns
      } yield ColumnPlus.forceNEV(ColumnPlus.byProject(p))(cs))
        .withReuse

    val pxColumnSelector: Px[VdomElement] =
      for {
        sel <- pxActiveColumns
        all <- pxColumnPlusAll
      } yield
        ColumnSelector.Props(sel, all, modifyViewFn.map(m => u => m.modState(_.withColumns(u)))).render

    val onFilterChange: FilterEditor.UpdateFn =
      (newState, newFilter, cb) =>
        for {
          p  <- pxProject.toCallback
          fd <- pxFilterDead.toCallback
          m1 = State.filter.set(newState)
          m2 = State.modifyView(p, fd, updateFilterText = false)(_.withFilter(newFilter))
          _ <- stateAccessS.modState(m1 compose m2, cb)
        } yield ()

    val pxTableContentStats: Px[TableContentStats] =
      for {
        p    <- pxProject
        rows <- pxRows
      } yield Logic.stats(p, rows)

    val pxPageSummary: Px[VdomElement] =
      for {
        fd    <- pxFilterDead
        stats <- pxTableContentStats
        sel   <- pxRowSelectionVisible
      } yield {
        // `legalSelection` because the same sourceId can appear more than once
        val totalSelected = sel.legalSelectionSize
        PageSummary.Props(stats, totalSelected, fd).render
      }

    val pxSortCriteriaEditor: Px[VdomElement] =
      for {
        o <- pxActiveOrder
        c <- pxColumnPlusAll
      } yield SortCriteriaEditor.Props(o, modifyViewFn.map(m => o => m.modState(View.order set o)), c).render

    val pxSelectionCtrls: Px[SelectionCtrls.Props] =
      for {
        project        <- pxProject
        projectWidgets <- pxProjectWidgets
        textSearch     <- pxTextSearch
        rows           <- pxRows
        sel            <- pxRowSelectionVisible
      } yield SelectionCtrls.Props(
        sel, rows, setModal.map(_.setState), project, projectWidgets, textSearch, updateIO, rowAsyncW)

    val pxSavedViewsMenu: Px[SavedViewLogic.Menu] =
      for {
        savedViews <- pxSavedViews
        viewState  <- pxViewState
        activeView <- pxActiveView
      } yield SavedViewLogic.menu(savedViews, viewState, activeView, activeViewCB)


    val reqTable = new Table(pxProjectWidgets)

    // Not Px because we don't want it to jitter.
    // Just choose a nice default once per view and stick with it.
    val defaultNewType: Option[CreateFeature.RowKey] = {
      val p = pxProject.value()
      MutableArray(p.config.reqTypes.all.iterator.filter(_.live is Live))
        .map(t => (t, p.reqTypeCount(t.reqTypeId).live))
        .sortBy(_._2)
        .array
        .lastOption
        .filter(_._2 > 0)
        .map(x => CreateFeature.RowKey.req(x._1.reqTypeId))
    }

    val runSavedViewAction: SavedViewLogic.Action ~=> Callback =
      Reusable.fn { action =>
        for {
          project <- pxProject.toCallback
          mod     = State.runSavedViewAction(project, true)(action)
          _       <- stateAccessS.modState(mod)
        } yield ()
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    def render(p: Props): VdomElement = {
      manualPxs.refresh()
      p.state.modal renderOrElse renderMain(p)
    }

    private val mainBase = <.main(BaseStyles.containerFull)

    def renderEmptyProject: VdomTag =
      NoContentMessage(
        "Welcome to your new project!",
        "Create new content using the button above.")

    def renderAllContentDead: VdomTag =
      NoContentMessage.becauseAllDead(
        TagMod(
          "Create new content (above) or enable display of dead content (via the ",
          Icon.TrashOutline.tag,
          "button in the top-right)."))

    def renderMain(p: Props): VdomElement = {
      val project           = pxProject.value()
      val activeView        = pxActiveView.value()
      val activeColumnsPlus = pxActiveColumnsPlus.value()
      val rows              = pxRows.value()
      val filterDead        = pxFilterDead.value()
      val projectWidgets    = pxProjectWidgets.value()
      val stats             = pxTableContentStats.value()

      val mode: Mode =
        if (rows.nonEmpty)
          Mode.HasContent
        else if (stats.reqsInProject.all ==* 0)
          Mode.EmptyProject
        else if (filterDead.is(HideDead) && stats.reqsInProject.live ==* 0)
          Mode.NoContentCosHideDead
        else
          Mode.NoContentCosFilter

      val filterDeadButton: VdomElement =
        if (mode ==* Mode.EmptyProject)
          FilterDeadButton.ForceHideDead
        else
          FilterDeadButton.Component(StateSnapshot.withReuse(activeView.filterDead)(setFilterDeadFn))

      val newFormColumns: NonEmptyVector[ColumnPlus] =
        mode match {
          case Mode.HasContent
             | Mode.NoContentCosFilter
             | Mode.NoContentCosHideDead => activeColumnsPlus
          case Mode.EmptyProject         => NonEmptyVector.one(ColumnPlus.title)
        }

      val newStuff = new NewStuff(
        p.state.newStuff,
        modNewStuff,
        reqDetailRC,
        projectWidgets,
        toast,
        project.config.reqTypes,
        Allow when activeView.viewCodeGroups,
        defaultNewType,
        p.create,
        newFormColumns,
      )

      val savedViews = SavedViewsUI.Props(
        pxSavedViewsMenu.value(),
        AsyncFeature.ReadWrite.D0(savedViewAsyncW, p.savedViewAsync),
        runSavedViewAction,
        savedViewIO,
      ).render

      val filterEditor = FilterEditor.Props(
        p.state.filter,
        project,
        onFilterChange,
      ).render

      def renderTable(mode: Table.Mode) = reqTable.Whole.Props(
        mode,
        activeColumnsPlus,
        pxRowSelectionVisible.value(),
        p.editor,
        p.rowAsync,
        project.config,
        pxProjectWidgets.value(),
        filterDead,
        modifyViewFn,
      ).render

      val body: VdomElement =
        mode match {
          case Mode.HasContent           => renderTable(Table.Mode.Normal(rows))
          case Mode.NoContentCosFilter   => renderTable(Table.Mode.FilteredOut)
          case Mode.NoContentCosHideDead => renderAllContentDead
          case Mode.EmptyProject         => renderEmptyProject
        }

      mainBase(
        <.div(*.viewRow,
          <.div(*.viewRowSV, savedViews),
          <.div(*.filterDeadButtonContainer, filterDeadButton)),
        <.div(*.actionCtrls,
          newStuff.buttonProps.render,
          pxSelectionCtrls.value().render,
          <.div(*.summary, pxPageSummary.value()).unless(mode ==* Mode.EmptyProject || mode ==* Mode.NoContentCosHideDead)
        ),
        newStuff.form.whenDefined,
        <.div(*.viewCtrls,
          pxSortCriteriaEditor.value(),
          <.div(*.flexGap),
          filterEditor,
          pxColumnSelector.value()
        ).unless(mode ==* Mode.EmptyProject || mode ==* Mode.NoContentCosHideDead),
        body)
    }

    // Prevent browser auto-focusing the first <input> it sees on page load
    def unfocus = Callback {
      document.activeElement.domToHtml.foreach(_.blur())
    }
  }

}
