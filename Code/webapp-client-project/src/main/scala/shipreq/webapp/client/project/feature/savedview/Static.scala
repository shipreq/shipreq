package shipreq.webapp.client.project.feature.savedview

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Px, StateSnapshot}
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.savedview._
import shipreq.webapp.base.data.{FilterDead, Project, ProjectConfig, ReqId, ShowDead}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.filter.{CompiledFilter, Filter}
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.lib.{ConfirmJs, PromptJs}
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.SavedViewCmd
import shipreq.webapp.client.project.app.Style.{savedViews => *}
import shipreq.webapp.client.project.widgets.{FilterDeadButton, FilterEditor}

final case class Static(stateAccess                   : StateAccessPure[(State, FilterDead)],
                        pxProject                     : Px[Project],
                        pxFilterDead                  : Px[FilterDead],
                        pxFilterCompilerFromFilterDead: Px[FilterDead => Filter.Valid.Compiler],
                        confirmJs                     : ConfirmJs,
                        promptJs                      : PromptJs,
                        savedViewAsyncW               : AsyncFeature.Write.D0[ErrorMsg],
                        savedViewIO                   : ServerSideProcInvoker[SavedViewCmd, ErrorMsg, VerifiedEvent.Seq]) {

  val pxProjectConfig: Px[ProjectConfig] =
    pxProject.map(_.config).withReuse

  val setFilterDeadFn: Reusable[SetStateFnPure[FilterDead]] =
    Reusable.byRef(SetStateFn((fdO, cb) =>
      fdO.fold(cb)(fd =>
        for {
          p       <- pxProject.toCallback
          (s1, _) <- stateAccess.state
          s2       = s1.setFilterDead(fd, p)
          _       <- stateAccess.setState((s2, fd), cb)
        } yield ())))

  val pxViewState: Px[ViewLogic.State] =
    Px.callback(stateAccess.state.map(_._1.view)).withReuse.autoRefresh

  val pxSavedViewId: Px[Option[SavedView.Id]] =
    pxViewState.map(_.referenceViewId).withReuse

  val pxSavedViews: Px[SavedViews.Optional] =
    pxProject.map(_.savedViews).withReuse

  val pxColumnPlusAll: Px[ColumnPlus.All] =
    (for {
      p  <- pxProject
      fd <- pxFilterDead
    } yield ColumnPlus.All(p, fd))
      .withReuse

  val pxActiveView: Px[View] =
    (for {
      vs <- pxViewState
      pc <- pxProjectConfig
      sv <- pxSavedViews
      fd <- pxFilterDead
      cs <- pxColumnPlusAll
    } yield vs.activeView(sv, fd).filterColumns(pc)(cs.containsColumn) // .filterColumns calls .makeCorrect
    ).withReuse

  val activeViewCB: Reusable[CallbackTo[View]] = {
    val f = pxActiveView.toCallback.toScalaFn
    Reusable.byRef(f).map(CallbackTo.lift)
  }

  val pxActiveColumns: Px[NonEmptyVector[Column]] =
    pxActiveView.map(_.columns).withReuse

  val pxActiveColumnsPlus: Px[NonEmptyVector[ColumnPlus]] =
    (for {
      p  <- pxProject
      fd <- pxFilterDead
      cs <- pxActiveColumns
    } yield ColumnPlus.forceNEV(ColumnPlus.byProject(p, fd))(cs)
    ).withReuse

  val pxActiveOrder: Px[SortCriteria] =
    pxActiveView.map(_.order).withReuse

  val pxActiveFilter: Px[Option[CompiledFilter]] =
    (for {
      view <- pxActiveView
      ff   <- pxFilterCompilerFromFilterDead
      fd   <- pxFilterDead
    } yield view.filter.map(f => ff(fd)(f))
    ).withReuse

  val pxReqWhitelistIgnoringFilterDead: Px[Option[Set[ReqId]]] =
    (for {
      f <- pxActiveFilter
      p <- pxProject
    } yield ImpGraphConfig.buildReqWhitelist(ShowDead, f, p)
    ).withReuse

  val pxReqWhitelist: Px[Option[Set[ReqId]]] =
    (for {
      fd <- pxFilterDead
      f  <- pxActiveFilter
      p  <- pxProject
    } yield ImpGraphConfig.buildReqWhitelist(fd, f, p)
    ).withReuse

  val modifyViewFn: Reusable[ModStateFnPure[View]] =
    Reusable.byRef(ModStateFn((mod, cb) =>
      for {
        p       <- pxProject.toCallback
        (s1, _) <- stateAccess.state
        fd      <- pxFilterDead.toCallback
        v1      = s1.view.activeView(p.savedViews, fd)
        v2      = mod(v1) getOrElse v1
        s2      = s1.setModifiedView(p, updateFilterText = false)(v2)
        _      <- stateAccess.setState((s2, v2.filterDead), cb)
      } yield ()))

  val pxSavedViewsMenu: Px[ViewLogic.Menu] =
    for {
      savedViews   <- pxSavedViews
      config       <- pxProjectConfig
      viewState    <- pxViewState
      validColumns <- pxColumnPlusAll
      activeView   <- pxActiveView
    } yield ViewLogic.Menu(savedViews, viewState, _.filterColumns(config)(validColumns.containsColumn), activeView, activeViewCB)

  private val runSavedViewAction: ViewLogic.Action ~=> Callback = {
    def mod(p: Project, a: ViewLogic.Action, state: (State, FilterDead)): (State, FilterDead) = {
      val (s1, fd1) = state
      val s2 = s1.runSavedViewAction(p, updateFilterText = true)(a)
      val fd2 = s2.filterDead(p, fd1)
      (s2, fd2)
    }
    Reusable.fn { action =>
      for {
        p <- pxProject.toCallback
        _ <- stateAccess.modState(mod(p, action, _))
      } yield ()
    }
  }

  private val onFilterChange: FilterEditor.UpdateFn =
    (newState, newFilter, cb) => {
      def mod(p: Project, state: (State, FilterDead)): (State, FilterDead) = {
        val (s1, fd) = state
        val s2 = s1.copy(filter = newState).modifyView(p, fd, updateFilterText = false)(_.withFilter(newFilter))
        (s2, fd)
      }
      for {
        p <- pxProject.toCallback
        _ <- stateAccess.modState(mod(p, _), cb)
      } yield ()
    }

  def renderSavedViewManager(savedViewAsync: AsyncFeature.Read.D0[ErrorMsg]): VdomElement =
    ViewManager.Props(
      menu        = pxSavedViewsMenu.value(),
      asyncRW     = AsyncFeature.ReadWrite.D0(savedViewAsyncW, savedViewAsync),
      confirmJs   = confirmJs,
      promptJs    = promptJs,
      runAction   = runSavedViewAction,
      savedViewIO = savedViewIO,
    ).render

  def renderFilterEditor(state: State): VdomElement =
    FilterEditor.Props(
      state   = state.filter,
      project = pxProject.value(),
      update  = onFilterChange,
    ).render

  def renderFilterDeadButton(filterDead: FilterDead): VdomElement =
    FilterDeadButton.Component(StateSnapshot.withReuse(filterDead)(setFilterDeadFn))

  private[this] val divViewRow                   = <.div(*.viewRow)
  private[this] val divViewRowSV                 = <.div(*.viewRowSV)
  private[this] val divFilterDeadButtonContainer = <.div(*.filterDeadButtonContainer)

  def renderSavedViewsAndFilterDeadButton(async: AsyncFeature.Read.D0[ErrorMsg], filterDead: FilterDead): VdomElement =
    renderSavedViewsAndFilterDeadButton(async, renderFilterDeadButton(filterDead))

  def renderSavedViewsAndFilterDeadButton(async: AsyncFeature.Read.D0[ErrorMsg], filterDeadButton: VdomElement): VdomElement =
    divViewRow(
      divViewRowSV(renderSavedViewManager(async)),
      divFilterDeadButtonContainer(filterDeadButton))
}
