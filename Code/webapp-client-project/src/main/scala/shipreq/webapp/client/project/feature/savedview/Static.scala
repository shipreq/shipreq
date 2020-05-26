package shipreq.webapp.client.project.feature.savedview

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Px, StateSnapshot}
import japgolly.scalajs.react.vdom.VdomElement
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.savedview.{Column, SavedViews, SortCriteria, View}
import shipreq.webapp.base.data.{FilterDead, Project}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.lib.{ConfirmJs, PromptJs}
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.SavedViewCmd
import shipreq.webapp.client.project.widgets.{FilterDeadButton, FilterEditor}

final case class Static(stateAccess    : StateAccessPure[(State, FilterDead)],
                        pxProject      : Px[Project],
                        pxFilterDead   : Px[FilterDead],
                        confirmJs      : ConfirmJs,
                        promptJs       : PromptJs,
                        savedViewAsyncW: AsyncFeature.Write.D0[ErrorMsg],
                        savedViewIO    : ServerSideProcInvoker[SavedViewCmd, ErrorMsg, VerifiedEvent.Seq]) {

  val setFilterDeadFn: Reusable[SetStateFnPure[FilterDead]] =
    Reusable.byRef(SetStateFn((fdO, cb) =>
      fdO.fold(cb)(fd =>
        for {
          p       <- pxProject.toCallback
          (s1, _) <- stateAccess.state
          s2      = s1.setFilterDead(fd, p)
          _       <- stateAccess.setState((s2, fd), cb)
        } yield ())))

  val pxViewState: Px[ViewLogic.State] =
    Px.callback(stateAccess.state.map(_._1.view)).withReuse.autoRefresh

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

  val pxActiveColumnsPlus: Px[NonEmptyVector[ColumnPlus]] =
    (for {
      p  <- pxProject
      fd <- pxFilterDead
      cs <- pxActiveColumns
    } yield ColumnPlus.forceNEV(ColumnPlus.byProject(p, fd))(cs)
    ).withReuse

  val pxActiveOrder: Px[SortCriteria] =
    pxActiveView.map(_.order).withReuse

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
      viewState    <- pxViewState
      validColumns <- pxColumnPlusAll
      activeView   <- pxActiveView
    } yield ViewLogic.Menu(savedViews, viewState, _.filterColumns(validColumns.containsColumn), activeView, activeViewCB)

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
}
