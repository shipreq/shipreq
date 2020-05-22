package shipreq.webapp.client.project.feature.savedview

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.vdom.VdomElement
import shipreq.base.util.{ErrorMsg, Optics}
import shipreq.webapp.base.data.{FilterDead, Project}
import shipreq.webapp.base.data.savedview.{Column, SavedViews, SortCriteria, View}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.SavedViewCmd
import shipreq.webapp.client.project.widgets.FilterEditor

final case class Static(stateAccess    : StateAccessPure[(FilterDead, State)],
                        pxProject      : Px[Project],
                        pxFilterDead   : Px[FilterDead],
                        savedViewAsyncW: AsyncFeature.Write.D0[ErrorMsg],
                        savedViewIO    : ServerSideProcInvoker[SavedViewCmd, ErrorMsg, VerifiedEvent.Seq]) {

  val stateAccessS: StateAccessPure[State] =
    stateAccess.zoomStateL(Optics.lensTuple2_2)

  val setFilterDeadFn: Reusable[SetStateFnPure[FilterDead]] =
    Reusable.byRef(SetStateFn((fdO, cb) =>
      fdO.fold(cb)(fd =>
        for {
          p       <- pxProject.toCallback
          (_, s1) <- stateAccess.state
          s2      = s1.setFilterDead(fd, p)
          _       <- stateAccess.setState((fd, s2), cb)
        } yield ())))

  val pxViewState: Px[ViewLogic.State] =
    Px.callback(stateAccess.state.map(_._2.view)).withReuse.autoRefresh

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

  val pxActiveOrder: Px[SortCriteria] =
    pxActiveView.map(_.order).withReuse

  val modifyViewFn: Reusable[ModStateFnPure[View]] =
    Reusable.byRef(ModStateFn((mod, cb) =>
      for {
        p       <- pxProject.toCallback
        (_, s1) <- stateAccess.state
        fd      <- pxFilterDead.toCallback
        v1      = s1.view.activeView(p.savedViews, fd)
        v2      = mod(v1) getOrElse v1
        s2      = State.setModifiedView(p, updateFilterText = false)(v2)(s1)
        _       <- stateAccess.setState((v2.filterDead, s2), cb)
      } yield ()))

  val pxActiveColumnsPlus: Px[NonEmptyVector[ColumnPlus]] =
    (for {
      p  <- pxProject
      cs <- pxActiveColumns
    } yield ColumnPlus.forceNEV(ColumnPlus.byProject(p))(cs))
      .withReuse

  val pxSavedViewsMenu: Px[ViewLogic.Menu] =
    for {
      savedViews <- pxSavedViews
      viewState  <- pxViewState
      activeView <- pxActiveView
    } yield ViewLogic.Menu(savedViews, viewState, activeView, activeViewCB)

  private val onFilterChange: FilterEditor.UpdateFn =
    (newState, newFilter, cb) =>
      for {
        p  <- pxProject.toCallback
        fd <- pxFilterDead.toCallback
        m1 = State.filter.set(newState)
        m2 = State.modifyView(p, fd, updateFilterText = false)(_.withFilter(newFilter))
        _ <- stateAccessS.modState(m1 compose m2, cb)
      } yield ()

  private val runSavedViewAction: ViewLogic.Action ~=> Callback =
    Reusable.fn { action =>
      for {
        project <- pxProject.toCallback
        mod     = State.runSavedViewAction(project, updateFilterText = true)(action)
        _       <- stateAccessS.modState(mod)
      } yield ()
    }

  def renderSavedViewManager(savedViewAsync: AsyncFeature.Read.D0[ErrorMsg]): VdomElement =
    ViewManager.Props(
      menu        = pxSavedViewsMenu.value(),
      asyncRW     = AsyncFeature.ReadWrite.D0(savedViewAsyncW, savedViewAsync),
      runAction   = runSavedViewAction,
      savedViewIO = savedViewIO,
    ).render

  def renderFilterEditor(state: State): VdomElement =
    FilterEditor.Props(
      state   = state.filter,
      project = pxProject.value(),
      update  = onFilterChange,
    ).render
}
