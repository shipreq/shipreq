package shipreq.webapp.client.project.app.reqtable2

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.univeq._
import monocle.Lens
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.base.util.Allow
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.ValidFilter
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.client.base.feature.AsyncFeature
import shipreq.webapp.client.base.lib.DataReusability._
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.base.ui.BaseStyles
import shipreq.webapp.client.project.app.state.{Changes, ClientData}
import shipreq.webapp.client.project.app.Style.reqtable2.{page => *}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.protocol.ServerCall
import shipreq.webapp.client.project.widgets.ProjectWidgets

object ReqTablePage {

  def apply(staticProps: StaticProps) =
    ScalaComponent.builder[Props]("ReqTablePage")
      .backend(new Backend(staticProps, _))
      .renderBackend
      .componentWillMount(_.backend.syncState)
      .componentWillReceiveProps($ => $.backend.onPropsChange($.currentProps, $.nextProps))
      .build

  final case class StaticProps(stateAccess     : StateAccessPure[State],
                               cd              : ClientData,
//                               cp              : ClientProtocol,
//                               createContentFn : CreateContentFn.Instance,
//                               updateContentFn : UpdateContentFn.Instance,
                               pxPlainText     : Px[PlainText.ForProject],
                               pxTextSearch    : Px[TextSearch],
                               pxProjectWidgets: Px[ProjectWidgets],
                               reqDetailRC     : RouterCtl[ExternalPubid])

  final case class Props(editor    : EditorFeature.ReadWrite.ForProject,
                         rowAsync  : AsyncFeature.ReadWrite.D1[Row.SourceId, String],
                         filterDead: StateSnapshot[FilterDead],
                         state     : State)

  object Props {
    implicit val reusability: Reusability[Props] =
      Reusability.caseClass
  }

  @Lenses
  final case class State(tableSettings: TableSettings,
                         filter       : FilterEditor.State,
                         selection    : RowSelection,
                         newButton    : NewButton.State,
                         modal        : Modal.State)

  object State {
    implicit val reusability: Reusability[State] =
      Reusability.caseClass

    def init: State =
      State(
        TableSettings.default,
        FilterEditor.State.init,
        Selection.empty,
        NewButton.initState,
        Modal.none)

    val validFilter: Lens[State, Option[ValidFilter]] =
      tableSettings ^|-> TableSettings.filter

    val sortCriteria: Lens[State, SortCriteria] =
      tableSettings ^|-> TableSettings.order
  }

  final class Backend(sp: StaticProps, $: BackendScope[Props, Unit]) {
    import sp._
    import cd.pxProject

    val modSettings    : ModFn[TableSettings] = Reusable.fn.state(stateAccess zoomStateL State.tableSettings).mod
    val setSelection   : SetFn[RowSelection ] = Reusable.fn.state(stateAccess zoomStateL State.selection).set
    val setSortCriteria: SetFn[SortCriteria ] = Reusable.fn.state(stateAccess zoomStateL State.sortCriteria).set

    private var manualRefresh = List.empty[Px.ThunkM[_]]
    private def pxProps[A: Reusability](f: Props => A): Px.ThunkM[A] = {
      val px = Px.props($).map(f).withReuse.manualRefresh
      manualRefresh ::= px
      px
    }

    val pxFilterDead                                = pxProps(_.filterDead.value)
    val pxTableSettings: Px[TableSettings         ] = pxProps(_.state.tableSettings)
    val pxSelection    : Px[RowSelection          ] = pxProps(_.state.selection)
    val pxActiveColumns: Px[NonEmptyVector[Column]] = pxProps(_.state.tableSettings.columns)

    val pxRows: Px[Vector[Row]] =
      for {
        p  <- pxProject
        s  <- pxTableSettings
        fd <- pxFilterDead
        pt <- pxPlainText
        ts <- pxTextSearch
      } yield Logic.rowsForTable(p, s, fd, pt, ts)

    val pxRowIdsWithWholeRowAsync: Px[Set[Row.SourceId]] =
      pxProps(_.rowAsync.read.keySet)

    val pxRowSelectionVisible: Px[RowSelectionVisible] =
      for {
        rs <- pxRows
        wr <- pxRowIdsWithWholeRowAsync
        s  <- pxSelection
      } yield
        s.updateBy(setSelection).legal(rs.iterator.map(_.sourceId).toSet &~ wr)

    val pxActiveColumnsPlus: Px[NonEmptyVector[ColumnPlus]] =
      (for {
        p  <- pxProject
        cs <- pxActiveColumns
      } yield ColumnPlus.forceNEV(ColumnPlus.byProject(p))(cs))
        .withReuse

    val pxColumnPlusAll: Px[ColumnPlus.All] =
      (for {
        p  <- pxProject
        fd <- pxFilterDead
      } yield ColumnPlus.All(p, fd))
        .withReuse

    val pxColumnSelector: Px[VdomElement] =
      for {
        sel <- pxActiveColumns
        all <- pxColumnPlusAll
      } yield
        ColumnSelector.Props(sel, all, modSettings.map(m => u => m(_.setColumns(u)))).render

    val onFilterChange: FilterEditor.UpdateFn =
      (newState, newFilter) => stateAccess.modState { oldState =>
        var f = State.filter.set(newState)
        if (newFilter !=* oldState.tableSettings.filter)
          f = f compose State.validFilter.set(newFilter)
        f(oldState)
      }

    val pxTableContentStats: Px[TableContentStats] =
      for {
        p    <- pxProject
        rows <- pxRows
      } yield Logic.stats(p, rows)

    val pxPageSummary: Px[VdomElement] =
      for {
        stats <- pxTableContentStats
        sel   <- pxRowSelectionVisible
      } yield PageSummary.Props(stats, sel.legalSelection.size).render

    val pxSortCriteriaEditor: Px[VdomElement] =
      for {
        s <- pxTableSettings
        c <- pxColumnPlusAll
      } yield SortCriteriaEditor.Props(s.order, setSortCriteria, c).render

    val newButtonUpdate: Reusable[NewButton.Update] =
      Reusable.byRef(
        NewButton.Update(
          stateAccess.zoomStateL(State.newButton).setState(_),
          c => Callback.alert("Create: " + c)))

    def render(p: Props): VdomElement = {
      Px.refresh(manualRefresh: _*)

      val newButton = NewButton.Props(
        p.state.newButton,
        pxProject.value().config.reqTypes,
        Allow,
        Some(newButtonUpdate),
      ).render

      val filterEditor = FilterEditor.Props(
        p.state.filter,
        pxProject.value(),
        onFilterChange,
      ).render

      val table = Table.Whole.Props(
        pxRows.value(),
        pxActiveColumnsPlus.value(),
        pxRowSelectionVisible.value(),
        p.editor,
        p.rowAsync.read,
        pxProject.value().config,
        pxProjectWidgets.value(),
        modSettings,
      ).render

      <.main(BaseStyles.containerFull,
        ViewsMenu.Component(p.filterDead),
        newButton,
        pxPageSummary.value(),
        <.div(*.viewCtrls,
          pxSortCriteriaEditor.value(),
          <.div(*.flexGap),
          filterEditor,
          pxColumnSelector.value()),
        table)
    }

    def onPropsChange(prev: Props, next: Props): Callback =
      if (prev.filterDead.value ==* next.filterDead.value)
        Callback.empty
      else
        syncState

    /** Synchronises the State of this page with external Props that affect it. */
    val syncState: Callback =
      stateAccess.modState { s =>
        pxFilterDead.refresh()
        State.tableSettings.modify(_.filterColumns(pxColumnPlusAll.value().containsColumn))(s)
      }
  }

}
