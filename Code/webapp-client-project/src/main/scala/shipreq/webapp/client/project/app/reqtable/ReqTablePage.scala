package shipreq.webapp.client.project.app.reqtable

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.univeq._
import monocle.Lens
import monocle.macros.Lenses
import org.scalajs.dom.document
import scalacss.ScalaCssReact._
import shipreq.base.util.{Allow, ErrorMsg, Valid}
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.filter.Filter.Implicits._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.base.ui.semantic.{Icon, Message}
import shipreq.webapp.client.project.app.state.ClientData
import shipreq.webapp.client.project.app.Style.reqtable.{page => *}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.widgets.ProjectWidgets

object ReqTablePage {

  def apply(staticProps: StaticProps) =
    ScalaComponent.builder[Props]("ReqTablePage")
      .backend(new Backend(staticProps, _))
      .renderBackend
      .componentWillMount(_.backend.syncState)
      .componentDidMount(_.backend.unfocus)
      .componentWillReceiveProps($ => $.backend.onPropsChange($.currentProps, $.nextProps))
      .build

  final case class StaticProps(stateAccess     : StateAccessPure[State],
                               cd              : ClientData,
                               pxTextSearch    : Px[TextSearch],
                               pxProjectWidgets: Reusable[Px[ProjectWidgets.NoCtx]],
                               reqDetailRC     : RouterCtl[ExternalPubid],
                               updateIO        : ServerSideProcInvoker[UpdateContentCmd, ErrorMsg, Any],
                               rowAsyncW       : AsyncFeature.Write.D1[Row.SourceId, ErrorMsg])

  final case class Props(create    : CreateFeature.ReadWrite.ForProject,
                         editor    : EditorFeature.ReadWrite.ForProject,
                         rowAsync  : AsyncFeature.Read.D1[Row.SourceId, ErrorMsg],
                         filterDead: StateSnapshot[FilterDead],
                         state     : State)

  @Lenses
  final case class State(tableSettings: TableSettings,
                         filter       : FilterEditor.State,
                         selection    : RowSelection,
                         newStuff     : NewStuff.State,
                         modal        : Modal.State) {

    def setFilter(f: Filter.Valid, cfg: ProjectConfig): State =
      copy(
        filter = FilterEditor.State(Filter.Valid.toText(cfg, f), Valid),
        tableSettings = tableSettings.copy(filter = Some(f)))
  }

  object State {
    def init: State =
      State(
        TableSettings.default,
        FilterEditor.State.init,
        Selection.empty,
        NewStuff.State.init,
        Modal.none)

    val validFilter: Lens[State, Option[Filter.Valid]] =
      tableSettings ^|-> TableSettings.filter

    val sortCriteria: Lens[State, SortCriteria] =
      tableSettings ^|-> TableSettings.order
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
    import cd.pxProject

    val modSettings    : ModFn[TableSettings ] = Reusable.fn.state(stateAccess zoomStateL State.tableSettings).mod
    val setNewStuff    : SetFn[NewStuff.State] = Reusable.fn.state(stateAccess zoomStateL State.newStuff).set
    val setSelection   : SetFn[RowSelection  ] = Reusable.fn.state(stateAccess zoomStateL State.selection).set
    val setSortCriteria: SetFn[SortCriteria  ] = Reusable.fn.state(stateAccess zoomStateL State.sortCriteria).set
    val setModal       : SetFn[Modal.State   ] = Reusable.fn.state(stateAccess zoomStateL State.modal).set

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
        pw <- pxProjectWidgets
        ts <- pxTextSearch
      } yield Logic.rowsForTable(p, s, fd, pw.plainText, ts)

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
        s <- pxTableSettings
        c <- pxColumnPlusAll
      } yield SortCriteriaEditor.Props(s.order, setSortCriteria, c).render

    val pxSelectionCtrls: Px[SelectionCtrls.Props] =
      for {
        project        <- pxProject
        projectWidgets <- pxProjectWidgets
        textSearch     <- pxTextSearch
        rows           <- pxRows
        sel            <- pxRowSelectionVisible
      } yield SelectionCtrls.Props(
        sel, rows, setModal, project, projectWidgets, textSearch, updateIO, rowAsyncW)

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

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    def render(p: Props): VdomElement = {
      Px.refresh(manualRefresh: _*)
      p.state.modal renderOrElse renderMain(p)
    }

    private val mainBase = <.main(BaseStyles.containerFull)

    def renderEmptyProject: VdomTag =
      Message(
        Message.Style(Message.Type.Info),
        Icon.InfoCircle,
        "Welcome to your new project!",
        "Create new content using the button above.")

    def renderAllContentDead: VdomTag =
      Message(
        Message.Style(Message.Type.Warning),
        Icon.TrashOutline,
        "No live content.",
        TagMod(
          "Create new content (above) or enable display of dead content (via the ",
          Icon.TrashOutline.tag,
          "button in the top-right)."))

    def renderMain(p: Props): VdomElement = {
      val project           = pxProject.value()
      val activeColumnsPlus = pxActiveColumnsPlus.value()
      val rows              = pxRows.value()
      val filterDead        = pxFilterDead.value()
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

      val newStuff = new NewStuff(
        p.state.newStuff,
        setNewStuff,
        project.config.reqTypes,
        Allow when p.state.tableSettings.viewCodeGroups,
        defaultNewType,
        p.create,
        activeColumnsPlus)

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
        modSettings,
      ).render

      val body: VdomElement =
        mode match {
          case Mode.HasContent           => renderTable(Table.Mode.Normal(rows))
          case Mode.NoContentCosFilter   => renderTable(Table.Mode.FilteredOut)
          case Mode.NoContentCosHideDead => renderAllContentDead
          case Mode.EmptyProject         => renderEmptyProject
        }

      mainBase(
        ViewsMenu.Component(Option.unless(mode ==* Mode.EmptyProject)(p.filterDead)),
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

    // Prevent browser auto-focusing the first <input> it sees on page load
    def unfocus = Callback {
      document.activeElement.domToHtml.foreach(_.blur())
    }
  }

}
