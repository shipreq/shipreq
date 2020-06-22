package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import monocle.macros.Lenses
import org.scalajs.dom.document
import scalacss.ScalaCssReact._
import shipreq.base.util.{Allow, ErrorMsg}
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.UpdateContentCmd
import shipreq.webapp.base.text.TextSearch
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.base.ui.{BaseStyles, NoContentMessage, Toast}
import shipreq.webapp.client.project.app.Style.reqtable.{page => *}
import shipreq.webapp.client.project.feature.SavedViewFeature.ColumnPlus
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.widgets.{FilterDeadButton, ProjectWidgets}

object ReqTablePage {

  def apply(staticProps: StaticProps) =
    ScalaComponent.builder[Props]
      .backend(new Backend(staticProps, _))
      .renderBackend
      .componentDidMount(_.backend.unfocus)
      .build

  final case class StaticProps(stateAccess           : StateAccessPure[State],
                               savedViewStatic       : SavedViewFeature.Static,
                               pxTextSearch          : Px[TextSearch],
                               pxProjectWidgets      : Reusable[Px[ProjectWidgets.NoCtx]],
                               pxFilterCompilerFromFD: Px[FilterDead => Filter.Valid.Compiler],
                               reqDetailRC           : RouterCtl[ExternalPubid],
                               toast                 : Toast,
                               updateIO              : ServerSideProcInvoker[UpdateContentCmd, ErrorMsg, Any],
                               rowAsyncW             : AsyncFeature.Write.D1[Row.SourceId, ErrorMsg])

  final case class Props(create    : CreateFeature.ReadWrite.ForProject,
                         editor    : EditorFeature.ReadWrite.ForProject,
                         savedViews: SavedViewFeature,
                         rowAsync  : AsyncFeature.Read.D1[Row.SourceId, ErrorMsg],
                         filterDead: FilterDead,
                         state     : State)

  @Lenses
  final case class State(selection: RowSelection,
                         newStuff : NewStuff.State,
                         modal    : Modal.State)

  object State {
    def init: State =
      State(
        Selection.empty,
        NewStuff.State.init,
        Modal.none)
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
    import savedViewStatic.{stateAccess => _, _}

    val modNewStuff : ModFn[NewStuff.State] = Reusable.fn.state(stateAccess zoomStateL State.newStuff).modStateFn
    val setSelection: SetFn[RowSelection  ] = Reusable.fn.state(stateAccess zoomStateL State.selection).setStateFn
    val setModal    : SetFn[Modal.State   ] = Reusable.fn.state(stateAccess zoomStateL State.modal).setStateFn

    private val manualPxs = Px.ManualCollection()

    private def pxProps[A: Reusability](f: Props => A): Px.ThunkM[A] = {
      val px = Px.props($).map(f).withReuse.manualRefresh
      manualPxs.add(px)
      px
    }

    val pxSelection: Px[RowSelection] = pxProps(_.state.selection)

    val pxRows: Px[Vector[Row]] =
      for {
        p  <- pxProject
        v  <- pxActiveView
        pw <- pxProjectWidgets
        fc <- pxFilterCompilerFromFD
      } yield Logic.rowsForTable(p, v, pw.plainText, fc(v.filterDead))

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

    val pxColumnSelector: Px[VdomElement] =
      for {
        sel <- pxActiveColumns
        all <- pxColumnPlusAll
        pc  <- pxProjectConfig
      } yield
        ColumnSelector.Props(sel, all, modifyViewFn.map(m => u => m.modState(_.withColumns(u, pc)))).render

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
      manualPxs.refresh()
      p.state.modal renderOrElse renderMain(p)
    }

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
          p.savedViews.renderFilterDeadButton

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

      val filterEditor =
        p.savedViews.renderFilterEditor

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

      <.main(
        p.savedViews.renderSavedViewsAndFilterDeadButton(filterDeadButton),
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
