package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.base.util.{Allow, ErrorMsg}
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.base.ui.widgets.NoContentMessage
import shipreq.webapp.base.util.DomUtil
import shipreq.webapp.client.project.app.Style.reqtable.{page => *}
import shipreq.webapp.client.project.feature.SavedViewFeature.ColumnPlus
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.widgets.{FilterDeadButton, ProjectWidgets}
import shipreq.webapp.member.feature.PreviewFeature
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.savedview._
import shipreq.webapp.member.project.filter.Filter
import shipreq.webapp.member.project.protocol.websocket.UpdateContentCmd
import shipreq.webapp.member.project.text.{PlainText, TextSearch}
import shipreq.webapp.member.project.util.DataReusability._
import shipreq.webapp.member.ui.Toast

object ReqTablePage {

  def apply(staticProps: StaticProps) =
    ScalaComponent.builder[Props]
      .backend(new Backend(staticProps, _))
      .renderBackend
      .componentDidMountConst(DomUtil.unfocus) // Prevent browser auto-focusing the first <input> it sees on page load
      .build

  final case class StaticProps(stateAccess           : StateAccessPure[State],
                               savedViewStatic       : SavedViewFeature.Static,
                               pxPlainText           : Px[PlainText.ForProject.NoCtx],
                               pxTextSearch          : Px[TextSearch],
                               pxProjectWidgets      : Reusable[Px[ProjectWidgets.NoCtx]],
                               pxFilterCompilerFromFD: Px[FilterDead => Filter.Valid.Compiler],
                               reqDetailRC           : RouterCtl[ExternalPubid],
                               assetManifest         : AssetManifest,
                               toast                 : Toast,
                               updateIO              : ServerSideProcInvoker[UpdateContentCmd, ErrorMsg, Any],
                               rowAsyncW             : AsyncFeature.Write.D1[Row.SourceId, ErrorMsg])

  final case class Props(create         : CreateFeature.ReadWrite.ForProject,
                         createPreviewRW: PreviewFeature.ReadWrite.Composite[CreateFeature.PreviewId],
                         editor         : EditorFeature.ReadWrite.ForProject,
                         editorArgs     : EditorFeature.EditorArgs.ForAny,
                         savedViews     : SavedViewFeature,
                         rowAsync       : AsyncFeature.Read.D1[Row.SourceId, ErrorMsg],
                         filterDead     : FilterDead,
                         state          : State)

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

    val sortCriteriaEditor = new SortCriteriaEditor(assetManifest)

    val pxSortCriteriaEditor: Px[VdomElement] =
      for {
        o <- pxActiveOrder
        c <- pxColumnPlusAll
      } yield sortCriteriaEditor.Props(o, modifyViewFn.map(m => o => m.modState(View.order replace o)), c).render

    val pxSelectionCtrls: Px[SelectionCtrls.Props] =
      for {
        project        <- pxProject
        projectWidgets <- pxProjectWidgets
        textSearch     <- pxTextSearch
        rows           <- pxRows
        sel            <- pxRowSelectionVisible
      } yield SelectionCtrls.Props(
        sel, rows, setModal.map(_.setState), project, projectWidgets, textSearch, updateIO, rowAsyncW)

    val reqTable = new Table(pxProjectWidgets, pxPlainText)

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
      val plainText         = pxPlainText.value()
      val textSearch        = pxTextSearch.value()
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
        previewRW      = p.createPreviewRW,
        project        = project,
        plainText      = plainText,
        textSearch     = textSearch,
        projectWidgets = projectWidgets,
        state          = p.state.newStuff,
        modState       = modNewStuff,
        routerCtl      = reqDetailRC,
        toast          = toast,
        allowRCG       = Allow when activeView.viewCodeGroups,
        create         = p.create,
        activeColumns  = newFormColumns,
      )

      val filterEditor =
        p.savedViews.renderFilterEditor

      def renderTable(mode: Table.Mode) = reqTable.Whole.Props(
        mode,
        activeColumnsPlus,
        pxRowSelectionVisible.value(),
        p.editor,
        p.editorArgs,
        p.rowAsync,
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
        actionCtrls(
          newStuff.buttonProps.render,
          pxSelectionCtrls.value().render,
          <.div(*.summary, pxPageSummary.value()).unless(mode ==* Mode.EmptyProject || mode ==* Mode.NoContentCosHideDead)
        ),
        newStuffContainer(newStuff.form.whenDefined),
        viewCtrls(
          pxSortCriteriaEditor.value(),
          <.div(*.flexGap),
          filterEditor,
          pxColumnSelector.value()
        ).unless(mode ==* Mode.EmptyProject || mode ==* Mode.NoContentCosHideDead),
        bodyContainer(body),
      )
    }
  }

  private val actionCtrls       = <.div(^.key := "a", *.actionCtrls)
  private val viewCtrls         = <.div(^.key := "v", *.viewCtrls)
  private val bodyContainer     = <.div(^.key := "b")
  private val newStuffContainer = <.div(^.key := "n")
}
