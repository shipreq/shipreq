package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import scala.collection.immutable.SortedSet
import scalacss.ScalaCssReact._
import shipreq.base.util.{Applicable, ErrorMsg, NotApplicable}
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.data.savedview._
import shipreq.webapp.base.feature.clipboard.ClipboardData
import shipreq.webapp.base.feature.{AsyncFeature, DragToReorderFeature, EditControlsFeature, TableNavigationFeature}
import shipreq.webapp.base.lib.DomUtil._
import shipreq.webapp.base.text.{PlainText, ProjectText}
import shipreq.webapp.base.ui.semantic
import shipreq.webapp.base.util._
import shipreq.webapp.client.project.app.Style.reqtable.{table => *}
import shipreq.webapp.client.project.feature.EditorFeature.FieldKey
import shipreq.webapp.client.project.feature.SavedViewFeature.{ColumnLogic, ColumnPlus}
import shipreq.webapp.client.project.feature.{EditorFeature, Selection}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.{NoFilterResults, ProjectWidgets, ViewReq}

final class Table(rootPxProjectWidgets: Reusable[Px[ProjectWidgets.NoCtx]],
                  pxPlainText         : Px[PlainText.ForProject.NoCtx]) {
  import Table._

  private val tableNavigationFeature = TableNavigationFeature.NoRowSpans

  object Whole {

    case class Props(mode      : Mode,
                     cols      : NonEmptyVector[ColumnPlus],
                     selection : RowSelectionVisible,
                     editor    : EditorFeature.ReadWrite.ForProject,
                     editorArgs: EditorFeature.EditorArgs.ForAny,
                     rowAsync  : AsyncFeature.Read.D1[Row.SourceId, ErrorMsg],
                     filterDead: FilterDead,
                     modifyView: ModFn[View]) {

      // This is a no-op because it's what's already provided by LoadedRoot
      val pw = editorArgs.projectWidgets.withCtx(ProjectText.Context.None)

      @inline def config = editorArgs.project.config
      @inline def render = Component(this)
    }

    implicit val reusabilityProps: Reusability[Props] =
      Reusability.derive

    final class Backend($: BackendScope[Props, Unit]) {

      private val pxProjectWidgets = Px.props($).map(_.pw).withReuse.manualRefresh
      private val pxProjectConfig  = Px.props($).map(_.config).withReuse.manualRefresh

      private val pxPubidFmt: Px[ProjectWidgets.NoCtx#PubidFormat] =
        pxProjectWidgets.map(_.PubidFormat(Plain, *.pubidColumnValue(_), titleFn = _ => None))

      private val pxProjectApplicability: Px[ProjectApplicability[Column, Row]] =
        pxProjectConfig.map(cfg => Row.applicability(cfg.applicability))

      def render(p: Props): VdomElement = {
        pxProjectWidgets.refresh()
        pxProjectConfig.refresh()

        val pc = pxProjectConfig.value()

        val header =
          Header.Component(
            Header.Props(
              p.cols,
              p.selection,
              p.modifyView.map(f => cs => f.modState(_.withColumns(cs.map(_.column), pc))),
              p.modifyView.map(f => c => f.modState(_.orderByColumn(c.column)))))

        def renderRows(rows: Vector[Row]): VdomArray = {
          val applicability = pxProjectApplicability.value()
          val reqViewInputs: ReqRow.ViewInput = (p.config, p.pw, pxPubidFmt.value())

          rows.toVdomArray { genericRow =>
            val rowAsync = p.rowAsync(genericRow.sourceId)
            val selection = p.selection(genericRow.sourceId)

            genericRow match {
              case row: Row.ForReq =>
                ReqRow.Props(
                  row,
                  p.filterDead,
                  reqViewInputs,
                  p.editor.forReq(row.req.id),
                  p.editorArgs,
                  p.cols,
                  applicability,
                  rowAsync,
                  selection,
                ).render

              case row: Row.ForCodeGroup =>
                CodeGroupRow.Props(
                  row,
                  p.filterDead,
                  p.pw,
                  p.editor.forCodeGroup(row.reqCodeId),
                  p.editorArgs,
                  p.cols,
                  applicability,
                  rowAsync,
                  selection,
                ).render
            }
          }
        }

        val body: TagMod =
          p.mode match {
            case Mode.Normal(rows) => renderRows(rows)
            case Mode.FilteredOut  => NoFilterResults.asTableRow(p.cols.length + 1)
          }

        semantic.Table.celledCompactUnstackable(
          *.table,
          header,
          <.tbody(body))
      }
    }

    val Component = ScalaComponent.builder[Props]
      .renderBackend[Backend]
      .configure(shouldComponentUpdate)
      .build
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private object Header {

    case class Props(cols     : NonEmptyVector[ColumnPlus],
                     selection: RowSelectionVisible,
                     reorder  : NonEmptyVector[ColumnPlus] ~=> Callback,
                     clickSort: ColumnPlus ~=> Callback)

    implicit val reusabilityProps = Reusability.derive[Props]

    final class Backend($: BackendScope[Props, Unit]) {

      private def setNewColumns(newOrder: Vector[ColumnPlus]): Callback =
        NonEmptyVector.maybe(newOrder, Callback.empty)(newCols =>
          $.props.flatMap(_ reorder newCols))

      private def dataColKeyDown(col: ColumnPlus)(e: ReactKeyboardEventFromHtml): Callback =
        tableNavigationFeature.Keys(e) | CallbackOption.keyCodeSwitch(e) {
          case KeyCode.Space => $.props.flatMap(_ clickSort col)
        }.asEventDefault(e)

      private val columnDND =
        DragToReorderFeature[ColumnPlus](
          getData             = $.props.map(_.cols.whole),
          updateData          = u => setNewColumns(u.newOrder),
          updateUI            = $.forceUpdate,
          dragOutsideToRemove = true,
        )

      def render(p: Props): VdomElement = {
        val items = columnDND.items()

        val selectionCell =
          <.th(
            *.selectionColumnHeader,
            tableNavigationFeature.onKeyDown,
            p.selection.total.checkboxAndOnClick) // TODO *.selectionCheckbox

        val cols =
          items.toVdomArray { i =>
            val c = i.data
            val live = c.column match {
              case Column.DeletionReason => Live // Don't render this title with strike-through
              case _                     => c.live
            }
            <.th(
              *.columnHeader((live, i.status)),
              i.mod,
              ^.tabIndex   := -1,
              ^.onKeyDown ==> dataColKeyDown(c),
              ^.onClick   --> p.clickSort(c),
              c.name)
          }

        <.thead(
          columnDND.container,
          <.tr(
            selectionCell,
            cols))
      }
    }

    val Component = ScalaComponent.builder[Props]
      .renderBackend[Backend]
      .configure(shouldComponentUpdate)
      .build
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private sealed abstract class RowTemplate[
        FK         <: FieldKey,
        _RowData   <: Row      : Reusability,
        _ViewInput             : Reusability,
      ](displayName: String) {

    protected val rowToColumnToEditorField: RowData => Column => Option[FK]

    protected def reusabilityRowEditor: Reusability[RowEditor]

    protected def viewMaker(row: RowData, fd: FilterDead, vi: ViewInput): Column => Reusable[TagMod]

    protected def pubidClipboardData(row: RowData, vi: ViewInput): Option[() => ClipboardData]

    // ↑ abstract
    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    // ↓ concrete

    final type RowData   = _RowData
    final type ViewInput = _ViewInput
    final type RowEditor = EditorFeature.ReadWrite.ForFields[FK]

    case class Props(row             : RowData,
                     filterDead      : FilterDead,
                     viewInput       : ViewInput,
                     editor          : RowEditor,
                     editorArgs      : EditorFeature.EditorArgs.ForAny,
                     cols            : NonEmptyVector[ColumnPlus],
                     applicability   : ProjectApplicability[Column, Row],
                     rowAsync        : AsyncFeature.Read.D0[ErrorMsg],
                     selection       : Selection.OneUI[Row.SourceId]) {
      @inline def render = Component.withKey(row.id.key)(this)
    }

    implicit final val reusabilityProps: Reusability[Props] = {
      @nowarn("cat=unused")
      implicit val a = reusabilityRowEditor
      Reusability.derive
    }

    protected final val reusabilityView: Reusability[(RowData, ViewInput, Column)] =
      implicitly

    private val rowBase = <.tr

    final def render(p: Props): VdomElement = {
      val row         = p.row
      val sel         = p.selection
      val rowSelected = sel.get
      val cellStateFn = CellState(rowSelected)
      val selBase     = <.td(*.selectionDataCell(cellStateFn(row.live)))

      val mkViewWhenApplicable: Column => Reusable[TagMod] =
        viewMaker(row, p.filterDead, p.viewInput)

      def mkProps(c: Column, ok: Reusable[TagMod] => Cell.Props): Cell.Props =
        p.applicability(row, c) match {
          case Applicable    => ok(mkViewWhenApplicable(c))
          case NotApplicable => Cell.Props.`n/a`(rowSelected)
        }

      def mkColumnCells(columnEditor: Column => EditorFeature.ReadWrite.ForEditor[Unit, Any]): VdomArray =
        p.cols.whole.toVdomArray { colPlus =>
          val col    = colPlus.column
          def editor = columnEditor(col)
          val cs     = cellStateFn(row.live & colPlus.live)
          val cp     = mkProps(col, Cell.Props(cs, editor, _))
          Cell.Component.withKey(ColumnLogic key col)(cp)
        }

      def nopEditorFor(col: Column): EditorFeature.ReadWrite.ForEditor[Any, Nothing] = {
        import EditorFeature.ReadWrite.ForEditor.{doNothing => empty}
        col match {
          case Column.Pubid => empty.withClipboardDataOption(pubidClipboardData(p.row, p.viewInput))
          case _            => empty
        }
      }

      def renderNormal = {
        val selCell =
          selBase(
            tableNavigationFeature.onKeyDown,
            sel.onClick,
            sel.checkbox(*.selectionCheckbox, ^.tabIndex := -1))

        val columnToEditorField = rowToColumnToEditorField(p.row)

        val colCells = mkColumnCells(col =>
          columnToEditorField(col) match {
            case Some(f) => p.editor(f, rootPxProjectWidgets, p.filterDead).withArgs(p.editorArgs(f: f.type, style = editorStyle))
            case None    => nopEditorFor(col)
          })

        rowBase(selCell, colCells)
      }

      def renderLocked = {
        val colCells = mkColumnCells(nopEditorFor)
        rowBase(selBase(EditControlsFeature.spinner), colCells)
      }

      p.rowAsync match {
        case None                                          => renderNormal
        case Some(AsyncFeature.Status.InProgress)          => renderLocked
        case Some(s: AsyncFeature.Status.Failed[ErrorMsg]) =>
          // Currently, whole-row state is only used when a row is being deleted/restored.
          // To save dev-time, if the RPC fails an alert popups asking to retry/cancel, thus this part of the code
          // should only execute when the row is locked. Whole-row editing + failure won't occur.
          dom.console.warn(s.failure.value)
          rowBase(
            <.td(^.colSpan := (p.cols.length + 1),
              <.div(
                s.failure.value,
                <.button("Retry", ^.onClick --> s.retry),
                <.button("Abort", ^.onClick --> s.cancel))))
      }
    }

    final val Component = ScalaComponent.builder[Props](displayName)
      .render_P(render)
      .configure(shouldComponentUpdate)
      .build
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object ReqRow extends RowTemplate[
      FieldKey.ForSomeReq,
      Row.ForReq,
      (ProjectConfig, ProjectWidgets.NoCtx, ProjectWidgets.NoCtx#PubidFormat),
    ]("ReqRow") {

    override protected val rowToColumnToEditorField =
      _.req.id match {
        case _: GenericReqId => ColumnLogic.editorFieldGR.getOption
        case _: UseCaseId    => ColumnLogic.editorFieldUC.getOption
      }

    override protected def reusabilityRowEditor = implicitly

    override protected def viewMaker(row: RowData, fd: FilterDead, vi: ViewInput): Column => Reusable[TagMod] = {
      val (cfg, pw, pubidFmt) = vi

      val viewReq = ViewReq.Data(
        req              = row.req,
        filterDead       = fd,
        live             = row.live,
        codes            = row.exp.reqCodes.all,
        focusedTags      = row.exp.focusedTags,
        unfocusedTags    = row.exp.unfocusedTags,
        generalImps      = row.exp.implications(_).all,
        customImps       = row.exp.cfImps.get(_).fold(Vector.empty[Pubid])(_.all),
        pastPubids       = SortedSet.empty[ExternalPubid], // ReqTable doesn't display pastPubids
        impsAreMandatory = cfg.reqTypes.idsRequiringImplication.contains(row.req.reqTypeId),
        fieldRules       = row.fieldRules
      ).apply(pw)

      def renderCodes: VdomElement =
        if (row.exp.reqCodeTree.values.nonEmpty)
          pw.reqCodeTree(row.exp.reqCodeTree.values)
        else
          viewReq.codes

      val view: Column => TagMod = {
        case Column.CustomField(id)   => viewReq.customField(id) getOrElse `n/a`
        case Column.Title             => viewReq.title
        case Column.ReqType           => viewReq.reqType
        case Column.OtherTags         => viewReq.otherTags
        case Column.AllTags           => viewReq.allTags
        case Column.Implications(dir) => viewReq.imps(dir)
        case Column.Code              => renderCodes
        case Column.Pubid             => pubidFmt(row.req)
        case Column.DeletionReason    => viewReq.deletionReason getOrElse `n/a`
      }
      c => reusabilityView.reusable((row, vi, c)).map(_ => view(c))
    }

    override protected def pubidClipboardData(row: RowData, vi: ViewInput): Option[() => ClipboardData] =
      Some(() => {
        val pt       = pxPlainText.value()
        val pubid    = PlainText.pubid(row.req.pubid, vi._1.reqTypes)
        val title    = pt.reqTitleWithoutMarkup(row.req)
        ClipboardData(s"[$pubid] $title")
      })
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object CodeGroupRow extends RowTemplate[
      FieldKey.ForCodeGroup,
      Row.ForCodeGroup,
      ProjectWidgets.NoCtx,
    ]("CodeGroupRow") {

    override protected val rowToColumnToEditorField =
      _ => ColumnLogic.editorFieldCG.getOption

    override protected def reusabilityRowEditor = implicitly

    override protected def pubidClipboardData(row: RowData, vi: ViewInput) = None

    override protected def viewMaker(row: RowData, fd: FilterDead, vi: ViewInput): Column => Reusable[TagMod] = {
      val pw = vi

      def ret(c: Column, view: => TagMod): Reusable[TagMod] =
        reusabilityView.reusable((row, vi, c)).map(_ => view)

      def renderCodes: TagMod =
        row match {
          case Row.ForCodeGroup(_, _, Some(t)) => pw.reqCodeTreeItem(t)
          case Row.ForCodeGroup(_, c, None)    => pw.reqCode(c)
        }

      {
        case _: Column.CustomField
           | _: Column.Implications
           | Column.ReqType
           | Column.OtherTags
           | Column.AllTags
           | Column.Pubid
           | Column.DeletionReason  => reusableNA
        case c@ Column.Title        => ret(c, pw.codeGroupTitle(row.group))
        case c@ Column.Code         => ret(c, renderCodes)
      }
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private object Cell {

    case class Props(cellState: CellState,
                     editor   : EditorFeature.ReadWrite.ForEditor[Unit, Any],
                     view     : Reusable[TagMod])

    object Props {
      implicit val reusability: Reusability[Props] =
        Reusability.derive

      val `n/a`: On => Props =
        On.memo(on =>
          Props(
            CellState(on)(Dead),
            EditorFeature.ReadWrite.ForEditor.doNothing,
            reusableNA))
    }

    type RenderScope = ScalaComponent.Lifecycle.RenderScope[Props, Unit, Unit]
    type Mounted = ScalaComponent.MountedPure[Props, Unit, Unit]
    type Dom = dom.html.TableCell

    def onKeyDown(editor: EditorFeature.ReadWrite.ForEditor[Unit, Any]): ReactKeyboardEventFromHtml => Callback =
      e => tableNavigationFeature.Keys(e) | EditorFeature.Keys(editor)(e)

    val cellBase = <.td(^.tabIndex := -1)

    def render($: RenderScope, p: Props): VdomElement = {
      val editor = p.editor.onClose($.mountedPure.getDOMNode.map(_.toHtml).asCBO.flatMapCB(focusParentOnChildClose))
      cellBase(
        *.dataCell(p.cellState),
        ^.onKeyDown ==> onKeyDown(editor),
        editor.themedRenderOr(())(p.view))
    }

    val Component = ScalaComponent.builder[Props]
      .renderP(render)
      .configure(shouldComponentUpdate)
      .build
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object Table {

  sealed abstract class Mode
  object Mode {

    final case class Normal(rows: Vector[Row]) extends Mode

    case object FilteredOut extends Mode

    implicit val reusability: Reusability[Mode] =
      Reusability.derive
  }

  type CellState = (Live, On)

  val CellState: On => Live => CellState =
    On.memo(on => Live.memo((_, on)))

  implicit val reusabilityProjectApplicability: Reusability[ProjectApplicability[Column, Row]] =
    Reusability.byRef

  val `n/a`: VdomTag =
    <.span(*.`N/A`, "–")

  val reusableNA: Reusable[TagMod] =
    Reusable.byRef(`n/a`)

  val editorStyle =
    EditControlsFeature.Style.default.copy(openPreview = EditControlsFeature.OpenPreview.MinimallyWithControls)
}

