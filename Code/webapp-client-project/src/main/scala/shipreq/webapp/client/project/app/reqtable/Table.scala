package shipreq.webapp.client.project.app.reqtable

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import scala.collection.immutable.SortedSet
import scalacss.ScalaCssReact._
import shipreq.base.util.{Applicable, ErrorMsg, NotApplicable}
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.feature.{AsyncFeature, TableNavigationFeature}
import shipreq.webapp.base.lib.DomUtil._
import shipreq.webapp.base.ui.{EditTheme, semantic}
import shipreq.webapp.base.ui.semantic.{Icon, Message}
import shipreq.webapp.client.project.app.Style.reqtable.{table => *}
import shipreq.webapp.client.project.feature.{EditorFeature, Selection}
import shipreq.webapp.client.project.widgets.{DragToReorder, ProjectWidgets, ViewReq}
import shipreq.webapp.client.project.lib.DataReusability._
import EditorFeature.FieldKey

final class Table(rootPxProjectWidgets: Reusable[Px[ProjectWidgets.NoCtx]]) {
  import Table._
  import Shared._

  object Whole {

    case class Props(mode            : Mode,
                     cols            : NonEmptyVector[ColumnPlus],
                     selection       : RowSelectionVisible,
                     editor          : EditorFeature.ReadWrite.ForProject,
                     rowAsync        : AsyncFeature.Read.D1[Row.SourceId, ErrorMsg],
                     config          : ProjectConfig,
                     pw              : ProjectWidgets.NoCtx,
                     modifyView      : ModFn[View]) {
      @inline def render = Component(this)
    }

    implicit val reusabilityProps: Reusability[Props] =
      Reusability.caseClass

    final class Backend($: BackendScope[Props, Unit]) {

      private val pxProjectWidgets = Px.props($).map(_.pw).withReuse.manualRefresh
      private val pxProjectConfig  = Px.props($).map(_.config).withReuse.manualRefresh

      private val pxPubidFmt: Px[ProjectWidgets.NoCtx#PubidFormat] =
        pxProjectWidgets.map(_.PubidFormat(Plain, *.pubidColumnValue(_), titleFn = _ => None))

      private val pxApplicability: Px[Applicability[Column, Row]] =
        pxProjectConfig.map(cfg => Row.applicability(cfg.applicability))

      def render(p: Props): VdomElement = {
        pxProjectWidgets.refresh()
        pxProjectConfig.refresh()

        val header =
          Header.Component(
            Header.Props(
              p.cols,
              p.selection,
              p.modifyView.map(f => cs => f(_ withColumns cs.map(_.column))),
              p.modifyView.map(f => c => f(_ orderByColumn c.column))))

        def renderMsg(msg: VdomTag): VdomTag =
          <.tr(<.td(*.noContent, ^.colSpan := p.cols.length + 1, msg))

        def renderRows(rows: Vector[Row]): VdomArray = {
          val applicability = pxApplicability.value()
          val reqViewInputs: ReqRow.ViewInput = (p.config.reqTypes, p.pw, pxPubidFmt.value())

          rows.toVdomArray { genericRow =>
            val rowAsync = p.rowAsync(genericRow.sourceId)
            val selection = p.selection(genericRow.sourceId)

            genericRow match {
              case row: Row.ForReq =>
                ReqRow.Props(
                  row,
                  reqViewInputs,
                  p.editor.forReq(row.req.id),
                  p.cols,
                  applicability,
                  rowAsync,
                  selection,
                ).render

              case row: Row.ForCodeGroup =>
                CodeGroupRow.Props(
                  row,
                  p.pw,
                  p.editor.forCodeGroup(row.reqCodeId),
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
            case m@Mode.FilteredOut => renderMsg(m.render)
          }

        semantic.Table.celledCompactUnstackable(
          *.table,
          header,
          <.tbody(body))
      }
    }

    val Component = ScalaComponent.builder[Props]("ReqTable")
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

    implicit val reusabilityProps = Reusability.caseClass[Props]

    final class Backend($: BackendScope[Props, Unit]) {

      private def setNewOrder(newOrder: Vector[ColumnPlus]): Callback =
        NonEmptyVector.maybe(newOrder, Callback.empty)(newCols =>
          $.props.flatMap(_ reorder newCols))

      private def selColKeyDown(e: ReactKeyboardEventFromHtml): Callback =
        focusKeyHandlers(e)

      private def dataColKeyDown(col: ColumnPlus)(e: ReactKeyboardEventFromHtml): Callback =
        focusKeyHandlers(e) | CallbackOption.asEventDefault(e, CallbackOption.keyCodeSwitch(e) {
          case KeyCode.Space => $.props.flatMap(_ clickSort col)
        })

      private def renderFn(p: Props, content: DragToReorder.Content[ColumnPlus]): VdomElement = {
        val selectionCell =
          <.th(
            *.selectionColumnHeader,
            ^.onKeyDown ==> selColKeyDown,
            p.selection.total.checkboxAndOnClick) // TODO *.selectionCheckbox

        val cols =
          content.items.toVdomArray { i =>
            val c = i.data
            val live = c.column match {
              case Column.DeletionReason => Live // Don't render this title with strike-through
              case _                     => c.live
            }
            <.th(
              *.columnHeader(live, i.status),
              i.mod,
              ^.tabIndex   := -1,
              ^.onKeyDown ==> dataColKeyDown(c),
              ^.onClick   --> p.clickSort(c),
              c.name)
          }

        <.thead(
          content.rootMod,
          <.tr(
            selectionCell,
            cols))
      }

      private val columnDND: DragToReorder[ColumnPlus] =
        new DragToReorder(setNewOrder, c => $.props.map(renderFn(_, c)))

      def render(p: Props): VdomElement =
        columnDND.Component(p.cols.whole)
    }

    val Component = ScalaComponent.builder[Props]("Header")
      .renderBackend[Backend]
      .configure(shouldComponentUpdate)
      .build
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private sealed abstract class RowTemplate[
        FK         <: FieldKey.Nullary,
        _RowData   <: Row              : Reusability,
        _ViewInput                     : Reusability,
      ](displayName: String) {

    protected val rowToColumnToEditorField: RowData => Column => Option[FK]

    protected def reusabilityRowEditor: Reusability[RowEditor]

    protected def viewMaker(row: RowData, vi: ViewInput): Column => Reusable[TagMod]

    // ↑ abstract
    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    // ↓ concrete

    final type RowData   = _RowData
    final type ViewInput = _ViewInput
    final type RowEditor = EditorFeature.ReadWrite.ForFields[FK]

    case class Props(row             : RowData,
                     viewInput       : ViewInput,
                     editor          : RowEditor,
                     cols            : NonEmptyVector[ColumnPlus],
                     applicability   : Applicability[Column, Row],
                     rowAsync        : AsyncFeature.Read.D0[ErrorMsg],
                     selection       : Selection.OneUI[Row.SourceId]) {
      @inline def render = Component.withKey(row.id.key)(this)
    }

    implicit final val reusabilityProps: Reusability[Props] = {
      implicit val a = reusabilityRowEditor
      Reusability.caseClass
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

      def selCellKeyDown(e: ReactKeyboardEventFromHtml): Callback =
        focusKeyHandlers(e)

      val mkViewWhenApplicable: Column => Reusable[TagMod] =
        viewMaker(row, p.viewInput)

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
          Cell.Component.withKey(Column key col)(cp)
        }

      def renderNormal = {
        val selCell =
          selBase(
            ^.onKeyDown ==> selCellKeyDown,
            sel.onClick,
            sel.checkbox(*.selectionCheckbox, ^.tabIndex := -1))

        val columnToEditorField = rowToColumnToEditorField(p.row)

        val colCells = mkColumnCells(col =>
          columnToEditorField(col) match {
            case Some(f) => p.editor(f, rootPxProjectWidgets)
            case None    => EditorFeature.ReadWrite.ForEditor.doNothing
          })

        rowBase(selCell, colCells)
      }

      def renderLocked = {
        val colCells = mkColumnCells(_ => EditorFeature.ReadWrite.ForEditor.doNothing)
        rowBase(selBase(EditTheme.spinner), colCells)
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
      (ReqTypes, ProjectWidgets.NoCtx, ProjectWidgets.NoCtx#PubidFormat),
    ]("ReqRow") {

    override protected val rowToColumnToEditorField =
      _.req.id match {
        case _: GenericReqId => Column.editorFieldGR.getOption
        case _: UseCaseId    => Column.editorFieldUC.getOption
      }

    override protected def reusabilityRowEditor = implicitly

    override protected def viewMaker(row: RowData, vi: ViewInput): Column => Reusable[TagMod] = {
      val (reqTypes, pw, pubidFmt) = vi

      val viewReq = ViewReq.Data(
        row.req,
        row.exp.reqCodes,
        row.mv.tags,
        row.exp.cfTags.getOrElse(_, Vector.empty),
        row.exp.implications.apply,
        row.exp.cfImps.getOrElse(_, Vector.empty),
        SortedSet.empty[ExternalPubid]) // ReqTable doesn't display pastPubids
        .apply(pw)

      def renderCodes: VdomElement =
        if (row.exp.reqCodeTree.nonEmpty)
          pw.reqCodeTree(row.exp.reqCodeTree)
        else
          viewReq.codes

      val view: Column => TagMod = {
        case Column.CustomField(id)   => viewReq.customField(id)
        case Column.Title             => viewReq.title
        case Column.ReqType           => viewReq.reqType
        case Column.Tags              => viewReq.tags
        case Column.Implications(dir) => viewReq.imps(dir)
        case Column.Code              => renderCodes
        case Column.Pubid             => pubidFmt(row.req)
        case Column.DeletionReason    => viewReq.deletionReason getOrElse `n/a`
      }
      c => reusabilityView.reusable((row, vi, c)).map(_ => view(c))
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object CodeGroupRow extends RowTemplate[
      FieldKey.ForCodeGroup,
      Row.ForCodeGroup,
      ProjectWidgets.NoCtx,
    ]("CodeGroupRow") {

    override protected val rowToColumnToEditorField =
      _ => Column.editorFieldCG.getOption

    override protected def reusabilityRowEditor = implicitly

    override protected def viewMaker(row: RowData, vi: ViewInput): Column => Reusable[TagMod] = {
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
           | Column.Tags
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
      implicit val reusability: Reusability[Props] = {
        implicit val cs: Reusability[CellState] = Reusability.byRef
        Reusability.caseClass
      }

      val `n/a`: On => Props =
        On.memo(on =>
          Props(
            CellState(on)(Dead),
            EditorFeature.ReadWrite.ForEditor.doNothing,
            reusableNA))
    }

    type RenderScope = ScalaComponent.Lifecycle.RenderScope[Props, Unit, Unit]
    type Mounted = ScalaComponent.MountedPure[Props, Unit, Unit]
    type Dom = dom.html.TableDataCell

    def domCB($: Mounted): CallbackTo[Dom] =
      $.getDOMNode.map(_.domCast[Dom])

    def focus($: Mounted): Callback =
      for {
        focused <- activeHtmlElement
        cell <- domCB($)
      } yield
        // If this cell's child is focused, or there is no focus at all, then focus this cell.
        // Otherwise, don't steal another element's focus
        if (focused.forall(cell.contains))
          cell.focus()

    /**
     * When a Button in the cell is clicked, we still get the event here in which case, the focus is set after the
     * button callback runs, meaning that (because separate modState()s don't compose) we trample the state change made by
     * the button, and replace it with a focus update.
     *
     * Rather than force all cell children to stop propagation of events, we apply so logic here to filter the events to
     * which we react.
     */
    def doesEventTargetCell(e: ReactEventFromHtml): Boolean =
      e.target == e.currentTarget ||
        (try e.target.tabIndex < 0 catch { case _: Throwable => false }) // .tabIndex is undefined from tests

    def onKeyDown(editor: EditorFeature.ReadWrite.ForAnyEditor): ReactKeyboardEventFromHtml => Callback = e => {
      def focusChild: CallbackOption[Unit] =
        CallbackOption
          .liftOption(focusableChildren(e.currentTarget.domAsHtml).nextOption())
          .map(_.focus())

      def focusOrStartEditor: CallbackOption[Unit] =
        if (editor.read.editor.isDefined) focusChild else editor.startEdit.getOrEmpty

      def cellEvents: CallbackOption[Unit] =
        CallbackOption.asEventDefault(e,
          CallbackOption.require(doesEventTargetCell(e)) >>
            CallbackOption.keyCodeSwitch(e) {
              case KeyCode.F2 => focusOrStartEditor
            })

      focusKeyHandlers(e) | cellEvents
    }

    val cellBase = <.td(^.tabIndex := -1)

    def render($: RenderScope, p: Props): VdomElement = {
      val editor = p.editor.onClose(focus($.mountedPure))
      cellBase(
        *.dataCell(p.cellState),
        ^.onKeyDown ==> onKeyDown(editor),
        editor.themedRenderOr(())(p.view))
    }

    val Component = ScalaComponent.builder[Props]("Cell")
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

    case object FilteredOut extends Mode {
      def render: VdomTag =
        Message(
          Message.Style(Message.Type.Info),
          Icon.Filter,
          "No filter results.",
          "None of the project content matches the specified filter criteria.")
    }

    private val reusabilityNormal: Reusability[Normal] =
      Reusability.caseClass

    implicit val reusability: Reusability[Mode] =
      Reusability((a, b) => // TODO Replace with Reusability.derive
        a match {
          case x: Normal => b match {
            case y: Normal => reusabilityNormal.test(x, y)
            case _ => false
          }
          case FilteredOut => a == b
        }
      )
  }

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Shared {

    type CellState = (Live, On)

    val CellState: On => Live => CellState =
      On.memo(on => Live.memo((_, on)))

    implicit val reusabilityApplicability: Reusability[Applicability[Column, Row]] =
      Reusability.byRef

    val `n/a`: VdomTag =
      <.span(*.`N/A`, "–")

    val reusableNA: Reusable[TagMod] =
      Reusable.byRef(`n/a`)

    @inline def focusKeyHandlers =
      TableNavigationFeature.Keys.handler
  }
}

