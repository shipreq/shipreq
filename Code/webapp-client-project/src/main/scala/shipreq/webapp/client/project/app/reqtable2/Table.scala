package shipreq.webapp.client.project.app.reqtable2

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data._
import shipreq.webapp.client.base.data.Plain
import shipreq.webapp.client.base.feature.AsyncFeature
import shipreq.webapp.client.base.lib.DomUtil._
import shipreq.webapp.client.base.ui.{EditTheme, semantic}
import shipreq.webapp.client.project.app.Style.reqtable2.{table => *}
import shipreq.webapp.client.project.feature.{EditorFeature, Selection}
import shipreq.webapp.client.project.widgets.{DragToReorder, ProjectWidgets, ViewReq}
import shipreq.webapp.client.project.lib.DataReusability._

object Table {

  // TODO Applicability - apply before editor/view

  object Whole {

    final case class Props(rows       : Vector[Row],
                           cols       : NonEmptyVector[ColumnPlus],
                           selection  : RowSelectionVisible,
                           editor     : EditorFeature.ReadWrite.ForProject,
                           rowAsync   : AsyncFeature.Read.D1[Row.SourceId, String],
                           reqTypes   : ReqTypes,
                           pw         : ProjectWidgets,
                           modSettings: ModFn[TableSettings]) {
      @inline def render = Component(this)
    }

    implicit val reusabilityProps: Reusability[Props] =
      Reusability.caseClass

    final class Backend($: BackendScope[Props, Unit]) {

      val pxProjectWidgets = Px.props($).map(_.pw).withReuse.manualRefresh
      val pxPubidFmt = pxProjectWidgets.map(_.PubidFormat(Plain, *.pubidColumnValue(_), titleFn = _ => None))

      def render(p: Props): VdomElement = {
        pxProjectWidgets.refresh()

        val header =
          Header.Component(
            Header.Props(
              p.cols,
              p.selection,
              p.modSettings.map(f => cs => f(_ setColumns cs.map(_.column))),
              p.modSettings.map(f => c => f(TableSettings.order.modify(_ want c.column)))))

        val reqViewInputs: ReqRow.ViewInput =
          (p.reqTypes, p.pw, pxPubidFmt.value())

        val renderRows: VdomArray =
          p.rows.toVdomArray { genericRow =>
            val rowAsync = p.rowAsync(genericRow.sourceId)
            val selection = p.selection(genericRow.sourceId)

            genericRow match {
              case row: Row.ForReq =>
                ReqRow.Props(
                  row,
                  reqViewInputs,
                  p.editor.forReq(row.req.id),
                  p.cols,
                  rowAsync,
                  selection,
                ).render

              case row: Row.ForReqCodeGroup =>
                ReqCodeGroupRow.Props(
                  row,
                  p.pw,
                  p.editor.forReqCodeGroup(row.reqCodeId),
                  p.cols,
                  rowAsync,
                  selection,
                ).render
            }
          }

        semantic.Table.celledCompactUnstackable(
          header,
          <.tbody(renderRows))
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
        focusKeyHandlers(e) | keyCodeSwitch(e) {
          case KeyCode.Space => $.props.flatMap(_ clickSort col)
        }

      private def renderFn(p: Props, content: DragToReorder.Content[ColumnPlus]): VdomElement = {
        val selectionCell =
          <.th(
            *.selectionColumnHeader,
            ^.onKeyDown ==> selColKeyDown,
            p.selection.total.checkboxAndOnClick)

        val cols =
          content.items.toVdomArray { i =>
            val c = i.data
            val live = c.column match {
              case Column.DeletionReason => Live // Don't render this title with strike-through
              case _                     => c.live
            }
            <.th(
              *.header(live, i.status),
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
        _RowData <: Row: Reusability,
        _ViewInput     : Reusability,
      ](displayName: String) {

    type RowEditField <: EditorFeature.RowKey
    type RowEditRead <: EditorFeature.Read.ForRow[RowEditField, _]

    protected val editFieldFilter: Option[EditorFeature.FieldKey] => Option[RowEditField#FieldKey]

    protected def reusabilityRowEditor: Reusability[RowEditor]

    protected def viewMaker(row: RowData, vi: ViewInput): Column => Reusable[TagMod]

    // ↑ abstract
    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    // ↓ concrete

    final type RowData   = _RowData
    final type ViewInput = _ViewInput
    final type RowEditor = EditorFeature.ReadWrite.ForRow[RowEditField, RowEditRead]

    case class Props(row      : RowData,
                     viewInput: ViewInput,
                     editor   : RowEditor,
                     cols     : NonEmptyVector[ColumnPlus],
                     rowAsync : AsyncFeature.Read.D0[String],
                     selection: Selection.OneUI[Row.SourceId]) {
      @inline def render = Component.withKey(row.id.key)(this)
    }

    implicit val reusabilityProps: Reusability[Props] = {
      implicit val a = reusabilityRowEditor
      Reusability.caseClass
    }

    protected val reusabilityView: Reusability[(RowData, ViewInput, Column)] =
      implicitly

    final def render(p: Props): VdomElement = {
      val row = p.row

//      val rowStatus: CellStatus =
//        if (row.live is Dead) CellStatus.DeadRow else CellStatus.Normal

      def selCellKeyDown(e: ReactKeyboardEventFromHtml): Callback =
        focusKeyHandlers(e)

      val td = <.td //(*.cell(rowStatus))

      val mkView = viewMaker(row, p.viewInput)

      def renderNormal = {
        val sel = p.selection

        def selCell =
          td(
            ^.onKeyDown ==> selCellKeyDown,
            sel.onClick,
            sel.checkbox(^.tabIndex := -1))

        def colCells =
          p.cols.whole.toVdomArray { colPlus =>
            val col = colPlus.column
            val editor = p.editor.apply(editFieldFilter(col.editorField))
            val view = mkView(col)
            val cp = Cell.Props(editor, view)
            Cell.Component.withKey(col.key)(cp)
          }

        <.tr(selCell, colCells)
      }

      def renderLocked = {
        def colCells =
          p.cols.whole.toVdomArray { colPlus =>
            val col = colPlus.column
            val editor = EditorFeature.ReadWrite.ForCell.doNothing
            val view = mkView(col)
            val cp = Cell.Props(editor, view)
            Cell.Component.withKey(col.key)(cp)
          }

        def lockedSel = <.div(^.cls := "locked", "LOCKED")

        <.tr(td(lockedSel), colCells)
      }

      p.rowAsync match {
        case None                                        => renderNormal
        case Some(AsyncFeature.Status.InProgress)        => renderLocked
        case Some(s: AsyncFeature.Status.Failed[String]) =>
          // Currently, whole-row state is only used when a row is being deleted/restored.
          // To save dev-time, if the RPC fails an alert popups asking to retry/cancel, thus this part of the code
          // should only execute when the row is locked. Whole-row editing + failure won't occur.
          dom.console.warn(s.failure)
          <.tr(
            td(^.colSpan := (p.cols.length + 1),
              <.div(
                s.failure,
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

  private object ReqRow extends RowTemplate[Row.ForReq, (ReqTypes, ProjectWidgets, ProjectWidgets#PubidFormat)]("ReqRow") {
    override type RowEditField = EditorFeature.RowKey.Req
    override type RowEditRead  = EditorFeature.Read.ForReq

    override protected val editFieldFilter = EditorFeature.FieldKey.filterForReq

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
        Vector.empty, // pastPubids unused
        reqTypes)
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
        case Column.DeletionReason    => viewReq.deletionReason getOrElse `N/A`
      }
      c => Reusable.explicitly((row, vi, c))(reusabilityView).map(_ => view(c))
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object ReqCodeGroupRow extends RowTemplate[Row.ForReqCodeGroup, ProjectWidgets]("ReqCodeGroupRow") {
    override type RowEditField = EditorFeature.RowKey.ReqCodeGroup
    override type RowEditRead  = EditorFeature.Read.ForReqCodeGroup

    override protected val editFieldFilter = EditorFeature.FieldKey.filterForReqCodeGroup

    override protected def reusabilityRowEditor = implicitly

    private val reusableNA: Reusable[TagMod] =
      Reusable.byRef(`N/A`)

    override protected def viewMaker(row: RowData, vi: ViewInput): Column => Reusable[TagMod] = {
      val pw = vi

      def ret(c: Column, view: => TagMod): Reusable[TagMod] =
        Reusable.explicitly((row, vi, c))(reusabilityView).map(_ => view)

      def renderCodes: TagMod =
        row match {
          case Row.ForReqCodeGroup(_, _, Some(t)) => pw.reqCodeTreeItem(t)
          case Row.ForReqCodeGroup(_, c, None)    => pw.reqCode(c)
        }

      {
        case _: Column.CustomField
           | _: Column.Implications
           | Column.ReqType
           | Column.Tags
           | Column.Pubid             => reusableNA
        case c@ Column.Title          => ret(c, pw.reqCodeGroupTitle(row.group))
        case c@ Column.Code           => ret(c, renderCodes)
        case c@ Column.DeletionReason => ProjectWidgets.DeletionReason.forReqCodeGroup.fold(reusableNA)(ret(c, _))
      }
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private object Cell {

    final case class Props(editor: EditorFeature.ReadWrite.ForCell,
                           view  : Reusable[TagMod])

    implicit val reusabilityProps: Reusability[Props] =
      Reusability.caseClass

    val cellBase = <.td(^.tabIndex := -1)

    type $ = ScalaComponent.Lifecycle.RenderScope[Props, Unit, Unit]
    type N = dom.html.TableDataCell

    // def domNode = CallbackTo($.getDOMNode.asInstanceOf[N])

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

    def onKeyDown(editor: EditorFeature.ReadWrite.ForCell): ReactKeyboardEventFromHtml => Callback =
      e => CallbackOption.require(doesEventTargetCell(e)) >> (
        focusKeyHandlers(e) | keyCodeSwitch(e) {
          case KeyCode.F2 => editor.startEdit getOrElse Callback.empty
        }
      )

    def render($: $, p: Props): VdomElement =
      cellBase(
//        *.cell(status),
        ^.onKeyDown ==> onKeyDown($.props.editor),
        p.editor.themedRenderOr(p.view))

    val Component = ScalaComponent.builder[Props]("Cell")
      .renderP(render)
      .configure(shouldComponentUpdate)
      .build
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  // Shared

  private val `N/A`: VdomTag =
    <.span(*.`N/A`, "–")

  private def moveFocus(cur: dom.html.Element, ↔ : Movement = Movement.None, ↕ : Movement = Movement.None): Callback =
    Callback {
      val cell: dom.html.Element =
        if ("INPUT" == cur.tagName) // Selection checkbox
          cur.parentElement
        else
          cur
      val z = TableCellZipper(cell) move_- ↔ move_| ↕
      val f: dom.html.Element =
        if (z.colIndex == 0)
          z.focus.children(0).domAsHtml // Selection checkbox
        else
          z.focus
      f.focus()
    }

  private def focusKeyHandlers(e: ReactKeyboardEventFromHtml): CallbackOption[Unit] =
    keyCodeSwitch(e) {
      case KeyCode.Up     => moveFocus(e.currentTarget, ↕ = Movement.Prev)
      case KeyCode.Down   => moveFocus(e.currentTarget, ↕ = Movement.Next)
      case KeyCode.Left   => moveFocus(e.currentTarget, ↔ = Movement.Prev)
      case KeyCode.Right  => moveFocus(e.currentTarget, ↔ = Movement.Next)
      case KeyCode.Home   => moveFocus(e.currentTarget, ↔ = Movement.Head)
      case KeyCode.End    => moveFocus(e.currentTarget, ↔ = Movement.Last)
      case KeyCode.Escape => Callback(e.target.blur())
    } | keyCodeSwitch(e, ctrlKey = true) {
      case KeyCode.Home   => moveFocus(e.currentTarget, Movement.Head, Movement.Head)
      case KeyCode.End    => moveFocus(e.currentTarget, Movement.Last, Movement.Last)
    }
}
