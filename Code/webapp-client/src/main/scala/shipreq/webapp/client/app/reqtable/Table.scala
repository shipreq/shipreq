package shipreq.webapp.client.app.reqtable

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import shipreq.base.util.NonEmptyVector
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.Style.{reqtable => *}
import shipreq.webapp.client.feature.AsyncActionFeature
import shipreq.webapp.client.lib._
import shipreq.webapp.client.widgets.DragToReorder
import AsyncActionFeature.Table.RowState
import AsyncActionFeature.{Locked, renderLocked}
import DataReusability._
import DomUtil._

object Table {

  implicit val reusabilityProps = Reusability.caseClass[Props]

  case class Props(project        : Project,
                   rows           : Rows,
                   colName        : Column.NameResolver,
                   colRenderers   : NonEmptyVector[ColumnRenderer],
                   cellEditors    : CellEditors,
                   editState      : EditState.Table,
                   asyncState     : AsyncState.TableState,
                   selection      : RowSelectionVisible,
                   modViewSettings: EndoFn[ViewSettings] ~=> Callback)

  val Component =
    ReactComponentB[Props]("Table")
      .renderBackend[Backend]
      .configure(shouldComponentUpdate)
      .build

  /** Input is a callback to run after editing starts. */
  type StartEdit = Callback ~=> Callback

  final class Backend($: BackendScope[Props, Unit]) {

    val startCellEdit = ReusableFn[Row, Column, Callback, Callback]((r, c, focus) =>
      $.props >>= (_.cellEditors.startEdit(r, c, focus) getOrElse Callback.empty))

    val reorderColumns = ReusableFn((cols: NonEmptyVector[Column]) =>
      $.props >>= (_.modViewSettings(_ setColumns cols)))

    val clickHeaderToSort = ReusableFn((col: Column) =>
      $.props >>= (_.modViewSettings(
        ViewSettings.order.modify(_ want col))))

    def render(p: Props): ReactElement = {
      val crs  = p.colRenderers
      val rows = p.rows

      val headerProps = HeaderProps(
        crs.map(_.column), p.colName, p.selection, reorderColumns, clickHeaderToSort)

      val renderRows =
        rows.indices.toReactNodeArray { i =>
          val row = rows(i)
          val rs2 = EditState.getRow(p.editState, row.sourceId)
          val as  = AsyncState.get(p.asyncState)(row.sourceId)
          val rp  = RowProps(row, crs, rs2, as, p.selection, startCellEdit(row))
          RowComponent.withKey(row.id.key)(rp)
        }

      // Render
      <.table(*.table,
        HeaderComponent(headerProps),
        <.tbody(renderRows))
    }
  }

  // ===================================================================================================================
  // Header row

  case class HeaderProps(cols     : NonEmptyVector[Column],
                         colName  : Column.NameResolver,
                         selection: RowSelectionVisible,
                         reorder  : NonEmptyVector[Column] ~=> Callback,
                         clickSort: Column ~=> Callback)

  implicit val headerPropReuse = Reusability.caseClass[HeaderProps]

  val HeaderComponent = ReactComponentB[HeaderProps]("Header")
    .renderBackend[HeaderBackend]
    .configure(shouldComponentUpdate)
    .build

  class HeaderBackend($: BackendScope[HeaderProps, Unit]) {

    def selColKeyDown(e: ReactKeyboardEventH): Callback =
      focusKeyHandlers(e)

    def dataColKeyDown(col: Column)(e: ReactKeyboardEventH): Callback =
      focusKeyHandlers(e) | keyCodeSwitch(e) {
        case KeyCode.Space => $.props.flatMap(_ clickSort col)
      }

    val columnDND = new DragToReorder[Column](
      newOrder =>
        NonEmptyVector.maybe(newOrder, Callback.empty)(no =>
          $.props.flatMap(_ reorder no)),

      content =>
        $.props map { p =>
          val name = p.colName

          val selectionCell =
            <.th(
              *.selectionRowHeader,
              ^.onKeyDown ==> selColKeyDown,
              p.selection.total.checkboxAndOnClick)

          val cols =
            content.items.map { i =>
              val c = i.data
              val live = c match {
                case Column.DeletionReason => Live // Don't render this title with strike-through
                case _                     => c.live
              }
              <.th(
                *.columnHeader(live, i.status),
                i.mod,
                ^.tabIndex   := -1,
                ^.onKeyDown ==> dataColKeyDown(c),
                ^.onClick   --> p.clickSort(c),
                name(c)
              )
            }

          <.thead(
            content.rootMod,
            <.tr(
              selectionCell,
              cols))
      })

    def render(p: HeaderProps) =
      columnDND.Component(p.cols.whole)
  }

  // ===================================================================================================================
  // Rows

  val noEdit: StartEdit =
    ReusableFn(_ => Callback.empty)

  case class RowProps(row        : Row,
                      crs        : NonEmptyVector[ColumnRenderer],
                      editState  : EditState.AtRow,
                      asyncState : AsyncState.RowState,
                      selection  : RowSelectionVisible,
                      startEdit  : Column ~=> StartEdit)

  implicit val rowPropReuse = Reusability.caseClass[RowProps]

  val RowComponent =
    ReactComponentB[RowProps]("Row")
      .render_P(renderRow)
      .configure(shouldComponentUpdate)
      .build

  def renderRow(p: RowProps): ReactTagOf[dom.html.TableRow] = {
    val row = p.row

    val rowStatus: ColumnRenderer.Status =
      if (row.live :: Dead) ColumnRenderer.DeadRow else ColumnRenderer.Normal

    def selCellKeyDown(e: ReactKeyboardEventH): Callback =
      focusKeyHandlers(e)

    val td = <.td(*.cell(rowStatus))

    def renderRowNormal(cells: AsyncState.ColStates) = {
      val sel = p.selection(row.sourceId)

      def selCell =
        td(
          ^.onKeyDown ==> selCellKeyDown,
          sel.onClick,
          sel.checkbox(^.tabIndex := -1))

      def colCells =
        p.crs.iterator.map { cr =>
          val col = cr.column
          val cp = CellProps(row, cr, p.editState get col, cells get col, p startEdit col)
          CellComponent.withKey(col.key)(cp)
        }.toReactNodeArray

      <.tr(selCell, colCells)
    }

    def renderRowLocked = {
      def colCells =
        p.crs.iterator.map { cr =>
          val col = cr.column
          val cp = CellProps(row, cr, None, None, noEdit)
          CellComponent.withKey(col.key)(cp)
        }.toReactNodeArray

      <.tr(td(renderLocked), colCells)
    }

    p.asyncState match {
      case RowState(None                      , cells) => renderRowNormal(cells)
      case RowState(Some(Locked)              , _    ) => renderRowLocked
      case RowState(Some(s: AsyncState.Failed), _    ) =>
        // Currently, whole-row state is only used when a row is being deleted/restored.
        // To save dev-time, if the RPC fails an alert popups asking to retry/cancel, thus this part of the code
        // should only execute when the row is locked. Whole-row editing + failure won't occur.
        dom.console.warn(s.failure)
        <.tr(
          td(^.colSpan := (p.crs.length + 1),
            renderAsyncState(s)))
    }
  }

  // ===================================================================================================================
  // Cells

  case class CellProps(row       : Row,
                       cr        : ColumnRenderer,
                       cellEditor: Option[CellEditor],
                       asyncState: AsyncState.Single,
                       startEdit : StartEdit)

  implicit val cellPropReuse = Reusability.caseClass[CellProps]

  val CellComponent =
    ReactComponentB[CellProps]("Cell")
      .renderBackend[CellBackend]
      .configure(shouldComponentUpdate)
      .build

  private val cellBase = <.td(^.tabIndex := -1)

  final class CellBackend($: BackendScope[CellProps, Unit]) {
    type N = dom.html.TableDataCell

    def domNode = CallbackTo($.getDOMNode().asInstanceOf[N])

    /**
     * When a Button in the cell is clicked, we still get the event here in which case, the focus is set after the
     * button callback runs, meaning that (because separate modState()s don't compose) we trample the state change made by
     * the button, and replace it with a focus update.
     *
     * Rather than force all cell children to stop propagation of events, we apply so logic here to filter the events to
     * which we react.
     */
    def doesEventTargetCell(e: ReactEventH): Boolean =
      e.target == e.currentTarget || e.target.tabIndex < 0

    def onKeyDown(e: ReactKeyboardEventH): Callback =
      CallbackOption.require(doesEventTargetCell(e)) >> (
        focusKeyHandlers(e) | keyCodeSwitch(e) {
          case KeyCode.F2 => startEdit
        }
      )

    def focus: Callback =
      domNode.map { n =>
        val target = focusableChildren(n).nextOption() getOrElse n
        target.focus()
      }

    def startEdit: Callback =
      $.props >>= (_ startEdit focus)

    def render(p: CellProps) = {
      val col = p.cr.column
      val (status, roView) = p.cr.render(p.row)
      // TODO roView should be non-strict or a fn

      def editView: Option[ReactElement] =
        p.cellEditor.flatMap(_.render(p.row, col))

      cellBase(
        *.cell(status),
        ^.onDblClick --> startEdit,
        ^.onKeyDown ==> onKeyDown,
        p.asyncState match {
          case None    => editView getOrElse[ReactElement] roView
          case Some(s) => renderAsyncState(s): ReactElement
        })
    }
  }

  // ===================================================================================================================
  // Shared fns

  def moveFocus(cur: dom.html.Element, ↔ : Movement = Movement.None, ↕ : Movement = Movement.None): Callback =
    Callback {
      val cell: dom.html.Element =
        if ("INPUT" == cur.tagName) // Selection checkbox
          cur.parentElement
        else
          cur
      val z = TableCellZipper(cell) move_- ↔ move_| ↕
      val f: dom.html.Element =
        if (z.colIndex == 0)
          z.focus.children(0).castHtml // Selection checkbox
        else
          z.focus
      f.focus()
    }

  def focusKeyHandlers(e: ReactKeyboardEventH): CallbackOption[Unit] =
    keyCodeSwitch(e) {
      case KeyCode.Up       => moveFocus(e.currentTarget, ↕ = Movement.Prev)
      case KeyCode.Down     => moveFocus(e.currentTarget, ↕ = Movement.Next)
      case KeyCode.Left     => moveFocus(e.currentTarget, ↔ = Movement.Prev)
      case KeyCode.Right    => moveFocus(e.currentTarget, ↔ = Movement.Next)
      case KeyCode.Home     => moveFocus(e.currentTarget, ↔ = Movement.Head)
      case KeyCode.End      => moveFocus(e.currentTarget, ↔ = Movement.Last)
      case KeyCode.Escape   => Callback(e.target.blur())
    } | keyCodeSwitch(e, ctrlKey = true) {
      case KeyCode.Home     => moveFocus(e.currentTarget, Movement.Head, Movement.Head)
      case KeyCode.End      => moveFocus(e.currentTarget, Movement.Last, Movement.Last)
    }
}
