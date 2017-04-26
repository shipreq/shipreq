package shipreq.webapp.client.project.app.reqtable

import scalacss.Domain
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.html_<^._
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.client.base.feature.AsyncFeature
import shipreq.webapp.client.base.lib.{DataReusability => _, _}
import shipreq.webapp.client.base.ui.EditTheme
import shipreq.webapp.client.project.app.Style.{reqtable => *}
import shipreq.webapp.client.project.feature.ContentEditorFeature
import shipreq.webapp.client.project.lib._
import shipreq.webapp.client.project.widgets.DragToReorder
import DataReusability._
import DomUtil._

object Table {

  def renderLocked = <.div(^.cls := "locked", "LOCKED")


  implicit val reusabilityProps = Reusability.never[Props] // TODO .caseClass[Props]

  case class Props(project        : Project,
                   rows           : Rows,
                   colName        : Column.NameResolver,
                   colRenderers   : NonEmptyVector[ColumnRenderer],
                   cellEditors    : ContentEditorFeature.D2.Feature[Row, Column],
                   editState      : ContentEditorFeature.D2.State.ReadOnly[Row.SourceId, Column],
                   asyncState     : AsyncFeature.ReadOnly.D2[Row.SourceId, Option[Column], String],
                   selection      : RowSelectionVisible,
                   modViewSettings: EndoFn[ViewSettings] ~=> Callback)

  val Component =
    ScalaComponent.builder[Props]("Table")
      .renderBackend[Backend]
      .configure(shouldComponentUpdate)
      .build

  final class Backend($: BackendScope[Props, Unit]) {

    val reorderColumns = Reusable.fn((cols: NonEmptyVector[Column]) =>
      $.props >>= (_.modViewSettings(_ setColumns cols)))

    val clickHeaderToSort = Reusable.fn((col: Column) =>
      $.props >>= (_.modViewSettings(
        ViewSettings.order.modify(_ want col))))

    def render(p: Props): VdomElement = {
      val crs  = p.colRenderers
      val rows = p.rows

      val headerProps = HeaderProps(
        crs.map(_.column), p.colName, p.selection, reorderColumns, clickHeaderToSort)

      val renderRows =
        rows.indices.toVdomArray { i =>
          val row = rows(i)
          val es  = p.editState(row.sourceId)
          val as  = p.asyncState(row.sourceId)
          val rp  = RowProps(p.project, row, crs, p.cellEditors, es, as, p.selection)
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

  implicit val headerPropReuse = Reusability.never[HeaderProps] // TODO caseClass[HeaderProps]

  val HeaderComponent = ScalaComponent.builder[HeaderProps]("Header")
    .renderBackend[HeaderBackend]
    .configure(shouldComponentUpdate)
    .build

  class HeaderBackend($: BackendScope[HeaderProps, Unit]) {

    def selColKeyDown(e: ReactKeyboardEventFromHtml): Callback =
      focusKeyHandlers(e)

    def dataColKeyDown(col: Column)(e: ReactKeyboardEventFromHtml): Callback =
      focusKeyHandlers(e) | keyCodeSwitch(e) {
        case KeyCode.Space => $.props.flatMap(_ clickSort col)
      }

    val columnDND = new DragToReorder[Column](
      newOrder =>
        NonEmptyVector.maybe(newOrder, Callback.empty)(no =>
          $.props.flatMap(_ reorder no)),

      content =>
        $.props map[VdomElement] { p =>
          val name = p.colName

          val selectionCell =
            <.th(
              *.selectionRowHeader,
              ^.onKeyDown ==> selColKeyDown,
              p.selection.total.checkboxAndOnClick)

          val cols =
            content.items.toVdomArray { i =>
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
                name(c))
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

  case class RowProps(project    : Project,
                      row        : Row,
                      crs        : NonEmptyVector[ColumnRenderer],
                      cellEditors: ContentEditorFeature.D2.Feature[Row, Column],
                      editState  : ContentEditorFeature.D1.State.ReadOnly[Column],
                      asyncState : AsyncFeature.ReadOnly.D1[Option[Column], String],
                      selection  : RowSelectionVisible)

  implicit val rowPropReuse = Reusability.never[RowProps] // TODO .caseClass[RowProps]

  val RowComponent =
    ScalaComponent.builder[RowProps]("Row")
      .render_P(renderRow)
      .configure(shouldComponentUpdate)
      .build

  def renderRow(p: RowProps): VdomTagOf[dom.html.TableRow] = {
    val row = p.row

    val rowStatus: CellStatus =
      if (row.live is Dead) CellStatus.DeadRow else CellStatus.Normal

    def selCellKeyDown(e: ReactKeyboardEventFromHtml): Callback =
      focusKeyHandlers(e)

    val td = <.td(*.cell(rowStatus))

    def renderRowNormal = {
      val sel = p.selection(row.sourceId)

      def selCell =
        td(
          ^.onKeyDown ==> selCellKeyDown,
          sel.onClick,
          sel.checkbox(^.tabIndex := -1))

      def colCells =
        p.crs.iterator.map { cr =>
          val col = cr.column
          val cp = CellProps(p.project, row, cr, p.cellEditors, p editState col, p asyncState Some(col))
          CellComponent.withKey(col.key)(cp)
        }.toVdomArray

      <.tr(selCell, colCells)
    }

    def renderRowLocked = {
      def colCells =
        p.crs.iterator.map { cr =>
          val col = cr.column
          val cp = CellProps(p.project, row, cr, ContentEditorFeature.D2.Feature.Nop, None, None)
          CellComponent.withKey(col.key)(cp)
        }.toVdomArray

      <.tr(td(renderLocked), colCells)
    }

    import AsyncFeature.Status
    p.asyncState(None) match {
      case None                           => renderRowNormal
      case Some(Status.Locked)            => renderRowLocked
      case Some(s: Status.Failed[String]) =>
        // Currently, whole-row state is only used when a row is being deleted/restored.
        // To save dev-time, if the RPC fails an alert popups asking to retry/cancel, thus this part of the code
        // should only execute when the row is locked. Whole-row editing + failure won't occur.
        dom.console.warn(s.failure)
        <.tr(
          td(^.colSpan := (p.crs.length + 1),
            <.div(
              s.failure,
              <.button("Retry", ^.onClick --> s.retry),
              <.button("Abort", ^.onClick --> s.cancel))))
    }
  }

  // ===================================================================================================================
  // Cells

  sealed abstract class CellStatus
  object CellStatus {
    case object Normal  extends CellStatus
    case object DeadRow extends CellStatus
    case object `N/A`   extends CellStatus
    implicit def univEq: UnivEq[CellStatus] = UnivEq.derive
    val domain = Domain.ofValues[CellStatus](Normal, DeadRow, `N/A`)
  }

  case class CellProps(project    : Project,
                       row        : Row,
                       cr         : ColumnRenderer,
                       cellEditors: ContentEditorFeature.D2.Feature[Row, Column],
                       editState  : ContentEditorFeature.D0.State,
                       asyncState : AsyncFeature.ReadOnly.D0[String]) {
    def column = cr.column
    def startEdit: Option[Callback] = cellEditors(row)(column).startEdit(project)
  }

  implicit val cellPropReuse = Reusability.never[CellProps] // TODO caseClass[CellProps]

  val CellComponent =
    ScalaComponent.builder[CellProps]("Cell")
      .renderBackend[CellBackend]
      .configure(shouldComponentUpdate)
      .build

  private val cellBase = <.td(^.tabIndex := -1)

  final class CellBackend($: BackendScope[CellProps, Unit]) {
    type N = dom.html.TableDataCell

    def domNode = CallbackTo($.getDOMNode.asInstanceOf[N])

    val startEdit: Callback =
      $.props.flatMap(_.startEdit getOrElse Callback.empty)

    val editableInline =
      EditTheme.editableInline(startEdit)

    def renderAsyncEditorOrValue(p: CellProps, view: => TagMod): TagMod = {
      def view2: TagMod =
        p.startEdit match {
          case Some(_) => TagMod(editableInline, view)
          case None    => view
        }
      p.editState.renderOr(p.asyncState)(view2)
    }

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

    def onKeyDown(e: ReactKeyboardEventFromHtml): Callback =
      CallbackOption.require(doesEventTargetCell(e)) >> (
        focusKeyHandlers(e) | keyCodeSwitch(e) {
          case KeyCode.F2 => startEdit
        }
      )

    def render(p: CellProps) = {
      val view = p.cr.view(p.row)

      val status: CellStatus =
        view match {
          case ColumnRenderer.`N/A` => CellStatus.`N/A`
          case _: ColumnRenderer.Render => p.row.live match {
            case Live => CellStatus.Normal
            case Dead => CellStatus.DeadRow
          }
        }

      cellBase(
        *.cell(status),
        ^.onKeyDown ==> onKeyDown,
        renderAsyncEditorOrValue(p, view.render))
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
          z.focus.children(0).domAsHtml // Selection checkbox
        else
          z.focus
      f.focus()
    }

  def focusKeyHandlers(e: ReactKeyboardEventFromHtml): CallbackOption[Unit] =
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
