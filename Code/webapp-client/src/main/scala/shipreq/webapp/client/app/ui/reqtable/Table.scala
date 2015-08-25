package shipreq.webapp.client.app.ui.reqtable

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import shipreq.base.util.NonEmptyVector
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.RemoteDataEditor
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.app.ui.reqtable.edit.ColumnEditors
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.util._
import DomUtil._
import tmp.ReactPatches._

object Table {

  implicit val reusabilityRDE : Reusability[RemoteDataEditor.State]         = Reusability.byRef
  implicit val reusabilityCEs : Reusability[ColumnEditors]                  = Reusability.byRef
  implicit val reusabilityCR  : Reusability[ColumnRenderer]                 = Reusability.byRef
  implicit val reusabilityCRs : Reusability[NonEmptyVector[ColumnRenderer]] = Reusability.byRef
  implicit val reusabilityRows: Reusability[Vector[Row]]                    = Reusability.byRef
  implicit val reusabilityRow : Reusability[Row]                            = Reusability.byRef
  implicit val reusabilityCTS : Reusability[Cell.TableState]                = Reusability.byRef
  implicit val reusabilityCRS : Reusability[Cell.RowState]                  = Reusability.byRef

  // ===================================================================================================================
  // Table

  implicit val reusabilityProps = Reusability.caseClass[Props]

  case class Props(project     : Project,
                   rows        : Vector[Row],
                   colRenderers: NonEmptyVector[ColumnRenderer],
                   colEditors  : ColumnEditors,
                   cells       : Cell.TableState)

  val Component =
    ReactComponentB[Props]("Table")
      .stateless
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(shouldComponentUpdate)
      .build

  final class Backend($: BackendScope[Props, Unit]) {

    val startCellEdit = ReusableFn[Row, Column, Callback]((row, col) =>
      $.propsCB.map { p =>
        if (p.cells(row.id, col).isEmpty)
          p.colEditors.startCellEditing(row, col)
            .foreach(_.runNow())
      }
    )

    def render: ReactElement = {
      val p     = $.props
      val crs   = p.colRenderers
      val rows  = p.rows

      def renderRows =
        rows.indices.toReactNodeArray { i =>
          val row   = rows(i)
          val cells = p.cells(row.id)
          val rp    = RowProps(row, crs, cells, startCellEdit(row))
          RowComponent.withKey(row.id.key)(rp)
        }

      // Render
      <.table(*.table,
        HeaderComponent(crs),
        <.tbody(renderRows))
    }
  }

  // ===================================================================================================================
  // Header row

  val HeaderComponent = ReactComponentB[NonEmptyVector[ColumnRenderer]]("Header")
    .backend(new HeaderBackend(_))
    .render(_.backend.render)
    .configure(shouldComponentUpdate)
    .build

  class HeaderBackend($: BackendScope[NonEmptyVector[ColumnRenderer], Unit]) {

    def onKeyDown(e: ReactKeyboardEventH): Callback =
      (e.nativeEvent.keyCode match {
        case KeyCode.Up     => moveFocus_|(e.currentTarget, _ - 1)
        case KeyCode.Down   => moveFocus_|(e.currentTarget, _ => 0)
        case KeyCode.Left   => moveFocus_-(e.currentTarget, -1)
        case KeyCode.Right  => moveFocus_-(e.currentTarget,  1)
        case KeyCode.Escape => Callback(e.currentTarget.blur())
        case _              => Callback.empty
      }).flatMapUnlessEmpty(
          _ << e.preventDefaultCB)

    def moveFocus_-(cur: dom.html.Element, by: Int): Callback =
      siblingAtOffset(cur, by)
        .map(_.castHtml.focus())

    def moveFocus_|(th: dom.html.Element, chooseRow: Int => Int): Callback =
      siblingIndex(th).map { col =>
        val tr     = th.parentElement
        val thead  = tr.parentElement
        val table  = thead.parentElement
        val tbody  = table.children(1)
        val rows   = tbody.children
        val rowcnt = rows.length
        if (rowcnt > 0) {
          val row = chooseRow(rowcnt)
          rows(row).children(col).castHtml.focus()
        }
      }

    def render = {
      val crs = $.props
      <.thead(
        <.tr(
          crs.toStream.map(cr =>
            <.th(
              *.columnHeader(cr.column.live),
              ^.tabIndex := 0,
              ^.onKeyDown ==> onKeyDown,
              cr.header))))
    }
  }

  // ===================================================================================================================
  // Rows

  implicit val rowPropReuse = Reusability.caseClass[RowProps]

  case class RowProps(row      : Row,
                      crs      : NonEmptyVector[ColumnRenderer],
                      cells    : Cell.RowState,
                      startEdit: Column ~=> Callback)

  val RowComponent =
    ReactComponentB[RowProps]("Row")
      .render_P(renderRow)
      .configure(shouldComponentUpdate)
      .build

  def renderRow(p: RowProps) =
    <.tr(
      p.crs.toStream.map { cr =>
        val col = cr.column
        val cp = CellProps(p.row, cr, p.cells get col, p.startEdit fnA col)
        CellComponent(cp)
      }
    )

  // ===================================================================================================================
  // Cells

  implicit val cellPropReuse = Reusability.caseClass[CellProps]

  case class CellProps(row      : Row,
                       cr       : ColumnRenderer,
                       cellState: Cell.State,
                       startEdit: ReusableFnA[Column, Callback])

  val CellComponent =
    ReactComponentB[CellProps]("Cell")
      .stateless
      .backend(new CellBackend(_))
      .render(_.backend.render)
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
      Callback.ifTrue(doesEventTargetCell(e) && checkModKeys(e),
        (e.nativeEvent.keyCode match {
          case KeyCode.F2     => startEdit
          case KeyCode.Up     => moveFocus_|(-1)
          case KeyCode.Down   => moveFocus_|( 1)
          case KeyCode.Left   => moveFocus_-(-1)
          case KeyCode.Right  => moveFocus_-( 1)
          case KeyCode.Escape => domNode.map(_.blur())
          case _              => Callback.empty
        }).flatMapUnlessEmpty(
            _ << e.preventDefaultCB)
    )

    def moveFocus_-(by: Int): Callback =
      for {
        cur <- domNode
        tgt <- siblingAtOffset(cur, by)
      } yield
        tgt.castHtml.focus()

    def moveFocus_|(by: Int): Callback =
      for {
        cur <- domNode
        col <- siblingIndex(cur)
        row <- siblingAtOffset(cur.parentElement, by)
      } yield
        row.children(col).castHtml.focus()

    def startEdit: Callback =
      $.propsCB >>= (_.startEdit())

    def render = {
      val p = $.props
      val (status, roView) = p.cr.render(p.row)
      cellBase(
        *.cell(status),
        ^.onDblClick --> startEdit,
        ^.onKeyDown ==> onKeyDown,
        p.cellState.fold(roView)(_.render))
    }
  }
}
