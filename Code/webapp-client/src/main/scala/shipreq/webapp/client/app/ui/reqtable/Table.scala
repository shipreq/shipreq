package shipreq.webapp.client.app.ui.reqtable

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import shipreq.base.util.NonEmptyVector
import shipreq.base.util.ScalaExt.EndoFn
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.DragToReorder
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.app.ui.reqtable.edit.ColumnEditors
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.TCB
import shipreq.webapp.client.util._
import DomUtil._
import tmp.ReactPatches._

object Table {

  implicit val reusabilityCEs : Reusability[ColumnEditors]                  = Reusability.byRef
  implicit val reusabilityCR  : Reusability[ColumnRenderer]                 = Reusability.byRef // TODO This is a problem
  implicit val reusabilityCs  : Reusability[NonEmptyVector[Column]]         = reusabilityNonEmptyVector
  implicit val reusabilityCRs : Reusability[NonEmptyVector[ColumnRenderer]] = reusabilityNonEmptyVector
  implicit val reusabilityCTS : Reusability[Cell.TableState]                = Reusability.byRef
  implicit val reusabilityCRS : Reusability[Cell.RowState]                  = Reusability.byRef
  implicit val reusabilityCCS : Reusability[Cell.State]                     = Reusability.byRef
  implicit val reusabilityRows: Reusability[Vector[Row]]                    = Reusability.byRef // Each row will be checked anyway

  // ===================================================================================================================
  // Table

  implicit val reusabilityProps = Reusability.caseClass[Props]

  case class Props(project        : Project,
                   rows           : Vector[Row],
                   colName        : Column.NameResolver,
                   colRenderers   : NonEmptyVector[ColumnRenderer],
                   colEditors     : ColumnEditors,
                   cells          : Cell.TableState,
                   modViewSettings: EndoFn[ViewSettings] ~=> Callback)

  val Component =
    ReactComponentB[Props]("Table")
      .renderBackend[Backend]
      .configure(shouldComponentUpdate)
      .build

  final class Backend($: BackendScope[Props, Unit]) {

    val startCellEdit = ReusableFn[Row, Column, TCB.Finally, Callback]((row, col, fin) =>
      $.props.map { p =>
        if (p.cells(row.sourceId, col).isEmpty)
          p.colEditors.startCellEditing(row, col, fin)
            .foreach(_.runNow())
      }
    )

    val reorderColumns = ReusableFn((cols: NonEmptyVector[Column]) =>
      $.props >>= (_.modViewSettings(_ setColumns cols)))

    val clickHeaderToSort = ReusableFn((col: Column) =>
      $.props >>= (_.modViewSettings(
        ViewSettings.order.modify(_ want col))))

    def render(p: Props): ReactElement = {
      val crs  = p.colRenderers
      val rows = p.rows

      def renderRows =
        rows.indices.toReactNodeArray { i =>
          val row   = rows(i)
          val cells = p.cells(row.sourceId)
          val rp    = RowProps(row, crs, cells, startCellEdit(row))
          RowComponent.withKey(row.id.key)(rp)
        }

      // Render
      <.table(*.table,
        HeaderComponent(HeaderProps(crs.map(_.column), p.colName, reorderColumns, clickHeaderToSort)),
        <.tbody(renderRows))
    }
  }

  // ===================================================================================================================
  // Header row

  case class HeaderProps(cols     : NonEmptyVector[Column],
                         colName  : Column.NameResolver,
                         reorder  : NonEmptyVector[Column] ~=> Callback,
                         clickSort: Column ~=> Callback)

  implicit val headerPropReuse = Reusability.caseClass[HeaderProps]

  val HeaderComponent = ReactComponentB[HeaderProps]("Header")
    .renderBackend[HeaderBackend]
    .configure(shouldComponentUpdate)
    .build

  class HeaderBackend($: BackendScope[HeaderProps, Unit]) {

    def onKeyDown(col: Column)(e: ReactKeyboardEventH): Callback =
      keyCodeSwitch(e) {
        case KeyCode.Up     => moveFocus_|(e.currentTarget, _ - 1)
        case KeyCode.Down   => moveFocus_|(e.currentTarget, _ => 0)
        case KeyCode.Left   => moveFocus_-(e.currentTarget, -1)
        case KeyCode.Right  => moveFocus_-(e.currentTarget,  1)
        case KeyCode.Escape => Callback(e.currentTarget.blur())
        case KeyCode.Space  => $.props.flatMap(_ clickSort col)
      }

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

    val columnDND = new DragToReorder[Column](
      newOrder =>
        NonEmptyVector.maybe(newOrder, Callback.empty)(no =>
          $.props.flatMap(_ reorder no)),

      content =>
        $.props map { p =>
          val name = p.colName
          var first = true
          <.thead(
            content.rootMod,
            <.tr(
              content.items.map { i =>
                val isFirst = first && { first = false; true }
                val c = i.data
                <.th(
                  *.columnHeader(c.live, i.status),
                  i.mod,
                  ^.tabIndex   := (if (isFirst) 0 else -1),
                  ^.onKeyDown ==> onKeyDown(c),
                  ^.onClick   --> p.clickSort(c),
                  name(c)
                )}))
      })

    def render(p: HeaderProps) =
      columnDND.Component(p.cols.whole)
  }

  // ===================================================================================================================
  // Rows

  case class RowProps(row      : Row,
                      crs      : NonEmptyVector[ColumnRenderer],
                      cells    : Cell.RowState,
                      startEdit: Column ~=> (TCB.Finally ~=> Callback))

  implicit val rowPropReuse = Reusability.caseClass[RowProps]

  val RowComponent =
    ReactComponentB[RowProps]("Row")
      .render_P(renderRow)
      .configure(shouldComponentUpdate)
      .build

  def renderRow(p: RowProps) =
    <.tr(
      p.crs.toStream.map { cr =>
        val col = cr.column
        val cp = CellProps(p.row, cr, p.cells get col, p startEdit col)
        CellComponent.withKey(col.key)(cp)
      }
    )

  // ===================================================================================================================
  // Cells

  case class CellProps(row      : Row,
                       cr       : ColumnRenderer,
                       cellState: Cell.State,
                       startEdit: TCB.Finally ~=> Callback)

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
      CallbackOption.require(doesEventTargetCell(e)) >>
      keyCodeSwitch(e) {
        case KeyCode.F2     => startEdit
        case KeyCode.Up     => moveFocus_|(-1)
        case KeyCode.Down   => moveFocus_|( 1)
        case KeyCode.Left   => moveFocus_-(-1)
        case KeyCode.Right  => moveFocus_-( 1)
        case KeyCode.Escape => domNode.map(_.blur())
      }

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
      $.props >>= (_ startEdit TCB.Finally(domNode.map(_.focus())))

    def render(p: CellProps) = {
      val (status, roView) = p.cr.render(p.row)
      cellBase(
        *.cell(status),
        ^.onDblClick --> startEdit,
        ^.onKeyDown ==> onKeyDown,
        p.cellState.fold(roView)(_.render))
    }
  }
}
