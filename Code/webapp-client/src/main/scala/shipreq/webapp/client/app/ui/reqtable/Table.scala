package shipreq.webapp.client.app.ui.reqtable

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import scala.scalajs.js, js.{UndefOr, undefined}
import scalaz.std.anyVal.intInstance
import scalaz.syntax.equal._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.NonEmptyVector
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.util._
import edit.ColumnEditors

object Table {

  implicit val reusabilityCEs : Reusability[ColumnEditors]                  = Reusability.byRef
  implicit val reusabilityCRs : Reusability[NonEmptyVector[ColumnRenderer]] = Reusability.byRef
  implicit val reusabilityRows: Reusability[Vector[Row]]                    = Reusability.byRef
  implicit val reusabilityRow : Reusability[Row]                            = Reusability.byRef
  implicit val reusabilityCNR : Reusability[Column.NameResolver]            = Reusability.byRef
  implicit val reusabilityCTS : Reusability[Cell.TableState]                = Reusability.byRef
  implicit val reusabilityCRS : Reusability[Cell.RowState]                  = Reusability.byRef

  implicit val reusabilityFocus = Reusability.caseClass[Focus]
  implicit val reusabilityProps = Reusability.caseClass[Props]

  case class Focus(rowInd: Int, col: Column) {
    @inline def row(rows: Vector[Row]): Option[Row] =
      try { Some(rows(rowInd)) } catch { case _: Throwable => None }
  }

  case class Props(project     : Project,
                   rows        : Vector[Row],
                   colRenderers: NonEmptyVector[ColumnRenderer],
                   colEditors  : ColumnEditors,
                   cells       : Cell.TableState,
                   focus       : ReusableVar[Option[Focus]])

  val Component =
    ReactComponentB[Props]("Table")
      .stateless
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(KeyPressListener.install(), shouldComponentUpdate)
      .build

  sealed trait KeyboardAction
  case object FocusUp    extends KeyboardAction
  case object FocusDown  extends KeyboardAction
  case object FocusLeft  extends KeyboardAction
  case object FocusRight extends KeyboardAction
  case object FocusNone  extends KeyboardAction
  case object EditStart  extends KeyboardAction

  final class Backend($: BackendScope[Props, Unit]) extends KeyPressListener {

    val keyDispatch =
      consumeHandledKbEvent(
        filterUntargeted(
          matchKeyCodeNoMods {
            case KeyCode.Up     => onKeyboardAction(FocusUp)
            case KeyCode.Down   => onKeyboardAction(FocusDown)
            case KeyCode.Left   => onKeyboardAction(FocusLeft)
            case KeyCode.Right  => onKeyboardAction(FocusRight)
            case KeyCode.Escape => onKeyboardAction(FocusNone)
            case KeyCode.F2     => onKeyboardAction(EditStart)
          }))

    override def onKeyDown(e: dom.KeyboardEvent): Callback =
      keyDispatch(e) getOrElse Callback.empty

    def onKeyboardAction(kb: KeyboardAction): Callback = {
      val p  = $.props
      val fv = p.focus

      def limit(i: Int, m: Int): Int =
        if (i < 0) m else if (i > m) 0 else i

      def withFocus(f: Focus => Callback) =
        fv.value.fold(Callback.empty)(f)

      def withFocusO(f: Focus => Option[Callback]) =
        fv.value flatMap f getOrElse Callback.empty

      def focusMod(nf: Focus => Focus) =
        withFocus(f => fv set Some(nf(f)))

      def focusShiftRow(add: Int) = focusMod { f =>
        val r = limit(f.rowInd + add, $.props.rows.length - 1)
        f.copy(rowInd = r)
      }

      def focusShiftCol(add: Int) = focusMod { f =>
        val cs = $.props.colRenderers.whole
        val i = cs.indexWhere(_.column ≟ f.col)
        val j = limit(i + add, cs.size - 1)
        val c = cs(j).column
        f.copy(col = c)
      }

      kb match {
        case FocusUp    => focusShiftRow(-1)
        case FocusDown  => focusShiftRow(1)
        case FocusLeft  => focusShiftCol(-1)
        case FocusRight => focusShiftCol(1)
        case FocusNone  => fv set None
        case EditStart  => withFocusO(f => f.row(p.rows) flatMap (startCellEditing(_, f.col)))
      }
    }

    def startCellEditing(row: Row, col: Column): Option[Callback] = {
      val p = $.props
      if (p.cells(row.id, col).isDefined)
        // Already has cell state
        None
      else
        p.colEditors.startCellEditing(row, col) //.map(_ >>> p.focus.set(None))
    }

    // This is ok because $.props.focus is dereferenced INSIDE the function
    val setFocusFn = ReusableFn[Int, Column, Callback](
      (i, c) => $.props.focus.set(Some(Focus(i, c))))

    def render: ReactElement = {
      val p     = $.props
      val crs   = p.colRenderers
      val rows  = p.rows
      val focus = p.focus.value

      def renderRows =
        rows.indices.toReactNodeArray { i =>
          val row = rows(i)
          val curFocus = focus.filter(_.rowInd ≟ i).map(_.col)
          val rowCells = p.cells(row.id)
          val rp = RowProps(row, crs, rowCells, curFocus, setFocusFn(i))
          RowComponent.withKey(row.id.key)(rp)
        }

      // Render
      <.table(*.table,
        HeaderComponent(crs),
        <.tbody(renderRows))
    }
  }

  // ===================================================================================================================

  val HeaderComponent = ReactComponentB[NonEmptyVector[ColumnRenderer]]("Header")
    .render_P(crs =>
      <.thead(
        <.tr(
          crs.toStream.map(cr =>
            <.th(
              *.columnHeader(cr.column.live),
              cr.header)))))
    .configure(shouldComponentUpdate)
    .build

  // ===================================================================================================================

  implicit val rowPropReuse = Reusability.caseClass[RowProps]

  case class RowProps(row     : Row,
                      crs     : NonEmptyVector[ColumnRenderer],
                      cells   : Cell.RowState,
                      focus   : Option[Column],
                      setFocus: Column ~=> Callback)

  val RowComponent =
    ReactComponentB[RowProps]("Row")
      .render_P(renderRow(_))
      .configure(shouldComponentUpdate)
      .build

  /**
   * When a button in the cell is clicked, we still get the event here in which case, the focus is set after the
   * button callback runs, meaning that (because separate modState()s don't compose) we trample the state change made by
   * the button, and replace it with a focus update.
   *
   * Rather than force all cell children to stop propagation of events, we apply so logic here to filter the events to
   * which we react.
   */
  def onCellClick(setFocus: => Callback): ReactMouseEventH => UndefOr[Callback] = e =>
    if (e.target == e.currentTarget || clickToFocusTargets.contains(e.target.tagName.toLowerCase))
      setFocus
    else
      undefined

  private val clickToFocusTargets =
    Set("span", "div", "label", "pre", "code", "p")

  def renderRow(p: RowProps) =
    <.tr(
      p.crs.toStream.map { cr =>
        val col = cr.column
        val (status, readOnlyView) = cr.render(p.row)
        <.td(
          *.cell((status, p.focus.exists(_ ≟ col))),
          ^.onClick ==>? onCellClick(p setFocus col),
          p.cells.get(cr.column).fold(readOnlyView)(_.render))
      })
}
