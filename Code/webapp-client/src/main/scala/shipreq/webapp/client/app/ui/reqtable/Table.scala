package shipreq.webapp.client.app.ui.reqtable

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import scala.scalajs.js, js.{UndefOr, undefined}
import scalaz.effect.IO
import scalaz.std.anyVal.intInstance
import scalaz.syntax.equal._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.effect.IoUtils.{nop, IoExt}
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

  implicit val reusabilityFocus = Reusability.caseclass2(Focus.unapply)
  implicit val reusabilityProps = Reusability.caseclass6(Props.unapply)

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

  class KeyUniqueness {
    val keysSeen = new scala.collection.mutable.HashMap[js.Any, Int]

    def apply(k: js.Any): js.Any = {
      var k2 = k
      val n = keysSeen.get(k).fold(1){r =>
        k2 = k.toString + "!" + r
        r + 1
      }
      keysSeen.update(k, n)
      k2
    }
  }

  val KeyCode_f2 = 113 // TODO temporary

  final class Backend($: BackendScope[Props, Unit]) extends KeyPressListener {

    val keyDispatch =
      consumeHandledKbEvent(
        filterUntargeted(
          matchKeyCodeNoMods {
            case KeyCode.up     => onKeyboardAction(FocusUp)
            case KeyCode.down   => onKeyboardAction(FocusDown)
            case KeyCode.left   => onKeyboardAction(FocusLeft)
            case KeyCode.right  => onKeyboardAction(FocusRight)
            case KeyCode.escape => onKeyboardAction(FocusNone)
            case KeyCode_f2     => onKeyboardAction(EditStart)
          }))

    override def onKeyDown(e: dom.KeyboardEvent): IO[Unit] =
      keyDispatch(e) getOrElse nop

    def onKeyboardAction(kb: KeyboardAction): IO[Unit] = {
      val p  = $.props
      val fv = p.focus

      def limit(i: Int, m: Int): Int =
        if (i < 0) m else if (i > m) 0 else i

      def withFocus(f: Focus => IO[Unit]) =
        fv.value.fold(nop)(f)

      def withFocusO(f: Focus => Option[IO[Unit]]) =
        fv.value flatMap f getOrElse nop

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

    def startCellEditing(row: Row, col: Column): Option[IO[Unit]] = {
      val p = $.props
      if (p.cells(row.id, col).isDefined)
        // Already has cell state
        None
      else
        p.colEditors.startCellEditing(row, col) //.map(_ >>> p.focus.set(None))
    }

    // This is ok because $.props.focus is dereferenced INSIDE the function
    val setFocusFn = ReusableFn[Int, Column, IO[Unit]](
      (i, c) => $.props.focus.set(Some(Focus(i, c))))

    def render: ReactElement = {
      val p     = $.props
      val crs   = p.colRenderers
      val rows  = p.rows
      val focus = p.focus.value

      val uniqKey = new KeyUniqueness
      val rowKey: Row => js.Any = {
        case r: GenericReqRow   => uniqKey(r.req.id.value)
        case r: ReqCodeGroupRow => "g" + r.id.value.value.toString
      }

      def renderRows =
        (0 until rows.length).toReactNodeArray { i =>
          val row = rows(i)
          val curFocus = focus.filter(_.rowInd ≟ i).map(_.col)
          val rowCells = p.cells(row.id)
          val rp = RowProps(row, crs, rowCells, curFocus, setFocusFn(i))
          RowComponent.withKey(rowKey(row))(rp)
        }

      // Render
      <.table(*.table,
        HeaderComponent(crs),
        <.tbody(renderRows))
    }
  }

  // ===================================================================================================================

  val HeaderComponent = ReactComponentB[NonEmptyVector[ColumnRenderer]]("Header")
    .render(crs =>
      <.thead(
        <.tr(
          crs.toStream.map(cr =>
            <.th(
              *.columnHeader(cr.column.live),
              cr.header)))))
    .configure(shouldComponentUpdate)
    .build

  // ===================================================================================================================

  implicit val rowPropReuse = Reusability.caseclass5(RowProps.unapply)

  case class RowProps(row     : Row,
                      crs     : NonEmptyVector[ColumnRenderer],
                      cells   : Cell.RowState,
                      focus   : Option[Column],
                      setFocus: Column ~=> IO[Unit])

  val RowComponent =
    ReactComponentB[RowProps]("Row")
      .render(renderRow(_))
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
  def onCellClick(setFocus: => IO[Unit]): ReactMouseEventH => UndefOr[IO[Unit]] = e =>
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
          ^.onClick ~~>? onCellClick(p setFocus col),
          p.cells.get(cr.column).fold(readOnlyView)(renderCellState))
      })

  def renderCellState: Cell.State => ReactElement = {
    case e: Cell.Editing => e.render
  }
}
