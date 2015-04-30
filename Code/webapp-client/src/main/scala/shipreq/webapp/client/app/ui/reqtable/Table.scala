package shipreq.webapp.client.app.ui.reqtable

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import scala.scalajs.js
import scalaz.effect.IO
import scalaz.std.anyVal.intInstance
import scalaz.syntax.equal._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.effect.IoUtils.{nop, IoExt}
import shipreq.base.util.NonEmptyVector
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.util._

object Table {

  implicit val reuseCEs : Reusable[ColumnEditors]                  = Reusable.byRef
  implicit val reuseCRs : Reusable[NonEmptyVector[ColumnRenderer]] = Reusable.byRef
  implicit val reuseRows: Reusable[Vector[Row]]                    = Reusable.byRef
  implicit val reuseRow : Reusable[Row]                            = Reusable.byRef
  implicit val reuseVSs : Reusable[ViewSettings]                   = Reusable.byRef
  implicit val reuseCNR : Reusable[Column.NameResolver]            = Reusable.byRef
  implicit val reuseCTS : Reusable[Cell.TableState]                = Reusable.byRef
  implicit val reuseCRS : Reusable[Cell.RowState]                  = Reusable.byRef

  implicit val propContent = Reusable.caseclass3(Content.unapply)
  implicit val propFocus   = Reusable.caseclass3(Focus.unapply)
  implicit val propReuse   = Reusable.caseclass4(Props.unapply)

  case class Content(crs: NonEmptyVector[ColumnRenderer], rows: Vector[Row], ces: ColumnEditors)

  case class Focus(rowInd: Int, col: Column, content: Content) {
    @inline def row(rows: Vector[Row]): Option[Row] =
      try { Some(rows(rowInd)) } catch { case _: Throwable => None }
  }

  case class Props(project: Project,
                   content: Content,
                   cells  : Cell.TableState,
                   focus  : ReusableExternalVar[Option[Focus]])

  val Component =
    ReactComponentB[Props]("Table")
      .stateless
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(KeyPressListener.install(), Reusable.preventUpdates)
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
        val r = limit(f.rowInd + add, f.content.rows.length - 1)
        f.copy(rowInd = r)
      }

      def focusShiftCol(add: Int) = focusMod { f =>
        val cs = $.props.content.crs.whole
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
        case EditStart  => withFocusO(f => f.row(p.content.rows) flatMap (startCellEditing(_, f.col)))
      }
    }

    def startCellEditing(row: Row, col: Column): Option[IO[Unit]] = {
      val p = $.props
      val rowState = p.cells(row.id)
      if (rowState.get(col).isDefined)
        // Already has cell state
        None
      else
        p.content.ces.startCellEditing(row, col) //.map(_ >>> p.focus.set(None))
    }

    // This is ok because $.props.focus is dereferenced INSIDE the function
    val setFocusFn = ReusableFn[Int, Column, IO[Unit]](
      (i, c) => $.props.focus.set(Some(Focus(i, c, $.props.content))))

    def render: ReactElement = {
      val p     = $.props
      val crs   = p.content.crs
      val rows  = p.content.rows
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
      // TODO handle zero rows nicely. "33 reqs (SHRs?), 11 deleted, 3 excluded by filter."
      <.table(*.table,
        <.thead(
          <.tr(
            crs.toStream.map(cr =>
              <.th(
                cr.columnStyle,
                cr.header)))),
        <.tbody(renderRows))
    }
  }

  // ===================================================================================================================

  implicit val rowPropReuse = Reusable.caseclass5(RowProps.unapply)

  case class RowProps(row     : Row,
                      crs     : NonEmptyVector[ColumnRenderer],
                      cells   : Cell.RowState,
                      focus   : Option[Column],
                      setFocus: Column ~=> IO[Unit])

  val RowComponent =
    ReactComponentB[RowProps]("Row")
      .render(p =>
        <.tr(
          p.crs.toStream.map { cr =>
            val col = cr.column
            <.td(
              *.cell(p.focus.exists(_ ≟ col)),
              ^.onClick ~~> p.setFocus(col),
              cr.columnStyle,
              renderCell(p.row, cr, p.cells))
          }
        )
      )
      .configure(Reusable.preventUpdates)
      .build

  @inline def renderCell(row: Row, cr: ColumnRenderer, cells: Cell.RowState) = {
    def readOnly: ReactElement =
      cr render row

    def stateful: Cell.State => ReactElement = {
      case e: Cell.Editing => e.render
    }

    cells.get(cr.column).fold(readOnly)(stateful)
  }
}
