package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import org.scalajs.dom.ext.KeyCode
import scalaz.effect.IO
import scalaz.std.anyVal.intInstance
import scalaz.syntax.equal._
import shipreq.base.util.{UnivEq, Must}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.util._
import DataImplicits._

object Table {

  implicit val reuseCRs : Reusable[Vector[ColumnRenderer]] = Reusable.byRef
  implicit val reuseRows: Reusable[Vector[Row]]            = Reusable.byRef
  implicit val reuseRow : Reusable[Row]                    = Reusable.byRef
  implicit val reuseVSs : Reusable[ViewSettings]           = Reusable.byRef
  implicit val reuseCNR : Reusable[Column.NameResolver]    = Reusable.byRef

  implicit val propContent = Reusable.caseclass2(Content.unapply)
  implicit val propFocus   = Reusable.caseclass3(Focus.unapply)
  implicit val propReuse   = Reusable.caseclass3(Props.unapply)

  case class Content(crs: Vector[ColumnRenderer], rows: Vector[Row])

  case class Focus(rowInd: Int, col: Column, content: Content)

  case class Props(project: Project,
                   content: Content,
                   focus: ReusableExternalVar[Option[Focus]])

  val Component =
    ReactComponentB[Props]("Table")
      .stateless
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(KeyPressListener.install(), Reusable.preventUpdates)
      .build

  sealed trait FocusKB
  case object Up    extends FocusKB
  case object Down  extends FocusKB
  case object Left  extends FocusKB
  case object Right extends FocusKB
  case object Clear extends FocusKB

  val rowKey: Row => Int = {
    case r: GenericReqRow => r.req.id.value.toInt
  }

  final class Backend($: BackendScope[Props, Unit]) extends KeyPressListener.IO {

    override val onKeyPressIO = matchNoMods {
      case KeyCode.up     => moveFocusKB(Up)
      case KeyCode.down   => moveFocusKB(Down)
      case KeyCode.left   => moveFocusKB(Left)
      case KeyCode.right  => moveFocusKB(Right)
      case KeyCode.escape => moveFocusKB(Clear)
    }

    def moveFocusKB(kb: FocusKB): IO[Unit] = {
      val fv = $.props.focus
      fv.value.fold(IO(())){f =>

        @inline def set(nf: Focus) =fv set Some(nf)

        @inline def limit(i: Int, m: Int): Int =
          if (i < 0) m else if (i > m) 0 else i

        def shiftRow(add: Int) = set {
          val r = limit(f.rowInd + add, f.content.rows.length - 1)
          f.copy(rowInd = r)
        }

        def shiftCol(add: Int) = set {
          val cs = $.props.content.crs
          val i = cs.indexWhere(_.column ≟ f.col)
          val j = limit(i + add, cs.size - 1)
          val c = cs(j).column
          f.copy(col = c)
        }

        kb match {
          case Up    => shiftRow(-1)
          case Down  => shiftRow(1)
          case Left  => shiftCol(-1)
          case Right => shiftCol(1)
          case Clear => fv set None
        }
      }
    }

    // This is ok because $.props.focus is dereferenced INSIDE the function
    val setFocusFn = ReusableFn[Int, Column, IO[Unit]](
      (i, c) => $.props.focus.set(Some(Focus(i, c, $.props.content))))

    def render: ReactElement = {
      val p     = $.props
      val crs   = p.content.crs
      val rows  = p.content.rows
      val focus = p.focus.value

      def renderRows =
        (0 until rows.length).toReactNodeArray { i =>
          val row = rows(i)
          val curFocus = focus.filter(_.rowInd ≟ i).map(_.col)
          val p = RowProps(row, crs, curFocus, setFocusFn(i))
          RowComponent.withKey(rowKey(row))(p)
        }

      // Render
      // TODO handle zero rows nicely. "33 reqs (SHRs?), 11 deleted, 3 excluded by filter."
      <.table(*.table,
        <.thead(
          <.tr(
            crs.map(cr =>
              <.th(
                cr.columnStyle,
                cr.header)))),
        <.tbody(renderRows))
    }
  }

  // ===================================================================================================================

  implicit val rowPropReuse = Reusable.caseclass4(RowProps.unapply)

  case class RowProps(row     : Row,
                      crs     : Vector[ColumnRenderer],
                      focus   : Option[Column],
                      setFocus: Column ~=> IO[Unit])

  val RowComponent =
    ReactComponentB[RowProps]("Row")
      .render(p =>
        <.tr(
          p.crs.map(cr =>
            <.td(
              *.cell(p.focus.exists(_ ≟ cr.column)),
              ^.onClick ~~> p.setFocus(cr.column),
              cr.columnStyle,
              cr render p.row)))
      )
      .configure(Reusable.preventUpdates)
      .build
}
