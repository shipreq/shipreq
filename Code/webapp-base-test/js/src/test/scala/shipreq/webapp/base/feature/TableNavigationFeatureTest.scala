package shipreq.webapp.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.test.ReactTestUtils
import org.scalajs.dom.html
import scalaz.\/-
import utest._
import shipreq.webapp.base.lib.DomUtil.{TableCellZipper => _, _}
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.{Backwards, Direction, Forwards}

object TableNavigationFeatureTest extends TestSuite {
  import TableNavigationFeature._

  val focusable = ^.tabIndex := -1

  /**
    * LR = Left to Right = header cells on the left, data cells on the right
    */
  object LR {
    final class Backend($: BackendScope[Unit, Unit]) {
      implicit def renderTablePos(p: TablePos): TagMod = p.toString
      def render: VdomElement =
        <.table(
          <.tbody(
            <.tr(
              <.th(TablePos(0, 0, 0, None)),
              <.td(TablePos(0, 0, 1, None), focusable),
              <.td(TablePos(0, 0, 2, None), focusable),
              <.td(TablePos(0, 0, 3, None), focusable),
            ),
            <.tr(
              <.th(TablePos(0, 1, 0, None), focusable),
              <.td(TablePos(0, 1, 1, None), focusable),
              <.td(TablePos(0, 1, 2, None)),
              <.td(TablePos(0, 1, 3, None), focusable),
            ),
            <.tr(
              <.th(TablePos(0, 2, 0, None)),
              <.td(TablePos(0, 2, 1, None), focusable),
              <.td(TablePos(0, 2, 2, None)),
            ),
            <.tr(
              <.th(TablePos(0, 3, 0, None), focusable),
            ),
            <.tr(
              <.th(TablePos(0, 4, 0, None), focusable),
              <.th(TablePos(0, 4, 1, None)),
              <.td(TablePos(0, 4, 2, None), <.input.checkbox),
              <.td(TablePos(0, 4, 3, None), <.input.text, focusable),
              <.td(TablePos(0, 4, 4, None), <.input.text),
              <.td(TablePos(0, 4, 5, None), <.input.checkbox, <.input.checkbox),
              <.td(TablePos(0, 4, 6, None), <.input.checkbox, focusable),
            ),
          )
        )
    }

    val Component = ScalaComponent.builder[Unit]("LR")
      .renderBackend[Backend]
      .build

    val leftMoves = List[List[TablePos]](
      List(
        TablePos(0, 0, 3, None),
        TablePos(0, 0, 2, None),
        TablePos(0, 0, 1, None),
        TablePos(0, 0, 3, None),
      ),
      List(
        TablePos(0, 1, 3, None),
        TablePos(0, 1, 1, None),
        TablePos(0, 1, 0, None),
        TablePos(0, 1, 3, None),
      ),
      List(
        TablePos(0, 2, 1, None),
        TablePos(0, 2, 1, None),
      ),
      List(
        TablePos(0, 3, 0, None),
        TablePos(0, 3, 0, None),
      ),
      List(
        TablePos(0, 4, 6, Some(PosXY(0, 0))),
        TablePos(0, 4, 6, None),
        TablePos(0, 4, 5, Some(PosXY(1, 0))),
        TablePos(0, 4, 5, Some(PosXY(0, 0))),
        TablePos(0, 4, 3, None),
        TablePos(0, 4, 2, Some(PosXY(0, 0))),
        TablePos(0, 4, 0, None),
      ),
    )
  }

  lazy val lr: html.Table = {
    val root = ReactTestUtils.newBodyElement()
    LR.Component().renderIntoDOM(root).getDOMNode.domCast[html.Table]
  }

  def testMoves(table: html.Table, axis: Axis, movement: Movement, moves: List[List[TablePos]], movesDir: Direction): Unit =
    moves.foreach { ms =>
      val ms2 = if (movesDir is Forwards) ms else ms.reverse
      testMoves2(table, axis, movement, ms2)
    }

  def testMoves2(table: html.Table, axis: Axis, movement: Movement, moves: List[TablePos]): Unit = {
    val z = TableCellZipper(lr.querySelectorAll("td,th").iterator.focusable.next())
    for ((from, to) <- moves zip moves.tail) {
      val z2 = z.goto(from).needRight
      assertEq(s"goto($from).focusPos", z2.focusPos, \/-(from))
      val actual = z2.move(axis, movement).flatMap(_.focusPos)
      assertEq(s"$axis $movement: $from --> $to", actual, \/-(to))
    }
  }

  override def tests = TestSuite {

    'posDetection {
      def text(e: html.Element): String =
        ReactTestUtils.removeReactInternals(e.innerHTML).replaceFirst("\\).+", ")")

      val cells = lr.querySelectorAll("td,th").iterator.focusable.toList
      assert(cells.nonEmpty)
      for (c <- cells) {
        val z = TableCellZipper(c)
        val pos = z.focusPos.needRight
        val tableRoot = z.root.needRight
        assertEq(pos.toString, text(c))
        assert(tableRoot == lr)

        val z2 = z.goto(pos).needRight
        val pos2 = z2.focusPos.needRight
        assertEq(pos2.toString, text(c))
        assertEq(pos2, pos)
      }
    }

    'moveLeft  - testMoves(lr, Axis.LeftRight, Movement.Prev, LR.leftMoves, Forwards)
    'moveRight - testMoves(lr, Axis.LeftRight, Movement.Next, LR.leftMoves, Backwards)

  }
}
