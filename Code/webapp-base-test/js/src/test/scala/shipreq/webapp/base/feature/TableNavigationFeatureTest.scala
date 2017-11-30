package shipreq.webapp.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.test.ReactTestUtils
import org.scalajs.dom.html
import scalaz.{Equal, \/-}
import scalaz.syntax.traverse._
import utest._
import shipreq.webapp.base.lib.DomUtil._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.{Backwards, Direction, Forwards}
import tablenav._

object TableNavigationFeatureTest extends TestSuite {
  import Axis._
  import Movement._

  val focusable = ^.tabIndex := -1

  def MovesBuilder(): MovesBuilder = new MovesBuilder
  class MovesBuilder {
    private var moves = List.newBuilder[TablePos]
    private var subMoves = List.newBuilder[TablePos]

    private var batchMoves = List.newBuilder[List[TablePos]]
    private var batchSubMoves = List.newBuilder[List[TablePos]]

    def general(p: TablePos): this.type = {
      moves += p
      subMoves += p
      this
    }

    def movOnly(p: TablePos): this.type = {
      moves += p
      this
    }

    def subOnly(p: TablePos): this.type = {
      subMoves += p
      this
    }

    def newBatch(): this.type = {
      batchMoves += moves.result()
      batchSubMoves += subMoves.result()
      moves = List.newBuilder
      subMoves = List.newBuilder
      this
    }

    def result(): TestMoves = {
      newBatch()

      val moveTests: List[List[TablePos]] =
        batchMoves.result()

      val subTests: List[List[TablePos]] =
        batchSubMoves.result()
          .flatten
          .groupBy(_.copy(sub = None))
          .iterator
          .map(_._2)
          .map {
            case a :: b :: Nil     if a ==* b      => a :: Nil
            case h :: (t@(_ :: _)) if h ==* t.last => t
            case x                                 => x
          }
          .toList

      TestMoves(moveTests, subTests)
    }
  }

  case class TestMoves(moveTests: List[List[TablePos]], subTests: List[List[TablePos]]) {
    def ++(t: TestMoves): TestMoves =
      TestMoves(moveTests ::: t.moveTests, subTests ::: t.subTests)
    def reverse: TestMoves =
      TestMoves(moveTests.map(_.reverse), subTests.map(_.reverse))
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

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
              <.th(TablePos(0, 4, 0, None), focusable, <.span(<.span)),
              <.th(TablePos(0, 4, 1, None)),
              <.td(TablePos(0, 4, 2, None), <.input.checkbox),
              <.td(TablePos(0, 4, 3, None), <.input.text, focusable),
              <.td(TablePos(0, 4, 4, None), <.input.text),
              <.td(TablePos(0, 4, 5, None), <.input.checkbox, <.input.checkbox),
              <.td(TablePos(0, 4, 6, None), focusable, <.input.checkbox),  // ignore cell focusability cos of sub-movable
              <.td(TablePos(0, 4, 7, None), focusable, <.span(<.input.checkbox), <.input.checkbox), // ignore cell focusability cos of sub-movables
            ),
            <.tr(
              <.td(TablePos(0, 5, 0, None), <.div(focusable), <.div, <.div(focusable)), // ReqDetail implications --
              <.td(TablePos(0, 5, 1, None), <.input.text,     <.div, <.div(focusable)), // ReqDetail implications *-
              <.td(TablePos(0, 5, 2, None), <.div(focusable), <.div, <.input.text),     // ReqDetail implications -*
              <.td(TablePos(0, 5, 3, None), <.textarea,       <.div, <.textarea),       // ReqDetail implications **
            ),
            <.tr(
              <.td(TablePos(0, 6, 0, None), focusable, <.div(focusable), <.div, <.div(focusable)), // ignore cell focusability cos of sub-movable
              <.td(TablePos(0, 6, 1, None), focusable, <.div(focusable), <.div, <.div(focusable)), // ignore cell focusability cos of sub-movable
              <.td(TablePos(0, 6, 2, None), <.table(TableNavigationFeature.nestedTable, <.tbody(<.tr(<.td(<.div(focusable)), <.td(<.div(focusable)))))), // nested table
            ),
          )
        )
    }

    val Component = ScalaComponent.builder[Unit]("LR")
      .renderBackend[Backend]
      .build

    val rightMoves = MovesBuilder()
      .general(TablePos(0, 0, 3, None))
      .general(TablePos(0, 0, 1, None))
      .general(TablePos(0, 0, 2, None))
      .general(TablePos(0, 0, 3, None))
      .newBatch()
      .general(TablePos(0, 1, 0, None))
      .general(TablePos(0, 1, 1, None))
      .general(TablePos(0, 1, 3, None))
      .general(TablePos(0, 1, 0, None))
      .newBatch()
      .general(TablePos(0, 2, 1, None))
      .general(TablePos(0, 2, 1, None))
      .newBatch()
      .general(TablePos(0, 3, 0, None))
      .general(TablePos(0, 3, 0, None))
      .newBatch()
      .general(TablePos(0, 4, 0, None))
      .general(TablePos(0, 4, 2, Some(PosXY(0, 0))))
      .general(TablePos(0, 4, 3, None))
      .subOnly(TablePos(0, 4, 3, Some(PosXY(0, 0))))
      .subOnly(TablePos(0, 4, 4, Some(PosXY(0, 0))))
      .general(TablePos(0, 4, 5, Some(PosXY(0, 0))))
      .general(TablePos(0, 4, 5, Some(PosXY(1, 0))))
//      .general(TablePos(0, 4, 6, None)) // no, cos it has sub-movables
      .general(TablePos(0, 4, 6, Some(PosXY(0, 0))))
//      .general(TablePos(0, 4, 7, None)) // no, cos it has sub-movables
      .general(TablePos(0, 4, 7, Some(PosXY(0, 0))))
      .general(TablePos(0, 4, 7, Some(PosXY(1, 0))))
      .general(TablePos(0, 4, 0, None))
      .newBatch()
      .general(TablePos(0, 5, 0, Some(PosXY(0, 0))))
      .general(TablePos(0, 5, 0, Some(PosXY(1, 0))))
      .subOnly(TablePos(0, 5, 1, Some(PosXY(0, 0))))
      .general(TablePos(0, 5, 1, Some(PosXY(1, 0))))
      .general(TablePos(0, 5, 2, Some(PosXY(0, 0))))
      .subOnly(TablePos(0, 5, 2, Some(PosXY(1, 0))))
      .subOnly(TablePos(0, 5, 3, Some(PosXY(0, 0))))
      .subOnly(TablePos(0, 5, 3, Some(PosXY(1, 0))))
      .general(TablePos(0, 5, 0, Some(PosXY(0, 0))))
      .result()

    private val downShared = MovesBuilder()
      .general(TablePos(0, 0, 1, None))
      .general(TablePos(0, 1, 1, None))
      .general(TablePos(0, 2, 1, None))
      .newBatch()
      .general(TablePos(0, 5, 2, Some(PosXY(0, 0))))
      .general(TablePos(0, 6, 2, Some(PosXY(0, 0))))
      .result()

    val downMoves = downShared ++ MovesBuilder()
      .general(TablePos(0, 1, 0, None))
      .general(TablePos(0, 2, 1, None)) // 0 not available
      .general(TablePos(0, 3, 0, None)) // 1 not available
      .general(TablePos(0, 4, 0, None))
      .general(TablePos(0, 5, 0, Some(PosXY(0, 0))))
      .general(TablePos(0, 6, 0, Some(PosXY(0, 0))))
      .general(TablePos(0, 0, 1, None))
      .general(TablePos(0, 1, 1, None))
      .result()

    val upMoves = downShared.reverse ++ MovesBuilder()
      .general(TablePos(0, 1, 1, None))
      .general(TablePos(0, 0, 1, None))
      .general(TablePos(0, 6, 1, Some(PosXY(0, 0)))) // not PosXY cos it has sub-movables
      .general(TablePos(0, 5, 1, Some(PosXY(1, 0))))
      .general(TablePos(0, 4, 0, None)) // or .general(TablePos(0, 4, 2, Some(PosXY(0, 0))))
      .general(TablePos(0, 3, 0, None))
      .general(TablePos(0, 2, 1, None)) // 0 not available
      .result()
  }

  /**
    * TD = TopDown = header row at the top, data below
    */
  object TD {
    final class Backend($: BackendScope[Unit, Unit]) {
      val subRow = <.div(focusable, TableNavigationFeature.newRow)
      implicit def renderTablePos(p: TablePos): TagMod = p.toString
      def render: VdomElement =
        <.table(
          <.thead(
            <.tr(
              <.th(TablePos(0, 0, 0, None), <.input.checkbox),
              <.th(TablePos(0, 0, 1, None), focusable),
              <.th(TablePos(0, 0, 2, None), focusable),
              <.th(TablePos(0, 0, 3, None)),
              <.th(TablePos(0, 0, 4, None), focusable),
              <.th(TablePos(0, 0, 5, None), focusable),
            ),
          ),
          <.tbody(
            <.tr(
              <.td(TablePos(1, 0, 0, None), <.input.checkbox),
              <.td(TablePos(1, 0, 1, None), focusable),
              <.td(TablePos(1, 0, 2, None), focusable),
              <.td(TablePos(1, 0, 3, None), focusable),
              <.td(TablePos(1, 0, 4, None)),
              <.td(TablePos(1, 0, 5, None), focusable, subRow, subRow, subRow), // 0x[0,2]
            ),
            <.tr(
              <.td(TablePos(1, 1, 0, None), <.input.checkbox),
              <.td(TablePos(1, 1, 1, None), focusable),
              <.td(TablePos(1, 1, 2, None), focusable),
              <.td(TablePos(1, 1, 3, None), focusable),
              <.td(TablePos(1, 1, 4, None)),
              <.td(TablePos(1, 1, 5, None), focusable, subRow, <.div(focusable), subRow, <.div(focusable)), // [01]x[01]
            ),
          )
        )
    }

    val Component = ScalaComponent.builder[Unit]("TD")
      .renderBackend[Backend]
      .build

    val rightMoves = MovesBuilder()
      .general(TablePos(0, 0, 0, Some(PosXY(0, 0))))
      .general(TablePos(0, 0, 1, None))
      .general(TablePos(0, 0, 2, None))
      .general(TablePos(0, 0, 4, None))
      .general(TablePos(0, 0, 5, None))
      .general(TablePos(0, 0, 0, Some(PosXY(0, 0))))
      .newBatch()
      .general(TablePos(1, 0, 0, Some(PosXY(0, 0))))
      .general(TablePos(1, 0, 1, None))
      .general(TablePos(1, 0, 2, None))
      .general(TablePos(1, 0, 3, None))
      .movOnly(TablePos(1, 0, 5, Some(PosXY(0, 0))))
      .general(TablePos(1, 0, 0, Some(PosXY(0, 0))))
      .newBatch()
      .general(TablePos(1, 1, 0, Some(PosXY(0, 0))))
      .general(TablePos(1, 1, 1, None))
      .general(TablePos(1, 1, 2, None))
      .general(TablePos(1, 1, 3, None))
      .movOnly(TablePos(1, 1, 5, Some(PosXY(0, 0))))
      .movOnly(TablePos(1, 1, 5, Some(PosXY(1, 0))))
      .newBatch()
      .movOnly(TablePos(1, 1, 5, Some(PosXY(0, 1))))
      .movOnly(TablePos(1, 1, 5, Some(PosXY(1, 1))))
      .result()

    val downMoves = MovesBuilder()
      .general(TablePos(0, 0, 0, Some(PosXY(0, 0))))
      .general(TablePos(1, 0, 0, Some(PosXY(0, 0))))
      .general(TablePos(1, 1, 0, Some(PosXY(0, 0))))
      .general(TablePos(0, 0, 0, Some(PosXY(0, 0))))
      .newBatch()
      .general(TablePos(0, 0, 1, None))
      .general(TablePos(1, 0, 1, None))
      .general(TablePos(1, 1, 1, None))
      .general(TablePos(0, 0, 1, None))
      .newBatch()
      .general(TablePos(1, 0, 5, Some(PosXY(0, 0))))
      .general(TablePos(1, 0, 5, Some(PosXY(0, 1))))
      .general(TablePos(1, 0, 5, Some(PosXY(0, 2))))
      .general(TablePos(1, 1, 5, Some(PosXY(0, 0))))
      .general(TablePos(1, 1, 5, Some(PosXY(0, 1))))
      .general(TablePos(0, 0, 5, None))
      .general(TablePos(1, 0, 5, Some(PosXY(0, 0))))
      .newBatch()
      .general(TablePos(1, 1, 5, Some(PosXY(1, 0))))
      .general(TablePos(1, 1, 5, Some(PosXY(1, 1))))
      .result()
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  lazy val lr: html.Table = {
    val root = ReactTestUtils.newBodyElement()
    LR.Component().renderIntoDOM(root).getDOMNode.domCast[html.Table]
  }

  lazy val td: html.Table = {
    val root = ReactTestUtils.newBodyElement()
    TD.Component().renderIntoDOM(root).getDOMNode.domCast[html.Table]
  }

  val changeDir: Direction => List[TablePos] => List[TablePos] = {
    case Forwards  => identity
    case Backwards => _.reverse
  }

  def testCellLabels(table: html.Table): Unit = {
    def text(e: html.Element): String =
      ReactTestUtils.removeReactInternals(e.innerHTML).replaceFirst("\\).+", ")")

    val cells = table.querySelectorAll("td,th").iterator.focusable.toList
    assert(cells.nonEmpty)
    for (c <- cells) {
      val z = TableCellZipper(c)
      val pos = z.focusPos.needRight
      val tableRoot = z.root.needRight
      assertEq(pos.toString, text(c))
      assert(tableRoot == table)

      val z2 = z.goto(pos).needRight
      val pos2 = z2.focusPos.needRight
      assertEq(pos2.toString, text(c))
      assertEq(pos2, pos)
    }
  }

  def init(table: html.Table): TableCellZipper =
    TableCellZipper(table.querySelectorAll("td,th").iterator.focusable.next())

  def needGoto(z: TableCellZipper, pos: TablePos): TableCellZipper = {
    val z2 = z.goto(pos).needRight
    assertEq(s"goto($pos).focusPos", z2.focusPos, \/-(pos))
    z2
  }

  def testMoves(table: html.Table, axis: Axis, movement: Movement, moves: TestMoves, movesDir: Direction): Unit = {
    val z = init(table)
    moves.moveTests.foreach { ms =>
      testMoves2(z, axis, movement, changeDir(movesDir)(ms))
    }
  }

  def testMoves2(z: TableCellZipper, axis: Axis, movement: Movement, moves: List[TablePos]): Unit =
    for ((from, to) <- moves zip moves.tail)
      testMove(z, axis, movement, from, to)

  def testMove(z: TableCellZipper, axis: Axis, movement: Movement, from: TablePos, to: TablePos): Unit = {
    val z2 = needGoto(z, from)
    val actual = z2.move(axis, movement).flatMap(_.focusPos)
    assertEq(s"$axis $movement: $from --> $to", actual, \/-(to))
  }

  def testSubMoves(table: html.Table, movement: Movement, moves: TestMoves, movesDir: Direction): Unit = {
    val z = init(table)

    def testData: Iterator[List[TablePos]] =
      moves.subTests.iterator.map(changeDir(movesDir))

    for (ps <- testData)
      if (ps.tail.isEmpty) {
        // Test movement returns None
        val pos = ps.head
        val z2 = needGoto(z, pos)
        val actual = z2.subMove(movement).map(_.void)
        assertEq(s"subMove $movement: $pos --> None", actual, \/-(None))
      } else
        // Test movement succeeds
        testSubMoves2(z, movement, ps.last :: ps)
  }

  def testSubMoves2(z: TableCellZipper, movement: Movement, moves: List[TablePos]): Unit =
    for ((from, to) <- moves zip moves.tail) {
      val z2 = needGoto(z, from)
      val actual = z2.subMove(movement).flatMap(_.traverse(_.focusPos))
      assertEq(s"subMove $movement: $from --> $to", actual, \/-(Some(to)))
    }

  override def tests = TestSuite {

    'lr {
      def t = lr
      def T = LR
      'posDetection   - testCellLabels(t)
      'moveRight      - testMoves(t, LeftRight, Next, T.rightMoves, Forwards)
      'moveLeft       - testMoves(t, LeftRight, Prev, T.rightMoves, Backwards)
      'moveDown       - testMoves(t, UpDown   , Next, T.downMoves , Forwards)
      'moveUp         - testMoves(t, UpDown   , Prev, T.upMoves   , Forwards)
      'subMoveLeft    - testSubMoves(t, Prev, T.rightMoves, Backwards)
      'subMoveRight   - testSubMoves(t, Next, T.rightMoves, Forwards)
    }

    'td {
      def t = td
      def T = TD
      'posDetection   - testCellLabels(t)
      'moveRight      - testMoves(t, LeftRight, Next, T.rightMoves, Forwards)
      'moveLeft       - testMoves(t, LeftRight, Prev, T.rightMoves, Backwards)
      'moveDown       - testMoves(t, UpDown   , Next, T.downMoves , Forwards)
      'moveUp         - testMoves(t, UpDown   , Prev, T.downMoves , Backwards)
      'subMoveLeft    - testSubMoves(t, Prev, T.rightMoves, Backwards)
      'subMoveRight   - testSubMoves(t, Next, T.rightMoves, Forwards)

      'fromOuter {
        val z = init(t)
        'up     - testMove(z, UpDown, Prev, TablePos(1, 1, 5, None), TablePos(1, 0, 5, Some(PosXY(0, 2))))
        'down   - testMove(z, UpDown, Next, TablePos(1, 1, 5, None), TablePos(1, 1, 5, Some(PosXY(0, 0))))
        'top    - testMove(z, UpDown, Head, TablePos(1, 1, 5, None), TablePos(0, 0, 5, None))
        'bottom - testMove(z, UpDown, Last, TablePos(1, 1, 5, None), TablePos(1, 1, 5, Some(PosXY(0, 1))))
      }
    }

  }
}
