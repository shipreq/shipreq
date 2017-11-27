package shipreq.webapp.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.test.ReactTestUtils
import org.scalajs.dom.html
import scalaz.\/-
import scalaz.syntax.traverse._
import utest._
import shipreq.webapp.base.lib.DomUtil.{TableCellZipper => _, _}
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.{Backwards, Direction, Forwards, Memo}

object TableNavigationFeatureTest extends TestSuite {
  import TableNavigationFeature._

  val focusable = ^.tabIndex := -1

  def MovesBuilder() = new MovesBuilder
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

  case class TestMoves(moveTests: List[List[TablePos]], subTests: List[List[TablePos]])

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
              <.td(TablePos(0, 4, 7, None), <.span(<.input.checkbox), <.input.checkbox, focusable),
            ),
            <.tr(
              <.td(TablePos(0, 5, 0, None), <.div(focusable), <.div, <.div(focusable)), // ReqDetail implications --
              <.td(TablePos(0, 5, 1, None), <.input.text,     <.div, <.div(focusable)), // ReqDetail implications *-
              <.td(TablePos(0, 5, 2, None), <.div(focusable), <.div, <.input.text),     // ReqDetail implications -*
              <.td(TablePos(0, 5, 3, None), <.textarea,       <.div, <.textarea),       // ReqDetail implications **
              // TODO what about buttons?
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
      .general(TablePos(0, 4, 6, None))
      .general(TablePos(0, 4, 6, Some(PosXY(0, 0))))
      .general(TablePos(0, 4, 7, None))
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

    val downMoves = MovesBuilder()
      .general(TablePos(0, 0, 1, None))
      .general(TablePos(0, 1, 1, None))
      .general(TablePos(0, 2, 1, None))
      .newBatch()
      .general(TablePos(0, 1, 0, None))
      .general(TablePos(0, 2, 1, None)) // 0 not available
      .general(TablePos(0, 3, 0, None)) // 1 not available
      .general(TablePos(0, 4, 0, None))
      .general(TablePos(0, 5, 0, Some(PosXY(0, 0))))
      .general(TablePos(0, 0, 1, None))
      .general(TablePos(0, 1, 1, None))
      .result()
  }

  lazy val lr: html.Table = {
    val root = ReactTestUtils.newBodyElement()
    LR.Component().renderIntoDOM(root).getDOMNode.domCast[html.Table]
  }

  val changeDir: Direction => List[TablePos] => List[TablePos] = {
    case Forwards  => identity
    case Backwards => _.reverse
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
    for ((from, to) <- moves zip moves.tail) {
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

    'moveLeft  - testMoves(lr, Axis.LeftRight, Movement.Prev, LR.rightMoves, Backwards)
    'moveRight - testMoves(lr, Axis.LeftRight, Movement.Next, LR.rightMoves, Forwards)
//    'moveUp    - testMoves(lr, Axis.UpDown   , Movement.Prev, LR.downMoves , Backwards)
    'moveDown  - testMoves(lr, Axis.UpDown   , Movement.Next, LR.downMoves , Forwards)

    'subMoveLeft  - testSubMoves(lr, Movement.Prev, LR.rightMoves, Backwards)
    'subMoveRight - testSubMoves(lr, Movement.Next, LR.rightMoves, Forwards)
  }
}
