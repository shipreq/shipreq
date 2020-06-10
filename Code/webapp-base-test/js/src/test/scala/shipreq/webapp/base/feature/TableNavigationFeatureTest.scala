package shipreq.webapp.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.test.ReactTestUtils
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalaz.\/-
import scalaz.syntax.traverse._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.{Backwards, Direction, Forwards}
import shipreq.webapp.base.feature.tablenav._
import shipreq.webapp.base.lib.DomUtil._
import sourcecode.Line
import utest._

object TableNavigationFeatureTest extends TestSuite {
  import Axis._
  import Movement._

  val focusable = ^.tabIndex := -1

  def MovesBuilder(): MovesBuilder = new MovesBuilder
  class MovesBuilder {
    private var moves = List.newBuilder[VirtualLoc]
    private var subMoves = List.newBuilder[VirtualLoc]

    private val batchMoves = List.newBuilder[List[VirtualLoc]]
    private val batchSubMoves = List.newBuilder[List[VirtualLoc]]

    def general(p: VirtualLoc): this.type = {
      moves += p
      subMoves += p
      this
    }

    def movOnly(p: VirtualLoc): this.type = {
      moves += p
      this
    }

    def subOnly(p: VirtualLoc): this.type = {
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

      val moveTests: List[List[VirtualLoc]] =
        batchMoves.result()

      val subTests: List[List[VirtualLoc]] =
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

  case class TestMoves(moveTests: List[List[VirtualLoc]], subTests: List[List[VirtualLoc]]) {
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
      implicit def renderVirtualLoc(p: VirtualLoc): TagMod = p.toString
      def render: VdomElement =
        <.table(
          <.tbody(
            <.tr(
              <.th(VirtualLoc(0, 0, 0, None)),
              <.td(VirtualLoc(0, 0, 1, None), focusable),
              <.td(VirtualLoc(0, 0, 2, None), focusable),
              <.td(VirtualLoc(0, 0, 3, None), focusable),
            ),
            <.tr(
              <.th(VirtualLoc(0, 1, 0, None), focusable),
              <.td(VirtualLoc(0, 1, 1, None), focusable),
              <.td(VirtualLoc(0, 1, 2, None)),
              <.td(VirtualLoc(0, 1, 3, None), focusable),
            ),
            <.tr(
              <.th(VirtualLoc(0, 2, 0, None)),
              <.td(VirtualLoc(0, 2, 1, None), focusable),
              <.td(VirtualLoc(0, 2, 2, None)),
            ),
            <.tr(
              <.th(VirtualLoc(0, 3, 0, None), focusable),
            ),
            <.tr(
              <.th(VirtualLoc(0, 4, 0, None), focusable, <.span(<.span)),
              <.th(VirtualLoc(0, 4, 1, None)),
              <.td(VirtualLoc(0, 4, 2, None), <.input.checkbox),
              <.td(VirtualLoc(0, 4, 3, None), <.input.text, focusable),
              <.td(VirtualLoc(0, 4, 4, None), <.input.text),
              <.td(VirtualLoc(0, 4, 5, None), <.input.checkbox, <.input.checkbox),
              <.td(VirtualLoc(0, 4, 6, None), focusable, <.input.checkbox),  // ignore cell focusability cos of sub-movable
              <.td(VirtualLoc(0, 4, 7, None), focusable, <.span(<.input.checkbox), <.input.checkbox), // ignore cell focusability cos of sub-movables
            ),
            <.tr(
              <.td(VirtualLoc(0, 5, 0, None), <.div(focusable), <.div, <.div(focusable)), // ReqDetail implications --
              <.td(VirtualLoc(0, 5, 1, None), <.input.text,     <.div, <.div(focusable)), // ReqDetail implications *-
              <.td(VirtualLoc(0, 5, 2, None), <.div(focusable), <.div, <.input.text),     // ReqDetail implications -*
              <.td(VirtualLoc(0, 5, 3, None), <.textarea,       <.div, <.textarea),       // ReqDetail implications **
            ),
            <.tr(
              <.td(VirtualLoc(0, 6, 0, None), focusable, <.div(focusable), <.div, <.div(focusable)), // ignore cell focusability cos of sub-movable
              <.td(VirtualLoc(0, 6, 1, None), focusable, <.div(focusable), <.div, <.div(focusable)), // ignore cell focusability cos of sub-movable
              <.td(VirtualLoc(0, 6, 2, None), <.table(TableNavigationFeature.nestedTable, <.tbody(<.tr(<.td(<.div(focusable)), <.td(<.div(focusable)))))), // nested table
            ),
          )
        )
    }

    val Component = ScalaComponent.builder[Unit]
      .renderBackend[Backend]
      .build

    val rightMoves = MovesBuilder()
      .general(VirtualLoc(0, 0, 3, None))
      .general(VirtualLoc(0, 0, 1, None))
      .general(VirtualLoc(0, 0, 2, None))
      .general(VirtualLoc(0, 0, 3, None))
      .newBatch()
      .general(VirtualLoc(0, 1, 0, None))
      .general(VirtualLoc(0, 1, 1, None))
      .general(VirtualLoc(0, 1, 3, None))
      .general(VirtualLoc(0, 1, 0, None))
      .newBatch()
      .general(VirtualLoc(0, 2, 1, None))
      .general(VirtualLoc(0, 2, 1, None))
      .newBatch()
      .general(VirtualLoc(0, 3, 0, None))
      .general(VirtualLoc(0, 3, 0, None))
      .newBatch()
      .general(VirtualLoc(0, 4, 0, None))
      .general(VirtualLoc(0, 4, 2, Some(PosXY(0, 0))))
      .general(VirtualLoc(0, 4, 3, None))
      .subOnly(VirtualLoc(0, 4, 3, Some(PosXY(0, 0))))
      .subOnly(VirtualLoc(0, 4, 4, Some(PosXY(0, 0))))
      .general(VirtualLoc(0, 4, 5, Some(PosXY(0, 0))))
      .general(VirtualLoc(0, 4, 5, Some(PosXY(1, 0))))
//      .general(VirtualLoc(0, 4, 6, None)) // no, cos it has sub-movables
      .general(VirtualLoc(0, 4, 6, Some(PosXY(0, 0))))
//      .general(VirtualLoc(0, 4, 7, None)) // no, cos it has sub-movables
      .general(VirtualLoc(0, 4, 7, Some(PosXY(0, 0))))
      .general(VirtualLoc(0, 4, 7, Some(PosXY(1, 0))))
      .general(VirtualLoc(0, 4, 0, None))
      .newBatch()
      .general(VirtualLoc(0, 5, 0, Some(PosXY(0, 0))))
      .general(VirtualLoc(0, 5, 0, Some(PosXY(1, 0))))
      .subOnly(VirtualLoc(0, 5, 1, Some(PosXY(0, 0))))
      .general(VirtualLoc(0, 5, 1, Some(PosXY(1, 0))))
      .general(VirtualLoc(0, 5, 2, Some(PosXY(0, 0))))
      .subOnly(VirtualLoc(0, 5, 2, Some(PosXY(1, 0))))
      .subOnly(VirtualLoc(0, 5, 3, Some(PosXY(0, 0))))
      .subOnly(VirtualLoc(0, 5, 3, Some(PosXY(1, 0))))
      .general(VirtualLoc(0, 5, 0, Some(PosXY(0, 0))))
      .result()

    private val downShared = MovesBuilder()
      .general(VirtualLoc(0, 0, 1, None))
      .general(VirtualLoc(0, 1, 1, None))
      .general(VirtualLoc(0, 2, 1, None))
      .newBatch()
      .general(VirtualLoc(0, 5, 2, Some(PosXY(0, 0))))
      .general(VirtualLoc(0, 6, 2, Some(PosXY(0, 0))))
      .result()

    val downMoves = downShared ++ MovesBuilder()
      .general(VirtualLoc(0, 1, 0, None))
      .general(VirtualLoc(0, 2, 1, None)) // 0 not available
      .general(VirtualLoc(0, 3, 0, None)) // 1 not available
      .general(VirtualLoc(0, 4, 0, None))
      .general(VirtualLoc(0, 5, 0, Some(PosXY(0, 0))))
      .general(VirtualLoc(0, 6, 0, Some(PosXY(0, 0))))
      .general(VirtualLoc(0, 0, 1, None))
      .general(VirtualLoc(0, 1, 1, None))
      .result()

    val upMoves = downShared.reverse ++ MovesBuilder()
      .general(VirtualLoc(0, 1, 1, None))
      .general(VirtualLoc(0, 0, 1, None))
      .general(VirtualLoc(0, 6, 1, Some(PosXY(0, 0)))) // not PosXY cos it has sub-movables
      .general(VirtualLoc(0, 5, 1, Some(PosXY(1, 0))))
      .general(VirtualLoc(0, 4, 0, None)) // or .general(VirtualLoc(0, 4, 2, Some(PosXY(0, 0))))
      .general(VirtualLoc(0, 3, 0, None))
      .general(VirtualLoc(0, 2, 1, None)) // 0 not available
      .result()
  }

  /**
    * TD = TopDown = header row at the top, data below
    */
  object TD {
    final class Backend($: BackendScope[Unit, Unit]) {
      val subRow = <.div(focusable, TableNavigationFeature.newRow)
      implicit def renderVirtualLoc(p: VirtualLoc): TagMod = p.toString
      def render: VdomElement =
        <.table(
          <.thead(
            <.tr(
              <.th(VirtualLoc(0, 0, 0, None), <.input.checkbox),
              <.th(VirtualLoc(0, 0, 1, None), focusable),
              <.th(VirtualLoc(0, 0, 2, None), focusable),
              <.th(VirtualLoc(0, 0, 3, None)),
              <.th(VirtualLoc(0, 0, 4, None), focusable),
              <.th(VirtualLoc(0, 0, 5, None), focusable),
            ),
          ),
          <.tbody(
            <.tr(
              <.td(VirtualLoc(1, 0, 0, None), <.input.checkbox),
              <.td(VirtualLoc(1, 0, 1, None), focusable),
              <.td(VirtualLoc(1, 0, 2, None), focusable),
              <.td(VirtualLoc(1, 0, 3, None), focusable),
              <.td(VirtualLoc(1, 0, 4, None)),
              <.td(VirtualLoc(1, 0, 5, None), focusable, subRow, subRow, subRow), // 0x[0,2]
            ),
            <.tr(
              <.td(VirtualLoc(1, 1, 0, None), <.input.checkbox),
              <.td(VirtualLoc(1, 1, 1, None), focusable),
              <.td(VirtualLoc(1, 1, 2, None), focusable),
              <.td(VirtualLoc(1, 1, 3, None), focusable),
              <.td(VirtualLoc(1, 1, 4, None)),
              <.td(VirtualLoc(1, 1, 5, None), focusable, subRow, <.div(focusable), subRow, <.div(focusable)), // [01]x[01]
            ),
          )
        )
    }

    val Component = ScalaComponent.builder[Unit]
      .renderBackend[Backend]
      .build

    val rightMoves = MovesBuilder()
      .general(VirtualLoc(0, 0, 0, Some(PosXY(0, 0))))
      .general(VirtualLoc(0, 0, 1, None))
      .general(VirtualLoc(0, 0, 2, None))
      .general(VirtualLoc(0, 0, 4, None))
      .general(VirtualLoc(0, 0, 5, None))
      .general(VirtualLoc(0, 0, 0, Some(PosXY(0, 0))))
      .newBatch()
      .general(VirtualLoc(1, 0, 0, Some(PosXY(0, 0))))
      .general(VirtualLoc(1, 0, 1, None))
      .general(VirtualLoc(1, 0, 2, None))
      .general(VirtualLoc(1, 0, 3, None))
      .movOnly(VirtualLoc(1, 0, 5, Some(PosXY(0, 0))))
      .general(VirtualLoc(1, 0, 0, Some(PosXY(0, 0))))
      .newBatch()
      .general(VirtualLoc(1, 1, 0, Some(PosXY(0, 0))))
      .general(VirtualLoc(1, 1, 1, None))
      .general(VirtualLoc(1, 1, 2, None))
      .general(VirtualLoc(1, 1, 3, None))
      .movOnly(VirtualLoc(1, 1, 5, Some(PosXY(0, 0))))
      .movOnly(VirtualLoc(1, 1, 5, Some(PosXY(1, 0))))
      .newBatch()
      .movOnly(VirtualLoc(1, 1, 5, Some(PosXY(0, 1))))
      .movOnly(VirtualLoc(1, 1, 5, Some(PosXY(1, 1))))
      .result()

    val downMoves = MovesBuilder()
      .general(VirtualLoc(0, 0, 0, Some(PosXY(0, 0))))
      .general(VirtualLoc(1, 0, 0, Some(PosXY(0, 0))))
      .general(VirtualLoc(1, 1, 0, Some(PosXY(0, 0))))
      .general(VirtualLoc(0, 0, 0, Some(PosXY(0, 0))))
      .newBatch()
      .general(VirtualLoc(0, 0, 1, None))
      .general(VirtualLoc(1, 0, 1, None))
      .general(VirtualLoc(1, 1, 1, None))
      .general(VirtualLoc(0, 0, 1, None))
      .newBatch()
      .general(VirtualLoc(1, 0, 5, Some(PosXY(0, 0))))
      .general(VirtualLoc(1, 0, 5, Some(PosXY(0, 1))))
      .general(VirtualLoc(1, 0, 5, Some(PosXY(0, 2))))
      .general(VirtualLoc(1, 1, 5, Some(PosXY(0, 0))))
      .general(VirtualLoc(1, 1, 5, Some(PosXY(0, 1))))
      .general(VirtualLoc(0, 0, 5, None))
      .general(VirtualLoc(1, 0, 5, Some(PosXY(0, 0))))
      .newBatch()
      .general(VirtualLoc(1, 1, 5, Some(PosXY(1, 0))))
      .general(VirtualLoc(1, 1, 5, Some(PosXY(1, 1))))
      .result()
  }

  /** {{{
    * +--+--+
    * |▛▜|  |0:21
    * +██+--+
    * |██|  |1:-1
    * +--+--+
    * |▛▜|▛▜|2:32
    * +██+██+
    * |██|██|3:--
    * +██+--+
    * |██|▛▜|4:-2
    * +--+██+
    * |  |██|5:1-
    * +--+--+
    * |  |▛▜|6:-3
    * +--+██+
    * |▛▜|██|7:2-
    * +██+██+
    * |██|██|8:--
    * +--+--+
    * }}}
    */
  object RowSpans {
    final class Backend($: BackendScope[Unit, Unit]) {
      private def rs(row: Int, col: Int, span: Int) = <.td(
        focusable,
        (^.rowSpan := span).unless(span == 1),
        VirtualLoc(0, row, col, None).toString)
      def render: VdomElement =
        <.table(
          <.tbody(
            <.tr(rs(0, 0, 2), rs(0, 1, 1), rs(0, 2, 1)),
            <.tr(             rs(1, 1, 1), rs(1, 2, 1)),
            <.tr(rs(2, 0, 3), rs(2, 1, 2), rs(2, 2, 1)),
            <.tr(                          rs(3, 2, 1)),
            <.tr(             rs(4, 1, 2), rs(4, 2, 1)),
            <.tr(rs(5, 0, 1),              rs(5, 2, 1)),
            <.tr(rs(6, 0, 1), rs(6, 1, 3), rs(6, 2, 1)),
            <.tr(rs(7, 0, 2),              rs(7, 2, 1)),
            <.tr(                          rs(8, 2, 1)),
          )
        )
    }

    val Component = ScalaComponent.builder[Unit]
      .renderBackend[Backend]
      .build

    val rightMoves = MovesBuilder()
      .general(VirtualLoc(0, 0, 0, None))
      .general(VirtualLoc(0, 0, 1, None))
      .general(VirtualLoc(0, 0, 2, None))
      .general(VirtualLoc(0, 0, 0, None))
      .newBatch()
      .general(VirtualLoc(0, 1, 1, None))
      .general(VirtualLoc(0, 1, 2, None))
      .general(VirtualLoc(0, 0, 0, None))
      .newBatch()
      .general(VirtualLoc(0, 2, 0, None))
      .general(VirtualLoc(0, 2, 1, None))
      .general(VirtualLoc(0, 2, 2, None))
      .general(VirtualLoc(0, 2, 0, None))
      .newBatch()
      .general(VirtualLoc(0, 3, 2, None))
      .general(VirtualLoc(0, 2, 0, None))
      .newBatch()
      .general(VirtualLoc(0, 4, 1, None))
      .general(VirtualLoc(0, 4, 2, None))
      .general(VirtualLoc(0, 2, 0, None))
      .newBatch()
      .general(VirtualLoc(0, 5, 2, None))
      .general(VirtualLoc(0, 5, 0, None))
      .general(VirtualLoc(0, 4, 1, None))
      .newBatch()
      .general(VirtualLoc(0, 6, 0, None))
      .general(VirtualLoc(0, 6, 1, None))
      .general(VirtualLoc(0, 6, 2, None))
      .general(VirtualLoc(0, 6, 0, None))
      .newBatch()
      .general(VirtualLoc(0, 7, 2, None))
      .general(VirtualLoc(0, 7, 0, None))
      .general(VirtualLoc(0, 6, 1, None))
      .newBatch()
      .general(VirtualLoc(0, 8, 2, None))
      .general(VirtualLoc(0, 7, 0, None))
      .result()

    val leftMoves = MovesBuilder()
      .general(VirtualLoc(0, 0, 2, None))
      .general(VirtualLoc(0, 0, 1, None))
      .general(VirtualLoc(0, 0, 0, None))
      .general(VirtualLoc(0, 0, 2, None))
      .newBatch()
      .general(VirtualLoc(0, 1, 2, None))
      .general(VirtualLoc(0, 1, 1, None))
      .general(VirtualLoc(0, 0, 0, None))
      .newBatch()
      .general(VirtualLoc(0, 2, 2, None))
      .general(VirtualLoc(0, 2, 1, None))
      .general(VirtualLoc(0, 2, 0, None))
      .general(VirtualLoc(0, 2, 2, None))
      .newBatch()
      .general(VirtualLoc(0, 3, 2, None))
      .general(VirtualLoc(0, 2, 1, None))
      .newBatch()
      .general(VirtualLoc(0, 4, 2, None))
      .general(VirtualLoc(0, 4, 1, None))
      .general(VirtualLoc(0, 2, 0, None))
      .newBatch()
      .general(VirtualLoc(0, 5, 0, None))
      .general(VirtualLoc(0, 5, 2, None))
      .general(VirtualLoc(0, 4, 1, None))
      .newBatch()
      .general(VirtualLoc(0, 6, 2, None))
      .general(VirtualLoc(0, 6, 1, None))
      .general(VirtualLoc(0, 6, 0, None))
      .general(VirtualLoc(0, 6, 2, None))
      .newBatch()
      .general(VirtualLoc(0, 7, 0, None))
      .general(VirtualLoc(0, 7, 2, None))
      .general(VirtualLoc(0, 6, 1, None))
      .newBatch()
      .general(VirtualLoc(0, 8, 2, None))
      .general(VirtualLoc(0, 6, 1, None))
      .result()

    val downMoves = MovesBuilder()
      .general(VirtualLoc(0, 0, 0, None))
      .general(VirtualLoc(0, 2, 0, None))
      .general(VirtualLoc(0, 5, 0, None))
      .general(VirtualLoc(0, 6, 0, None))
      .general(VirtualLoc(0, 7, 0, None))
      .general(VirtualLoc(0, 0, 0, None))
      .newBatch()
      .general(VirtualLoc(0, 0, 1, None))
      .general(VirtualLoc(0, 1, 1, None))
      .general(VirtualLoc(0, 2, 1, None))
      .general(VirtualLoc(0, 4, 1, None))
      .general(VirtualLoc(0, 6, 1, None))
      .general(VirtualLoc(0, 0, 1, None))
      .newBatch()
      .general(VirtualLoc(0, 0, 2, None))
      .general(VirtualLoc(0, 1, 2, None))
      .general(VirtualLoc(0, 2, 2, None))
      .general(VirtualLoc(0, 3, 2, None))
      .general(VirtualLoc(0, 4, 2, None))
      .general(VirtualLoc(0, 5, 2, None))
      .general(VirtualLoc(0, 6, 2, None))
      .general(VirtualLoc(0, 7, 2, None))
      .general(VirtualLoc(0, 8, 2, None))
      .general(VirtualLoc(0, 0, 2, None))
      .result()
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  lazy val lr: html.Table = {
    val root = ReactTestUtils.newBodyElement()
    LR.Component().renderIntoDOM(root).getDOMNode.asMounted().domCast[html.Table]
  }

  lazy val td: html.Table = {
    val root = ReactTestUtils.newBodyElement()
    TD.Component().renderIntoDOM(root).getDOMNode.asMounted().domCast[html.Table]
  }

  lazy val rowSpans: html.Table = {
    val root = ReactTestUtils.newBodyElement()
    RowSpans.Component().renderIntoDOM(root).getDOMNode.asMounted().domCast[html.Table]
  }

  val changeDir: Direction => List[VirtualLoc] => List[VirtualLoc] = {
    case Forwards  => identity
    case Backwards => _.reverse
  }

  def compatibleTableStyles(implicit ts: TableStyle): List[TableStyle] =
    if (ts.hasRowSpans)
      Nil
    else
      // If a table doesn't have rowSpans, it should function the same when rowSpans are enabled
      TableStyle(hasRowSpans = true) :: Nil


  def testCellLabels(table: html.Table)(implicit ts: TableStyle, l: Line): Unit = {
    def text(e: html.Element): String =
      ReactTestUtils.removeReactInternals(e.innerHTML).replaceFirst("\\).+", ")")

    val cells = table.querySelectorAll("td,th").iterator.focusable.toList
    assert(cells.nonEmpty)
    for (c <- cells) {
      val z = TableCellZipper(c)
      val loc = z.focusVLoc.getOrThrow()
      val tableRoot = z.virtualTable.getOrThrow().root
      assertEq("focusVLoc", loc.toString, text(c))
      assert(tableRoot == table)

      val z2 = z.goto(loc).getOrThrow()
      val loc2 = z2.focusVLoc.getOrThrow()
      assertEq("goto(focusVLoc).focusVLoc", loc2.toString, expect = text(c))
      assertEq(loc2, loc)
    }

    compatibleTableStyles.foreach(testCellLabels(table)(_, l))
  }

  def init(table: html.Table)(implicit ts: TableStyle): TableCellZipper =
    TableCellZipper(table.querySelectorAll("td,th").iterator.focusable.next())

  def needGoto(z: TableCellZipper, pos: VirtualLoc)(implicit l: Line): TableCellZipper = {
    val z2 = z.goto(pos).getOrThrow()
    assertEq(s"goto($pos).focusVLoc", z2.focusVLoc, \/-(pos))
    z2
  }

  def testMoves(table: html.Table, axis: Axis, movement: Movement, moves: TestMoves, movesDir: Direction)
               (implicit ts: TableStyle, l: Line): Unit = {
    val z = init(table)
    moves.moveTests.foreach { ms =>
      testMoves2(z, axis, movement, changeDir(movesDir)(ms))
    }

    compatibleTableStyles.foreach(testMoves(table, axis, movement, moves, movesDir)(_, l))
  }

  def testMoves2(z: TableCellZipper, axis: Axis, movement: Movement, moves: List[VirtualLoc])(implicit l: Line): Unit =
    for ((from, to) <- moves zip moves.tail)
      testMove(z, axis, movement, from, to)

  def testMove(z: TableCellZipper, axis: Axis, movement: Movement, from: VirtualLoc, to: VirtualLoc)(implicit l: Line): Unit = {
    val z2 = needGoto(z, from)
    val actual = z2.move(axis, movement).flatMap(_.focusVLoc)
    assertEq(s"$movement along $axis from $from", actual, \/-(to))
  }

  def testSubMoves(table: html.Table, movement: Movement, moves: TestMoves, movesDir: Direction)
                  (implicit ts: TableStyle, l: Line): Unit = {
    val z = init(table)

    def testData: Iterator[List[VirtualLoc]] =
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

    compatibleTableStyles.foreach(testSubMoves(table, movement, moves, movesDir)(_, l))
  }

  def testSubMoves2(z: TableCellZipper, movement: Movement, moves: List[VirtualLoc]): Unit =
    for ((from, to) <- moves zip moves.tail) {
      val z2 = needGoto(z, from)
      val actual = z2.subMove(movement).flatMap(_.traverse(_.focusVLoc))
      assertEq(s"subMove $movement: $from --> $to", actual, \/-(Some(to)))
    }

  override def tests = Tests {

    "lr" - {
      implicit val ts = TableStyle(hasRowSpans = false)
      def t = lr
      def T = LR
      "posDetection"   - testCellLabels(t)
      "moveRight"      - testMoves(t, LeftRight, Next, T.rightMoves, Forwards)
      "moveLeft"       - testMoves(t, LeftRight, Prev, T.rightMoves, Backwards)
      "moveDown"       - testMoves(t, UpDown   , Next, T.downMoves , Forwards)
      "moveUp"         - testMoves(t, UpDown   , Prev, T.upMoves   , Forwards)
      "subMoveLeft"    - testSubMoves(t, Prev, T.rightMoves, Backwards)
      "subMoveRight"   - testSubMoves(t, Next, T.rightMoves, Forwards)
    }

    "td" - {
      implicit val ts = TableStyle(hasRowSpans = false)
      def t = td
      def T = TD
      "posDetection"   - testCellLabels(t)
      "moveRight"      - testMoves(t, LeftRight, Next, T.rightMoves, Forwards)
      "moveLeft"       - testMoves(t, LeftRight, Prev, T.rightMoves, Backwards)
      "moveDown"       - testMoves(t, UpDown   , Next, T.downMoves , Forwards)
      "moveUp"         - testMoves(t, UpDown   , Prev, T.downMoves , Backwards)
      "subMoveLeft"    - testSubMoves(t, Prev, T.rightMoves, Backwards)
      "subMoveRight"   - testSubMoves(t, Next, T.rightMoves, Forwards)

      "fromOuter" - {
        val z = init(t)
        "up"     - testMove(z, UpDown, Prev, VirtualLoc(1, 1, 5, None), VirtualLoc(1, 0, 5, Some(PosXY(0, 2))))
        "down"   - testMove(z, UpDown, Next, VirtualLoc(1, 1, 5, None), VirtualLoc(1, 1, 5, Some(PosXY(0, 0))))
        "top"    - testMove(z, UpDown, Head, VirtualLoc(1, 1, 5, None), VirtualLoc(0, 0, 5, None))
        "bottom" - testMove(z, UpDown, Last, VirtualLoc(1, 1, 5, None), VirtualLoc(1, 1, 5, Some(PosXY(0, 1))))
      }
    }

    "rowSpans" - {
      implicit val ts = TableStyle(hasRowSpans = true)
      def t = rowSpans
      def T = RowSpans
      "posDetection"   - testCellLabels(t)
      "moveRight"      - testMoves(t, LeftRight, Next, T.rightMoves, Forwards)
      "moveLeft"       - testMoves(t, LeftRight, Prev, T.leftMoves , Forwards)
      "moveDown"       - testMoves(t, UpDown   , Next, T.downMoves , Forwards)
      "moveUp"         - testMoves(t, UpDown   , Prev, T.downMoves , Backwards)
    }

  }
}
