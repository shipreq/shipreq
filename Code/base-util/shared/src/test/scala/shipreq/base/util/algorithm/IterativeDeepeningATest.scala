package shipreq.base.util.algorithm

import scala.collection.immutable.ArraySeq
import japgolly.microlibs.testutil.TestUtil._
import utest._

object IterativeDeepeningATest extends TestSuite {

  def main(args: Array[String]): Unit =
    testMaze()

  private def testMaze(): Unit = {
    val board: ArraySeq[ArraySeq[Boolean]] =
      ArraySeq(
        "________".iterator.map(_ == 'X').to(ArraySeq),
        "________".iterator.map(_ == 'X').to(ArraySeq),
        "___XXX__".iterator.map(_ == 'X').to(ArraySeq),
        "_____X__".iterator.map(_ == 'X').to(ArraySeq),
        "__X__X_X".iterator.map(_ == 'X').to(ArraySeq),
        "__X__X__".iterator.map(_ == 'X').to(ArraySeq),
        "__XXXX__".iterator.map(_ == 'X').to(ArraySeq),
        "X_______".iterator.map(_ == 'X').to(ArraySeq),
      )

    final case class Pos(x: Int, y: Int) {
      override def toString = s"($x,$y)"

      def directDist(to: Pos): Double =
        Math.hypot((x - to.x).abs, (y - to.y).abs)

      def stepDist(to: Pos): Double =
        if (x == to.x)
          (y - to.y).abs
        else if (y == to.y)
          (x - to.x).abs
        else
          directDist(to)

      def isValid: Boolean =
        x >= 0 && x <= 7 && y >= 0 && y <= 7

      def neighbours: Array[Pos] = {
        val moves =
          for {
            dx <- -1 to 1
            dy <- -1 to 1
          } yield Pos(x + dx, y + dy)
        moves.iterator.filter(_.isValid).filterNot(_ == this).toArray
      }

      def isWall: Boolean =
        board(y)(x)
    }

    val goal = Pos(7, 7)

    val ida = IterativeDeepeningA[Pos](
      estCost = _.directDist(goal),
      cost = (x, _, y) => if (y.isWall) 100 else x.stepDist(y),
      isGoal = (n, _) => n == goal,
      children = _.neighbours,
    )

    val path = ida.findPathFrom(Pos(0, 0))
    val pathNodes = path.toSet

    def draw(x: Int, y: Int) = {
      val p = Pos(x, y)
      if (pathNodes.contains(p)) {
        assert(!p.isWall)
        "*"
      } else if (p.isWall)
        "#"
      else
        "-"
    }

    val result =
      (0 to 7).iterator
        .map(y => (0 to 7).iterator.map(draw(_, y)).mkString)
        .mkString("\n")

    val expect =
      """*-------
        |-*****--
        |---###*-
        |-----#*-
        |--#--#*#
        |--#--#-*
        |--####-*
        |#------*
        |""".stripMargin

    assertMultiline(result.trim, expect.trim)
  }

  override def tests = Tests {
    "maze" - testMaze()
  }
}
