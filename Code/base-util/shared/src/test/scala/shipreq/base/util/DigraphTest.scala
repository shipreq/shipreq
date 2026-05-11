package shipreq.base.util

import japgolly.microlibs.testutil.TestUtil._
import utest._

object DigraphTest extends TestSuite {

  override def tests = Tests {

    "rootsAndTerminals" - {

      // 1 → 2
      // 3 → 4 → 5
      //   ↘ 6 → 8 → 9
      //   ↗       ↗
      // 7 →   →
      val forwards = Digraph.emptyUniDir[Int]
        .add(1, 2)
        .add(3, 4)
        .add(4, 5)
        .add(3, 6)
        .add(6, 8)
        .add(8, 9)
        .add(7, 6)
        .add(7, 9)
      val graph = Digraph.BiDir(forwards)
      val rat = Digraph.RootsAndTerminals.derive(graph)
      assertEq("members", rat.members, Set(1, 2, 3, 4, 5, 6, 7, 8, 9))
      assertEq("roots", rat.roots, Set(1, 3, 7))
      assertEq("terminals", rat.terminals, Set(2, 5, 9))
    }

    "stronglyConnectedComponents" - {
      val forwards = Digraph.emptyUniDir[Int]

        .add(0, 1)
        .add(1, 2)
        .add(2, 0)
        .add(0, 3)
        .add(3, 4)
        .add(4, 0)
        .add(0, 5)
        .add(5, 7)

        .add(0, 9)
        .add(9, 10)
        .add(10, 11)
        .add(11, 12)
        .add(12, 9)
        .add(9, 11)

        .add(1, 12)

        .add(3, 5)
        .add(5, 6)
        .add(6, 7)
        .add(7, 8)
        .add(8, 6)
        .add(6, 15)

        .add(5, 13)
        .add(13, 14)
        .add(14, 13)
        .add(13, 15)

        .add(8, 15)

        .add(10, 13)

      val expect = Set[NonEmptySet[Int]](
        NonEmptySet(15),
        NonEmptySet(13, 14),
        NonEmptySet(9, 10, 11, 12),
        NonEmptySet(6, 7, 8),
        NonEmptySet(5),
        NonEmptySet(0, 1, 2, 3, 4),
      )

      val g = Digraph.BiDir(forwards)
      val actual = g.stronglyConnectedComponents

//      def log(name: String, data: Set[NonEmptySet[Int]]): Unit = {
//        println(name + ":")
//        data.toArray.map(_.whole.mkString("  [", ",", "]")).sortInPlace().foreach(println)
//      }
//      log("Actual", actual)
//      log("Expect", expect)

      assertSet(actual, expect)
    }

  }
}
