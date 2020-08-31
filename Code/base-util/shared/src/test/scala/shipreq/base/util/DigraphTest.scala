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
  }
}
