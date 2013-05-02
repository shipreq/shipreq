package com.beardedlogic.usecase.lib

import StepTree.{ Step, StepNode, flattenNodes, incrementPosition, insertStep }
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class StepTreeTest extends WordSpec with ShouldMatchers {
  import StepTree._

  /**
   * StepNode test data.
   */
  object StepNodes {
    val A = StepNode("id.A", 1, "1", null, Nil)
    val B = StepNode("id.B", 1, "2", null, Nil)
    val C = StepNode("id.C", 1, "3", null, Nil)
    val D = StepNode("id.D", 1, "4", null, Nil)
    val ABCD = A :: B :: C :: D :: Nil

    val A2 = StepNode("id.A", 1, "2", null, Nil)
    val B2 = StepNode("id.B", 1, "3", null, Nil)
    val C2 = StepNode("id.C", 1, "4", null, Nil)
    val D2 = StepNode("id.D", 1, "5", null, Nil)
    val ABCD2 = A2 :: B2 :: C2 :: D2 :: Nil

    val N = StepNode("id.N", 1, "N", null, Nil)
  }

  /**
   * Step test data.
   */
  object Steps {
    val A = Step("A*")
    val A1 = Step("A.1")
    val A2 = Step("A.2")
    val A3 = Step("A.3")
    val A4 = Step("A.4")
    val B = Step("B*")
    val C = Step("C*")
    val N = Step("N")

    val `1.0 & 1.0.1` = StepNode("id.1.0", 0, "1.0", A, StepNode("id.1.0.1", 1, "1", B, Nil) :: Nil) :: Nil
    val `1.0.[1234]` = nodeRow(1, "1.0.", (1, A1), (2, A2), (3, A3), (4, A4))
    val `1.0 & 1.0.[1234]` = StepNode("id.1.0", 0, "1.0", A, `1.0.[1234]`) :: Nil
  }

  /**
   * Builds a row of nodes.
   */
  def nodeRow(lvl: Int,
              idPrefix: String,
              nodes: Tuple2[Any, Step]*): List[StepNode] =
    List((
      for ((lbl, step) <- nodes)
        yield StepNode(if (idPrefix == null) null else idPrefix + lbl.toString, lvl, lbl.toString, step, Nil)
    ): _*)

  /**
   * Recursively sets all IDs to null.
   */
  def removeIds(l: List[StepNode]): List[StepNode] = l.map((n) => n.copy(id = null, children = removeIds(n.children)))

  // -------------------------------------------------------------------------------------------------------------------

  "flattenNodes()" should {
    "flatten recursively" in {

      val c1_0_2_x =
        StepNode("1.0.2.a", 2, "a", null, Nil) ::
          StepNode("1.0.2.b", 2, "b", null, Nil) ::
          Nil

      val c1_0_x =
        StepNode("1.0.1", 1, "1", null, Nil) ::
          StepNode("1.0.2", 1, "2", null, c1_0_2_x) ::
          StepNode("1.0.3", 1, "3", null, Nil) ::
          Nil

      val c1_2_x =
        StepNode("1.2.1", 1, "1", null, Nil) ::
          Nil

      val top =
        StepNode("1.0", 0, "1.0", null, c1_0_x) ::
          StepNode("1.1", 0, "1.1", null, Nil) ::
          StepNode("1.2", 0, "1.1", null, c1_2_x) ::
          Nil

      flattenNodes(top).map(_.id) should be(List(
        "1.0", "1.0.1", "1.0.2", "1.0.2.a", "1.0.2.b", "1.0.3",
        "1.1",
        "1.2", "1.2.1"))
    }
  }

  "incrementPosition()" should {
    val test = (lvl: Int, before: String, after: String) => {
      val B = StepNode("blah", lvl, before, null, Nil)
      val A = StepNode("blah", lvl, after, null, Nil)
      incrementPosition(B) should be(A)
    }
    "increase numeric positions" in {
      test(1, "1", "2")
      test(1, "9", "10")
      test(1, "15", "16")
    }
    "increase alpha positions" in {
      test(2, "a", "b")
      test(2, "h", "i")
      test(2, "z", "aa")
    }
    "increase roman positions" in {
      test(3, "i", "ii")
      test(3, "iii", "iv")
      test(3, "viii", "ix")
    }
  }

  /*
   * TODO
   * #
   * |-- 1.0
   *     |-- 1.0.1
   *     |-- 1.0.2
   *         | -- 1.0.2.a
   *              | -- 1.0.2.a.i
   *              | -- 1.0.2.a.ii
   *              | -- 1.0.2.a.iii
   *         | -- 1.0.2.b
   *         | -- 1.0.2.c
   *     |-- 1.0.3
   *         | -- 1.0.3.a
   *              | -- 1.0.3.a.i
   *              | -- 1.0.3.a.ii
   *              | -- 1.0.3.a.iii
   *         | -- 1.0.3.b
   *         | -- 1.0.3.c
   *              | -- 1.0.3.c.i
   *              | -- 1.0.3.c.ii
   *              | -- 1.0.3.c.iii
   *         | -- 1.0.3.d
   *     |-- 1.0.4 
   * |-- 2.0
   *     |-- 2.0.1
   *     |-- 2.0.2
   *     |-- 2.0.3
   * |-- 2.1
   *     |-- 2.1.1
   *     |-- 2.1.2
   * 
   */

  "insertStep()" when {
    import Steps._

    "tree is 1.0 & 1.0.1" should {
      "insert before 1.0.1" in {
        removeIds(insertStep(N, "id.1.0", `1.0 & 1.0.1`)._1) should be(
          StepNode(null, 0, "1.0", A, nodeRow(1, null, (1, N), (2, B))) :: Nil
        )
      }
      "insert after 1.0.1" in {
        removeIds(insertStep(N, "id.1.0.1", `1.0 & 1.0.1`)._1) should be(
          StepNode(null, 0, "1.0", A, nodeRow(1, null, (1, B), (2, N))) :: Nil
        )
      }
    }
  }

}