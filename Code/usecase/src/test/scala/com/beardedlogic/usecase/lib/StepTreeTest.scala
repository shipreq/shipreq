package com.beardedlogic.usecase.lib

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import com.beardedlogic.usecase.test.TestHelpers
import TypeTags._

class StepTreeTest extends WordSpec with ShouldMatchers with TestHelpers {
  import NodeUtils._
  import StepTree._
  import TestHelpers.TreeDSL._
  import StepLabels.LabelMakers

  implicit def autoTagLocalStepIds(s: String) = s.asLocalStepId

  /**
   * StepNode test data.
   */
  object StepNodes {
    val A = new StepNode("id.A", 1, 1, null)
    val B = new StepNode("id.B", 1, 2, null)
    val C = new StepNode("id.C", 1, 3, null)
    val D = new StepNode("id.D", 1, 4, null)
    val ABCD = A :: B :: C :: D :: Nil

    val A2 = new StepNode("id.A", 1, 2, null)
    val B2 = new StepNode("id.B", 1, 3, null)
    val C2 = new StepNode("id.C", 1, 4, null)
    val D2 = new StepNode("id.D", 1, 5, null)
    val ABCD2 = A2 :: B2 :: C2 :: D2 :: Nil

    val N = new StepNode("id.N", 1, 66, null)
  }

  /**
   * Step test data.
   */
  object Steps {
    val InitialTree = $("1.0" ~> $("1")).toStepNodes
    val Tree_10_11 = $("1.0", "1.1").toStepNodes
    val BT_102 = $("a" ~> $("i", "ii", "iii"), "b", "c" ~> $("i", "ii"))
    val BT_103 = $("a" ~> $("i"), "b")
    val BigTree = $(
      "1.0" ~> $("1", "2" ~> BT_102, "3" ~> BT_103, "4"),
      "1.1" ~> $("1", "2", "3"),
      "1.2" ~> $("1", "2")
    ).toStepNodes
    val N = Step("N")
  }

  // -------------------------------------------------------------------------------------------------------------------

  "mapIdsAndFullLabels()" should {

    def oneLevel: List[StepNode] =
      StepNode("X1", 0, 0, NewStep, Nil) ::
      StepNode("X2", 0, 1, NewStep, Nil) ::
      Nil

    "map ids to labels" in {
      val map = mapIdsAndFullLabels(oneLevel, "1.")
      map("X1") should be("1.0")
      map("X2") should be("1.1")
    }

    "map labels to ids" in {
      val map = mapIdsAndFullLabels(oneLevel, "1.")
      map("1.0") should be("X1")
      map("1.1") should be("X2")
    }

    "map children and generate full labels" in {
      val map = mapIdsAndFullLabels(
        StepNode("X5", 0, 1, NewStep,  // 1.E.1
          new StepNode("X3", 1, 1, NewStep, // 1.E.1.1
            new StepNode("X4", 2, 1, NewStep, Nil) :: Nil // 1.E.1.1.a
          ) ::
          new StepNode("X2", 1, 2, NewStep, Nil) ::  // 1.E.1.2
          Nil
        ) ::
        StepNode("X1", 0, 2, NewStep, Nil) :: // 1.E.2
        Nil, "1.E.")
      map("X5") should be("1.E.1")
      map("X3") should be("1.E.1.1")
      map("X4") should be("1.E.1.1.a")
      map("X2") should be("1.E.1.2")
      map("X1") should be("1.E.2")
      map("1.E.1") should be("X5")
      map("1.E.1.1") should be("X3")
      map("1.E.1.1.a") should be("X4")
      map("1.E.1.2") should be("X2")
      map("1.E.2") should be("X1")
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  "TreeNodeLike stuff" should {
      val c1_0_2_x =
        new StepNode("1.0.2.a", 2, 1, null) ::
          new StepNode("1.0.2.b", 2, 2, null) ::
          Nil

      val c1_0_x =
        new StepNode("1.0.1", 1, 1, null) ::
          new StepNode("1.0.2", 1, 2, null, c1_0_2_x) ::
          new StepNode("1.0.3", 1, 3, null) ::
          Nil

      val c1_2_x =
        new StepNode("1.2.1", 1, 1, null) ::
          Nil

      val top =
        new StepNode("1.0", 0, 0, null, c1_0_x) ::
          new StepNode("1.1", 0, 1, null) ::
          new StepNode("1.2", 0, 2, null, c1_2_x) ::
          Nil

      val ids = List(
        "1.0", "1.0.1", "1.0.2", "1.0.2.a", "1.0.2.b", "1.0.3",
        "1.1",
        "1.2", "1.2.1").sorted

    "node.foreach()" in {
      var list = List.empty[String]
      top(0).foreach(list :+= _.id)
      list.sorted should be(ids.filter(_.startsWith("1.0")))
    }
    "foreachNode()" in {
      var list = List.empty[String]
      top.foreachNode(list :+= _.id)
      list.sorted should be(ids)
    }
    "mapEachNode()" in {
      top.mapEachNode(_.id.asInstanceOf[String]).sorted should be(ids)
    }
  }

  "incrementPosition()" should {
    val test = (lvl: Int, before: String, after: String) => {
      val beforeIndex = LabelMakers(lvl)(before)
      val afterIndex = LabelMakers(lvl)(after)
      val B = new StepNode("blah", lvl, beforeIndex, null)
      val A = new StepNode("blah", lvl, afterIndex, null)
      B.incrementPosition() should be(A)
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
    "increase top-level positions" in {
      val before = new StepNode("blah", 0, 1, null)
      val after = new StepNode("blah", 0, 2, null)
      after.label should be("2")
      before.incrementPosition() should be(after)
    }
  }

  // ------------------------------------------------------------------------------------------------------------------

  "stepInsert()" when {
    import Steps._

    def test(afterId: String, nodes: List[StepNode], expectedTreeTxt: String) {
      val expected = parseStepTree(expectedTreeTxt)
      val actual = stepInsert(N, afterId, nodes)
      actual._2.isDefined should be(true)
      actual._1 should matchTree(expected)
    }

    "tree is in initial state (1.0 & 1.0.1)" should {
      "insert before 1.0.1" in {
        test("1.0", InitialTree, """
          1.0. Step:1.0
            1. N
            2. Step:1
            """)
      }

      "insert after 1.0.1" in {
        test("1.0.1", InitialTree, """
          1.0. Step:1.0
            1. Step:1
            2. N
            """)
      }
    }

    "tree is in state: 1.0 & 1.1" should {
      "creates 1.0.1 after 1.0" in {
        test("1.0", Tree_10_11, """
			1.0. Step:1.0
			  1. N
			1.1. Step:1.1
			""")
      }

      "creates 1.1.1 after 1.1" in {
        test("1.1", Tree_10_11, """
			1.0. Step:1.0
			1.1. Step:1.1
			  1. N
			""")
      }
    }

    "tree is large and deep" when {
      "inserting after 1.0 (lvl 0) should create 1.0.1" in {
        test("1.0", BigTree, """
          1.0. Step:1.0
            1. N
            2. Step:1
            3. Step:2
              a. Step:a
                i. Step:i
                ii. Step:ii
                iii. Step:iii
              b. Step:b
              c. Step:c
                i. Step:i
                ii. Step:ii
            4. Step:3
              a. Step:a
                i. Step:i
              b. Step:b
            5. Step:4
          1.1. Step:1.1
            1. Step:1
            2. Step:2
            3. Step:3
          1.2. Step:1.2
            1. Step:1
            2. Step:2
        """)
      }

      "inserting after 1.0.2 (lvl 1) should create 1.0.2.a" in {
        test("1.0.2", BigTree, """
          1.0. Step:1.0
            1. Step:1
            2. Step:2
              a. N
              b. Step:a
                i. Step:i
                ii. Step:ii
                iii. Step:iii
              c. Step:b
              d. Step:c
                i. Step:i
                ii. Step:ii
            3. Step:3
              a. Step:a
                i. Step:i
              b. Step:b
            4. Step:4
          1.1. Step:1.1
            1. Step:1
            2. Step:2
            3. Step:3
          1.2. Step:1.2
            1. Step:1
            2. Step:2
        """)
      }

      "inserting after 1.0.2.a (lvl 2) should create 1.0.2.a.i" in {
        test("1.0.2.a", BigTree, """
          1.0. Step:1.0
            1. Step:1
            2. Step:2
              a. Step:a
                i. N
                ii. Step:i
                iii. Step:ii
                iv. Step:iii
              b. Step:b
              c. Step:c
                i. Step:i
                ii. Step:ii
            3. Step:3
              a. Step:a
                i. Step:i
              b. Step:b
            4. Step:4
          1.1. Step:1.1
            1. Step:1
            2. Step:2
            3. Step:3
          1.2. Step:1.2
            1. Step:1
            2. Step:2
        """)
      }

      "inserting after 1.0.2.a.i (lvl 3) should create 1.0.2.a.ii" in {
        test("1.0.2.a.i", BigTree, """
          1.0. Step:1.0
            1. Step:1
            2. Step:2
              a. Step:a
                i. Step:i
                ii. N
                iii. Step:ii
                iv. Step:iii
              b. Step:b
              c. Step:c
                i. Step:i
                ii. Step:ii
            3. Step:3
              a. Step:a
                i. Step:i
              b. Step:b
            4. Step:4
          1.1. Step:1.1
            1. Step:1
            2. Step:2
            3. Step:3
          1.2. Step:1.2
            1. Step:1
            2. Step:2
        """)
      }

      "inserting after 1.0.2.c.ii (lvl 3) should create 1.0.2.c.iii" in {
        test("1.0.2.c.ii", BigTree, """
          1.0. Step:1.0
            1. Step:1
            2. Step:2
              a. Step:a
                i. Step:i
                ii. Step:ii
                iii. Step:iii
              b. Step:b
              c. Step:c
                i. Step:i
                ii. Step:ii
                iii. N
            3. Step:3
              a. Step:a
                i. Step:i
              b. Step:b
            4. Step:4
          1.1. Step:1.1
            1. Step:1
            2. Step:2
            3. Step:3
          1.2. Step:1.2
            1. Step:1
            2. Step:2
        """)
      }

      "inserting after 1.1 (lvl 0) should create 1.1.1" in {
        test("1.1", BigTree, """
          1.0. Step:1.0
            1. Step:1
            2. Step:2
              a. Step:a
                i. Step:i
                ii. Step:ii
                iii. Step:iii
              b. Step:b
              c. Step:c
                i. Step:i
                ii. Step:ii
            3. Step:3
              a. Step:a
                i. Step:i
              b. Step:b
            4. Step:4
          1.1. Step:1.1
            1. N
            2. Step:1
            3. Step:2
            4. Step:3
          1.2. Step:1.2
            1. Step:1
            2. Step:2
        """)
      }

      "inserting after 1.1.2 (lvl 1) should create 1.1.3" in {
        test("1.1.2", BigTree, """
          1.0. Step:1.0
            1. Step:1
            2. Step:2
              a. Step:a
                i. Step:i
                ii. Step:ii
                iii. Step:iii
              b. Step:b
              c. Step:c
                i. Step:i
                ii. Step:ii
            3. Step:3
              a. Step:a
                i. Step:i
              b. Step:b
            4. Step:4
          1.1. Step:1.1
            1. Step:1
            2. Step:2
            3. N
            4. Step:3
          1.2. Step:1.2
            1. Step:1
            2. Step:2
        """)
      }
    }
  } // stepInsert()

  "stepRemove()" when {

    def test(id: String, expectedTreeTxt: String) {
      val expected = parseStepTree(expectedTreeTxt)
      val actual = stepRemove(id, Steps.BigTree)
      actual._2.get.id should be(id)
      actual._1 should matchTree(expected)
    }

    "removing 1.0.1" in {
      test("1.0.1", """
        1.0. Step:1.0
          1. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          2. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          3. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "removing 1.0.2" in {
      test("1.0.2", """
        1.0. Step:1.0
          1. Step:1
          2. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          3. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "removing 1.0.2.a" in {
      test("1.0.2.a", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:b
            b. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "removing 1.0.2.a.i" in {
      test("1.0.2.a.i", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:ii
              ii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "removing 1.0.3.a.i" in {
      test("1.0.3.a.i", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "removing 1.1" in {
      test("1.1", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "removing 1.1.1" in {
      test("1.1.1", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:2
          2. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "removing 1.1.3" in {
      test("1.1.3", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "removing 1.2" in {
      test("1.2", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
    """)
    }

    "removing 1.2.2" in {
      test("1.2.2", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
    """)
    }

  } // stepRemove()

  /*
    --    1.0. Step:1.0
    [-      1. Step:1
    [>      2. Step:2
    <-        a. Step:a
    <-          i. Step:i
    <>          ii. Step:ii
    <>          iii. Step:iii
    <>        b. Step:b
    <>        c. Step:c
    <-          i. Step:i
    <>          ii. Step:ii
    [>      3. Step:3
    <-        a. Step:a
    <-          i. Step:i
    <>        b. Step:b
    [>      4. Step:4
    -}    1.1. Step:1.1
    [-      1. Step:1
    [>      2. Step:2
    [>      3. Step:3
    -}    1.2. Step:1.2
    [-      1. Step:1
    [>      2. Step:2

    Rules for increasing indent
      + Not first child

    Rules for decreasing indent
      + Not top-level

    Allow 1st<->2nd level? Yes.
    Allow 1.0 without 1.0.1? Yes.

    Buttons always exist. Control visibility with JS.
      $(".inc").show()
      $(".lvl-0 .inc").hide()
   */

  "indentDecrease()" when {

    def test(id: String, expectedTreeTxt: String) {
      val expected = parseStepTree(expectedTreeTxt)
      val actual = indentDecrease(id, Steps.BigTree)
      actual._2.isDefined should be(true)
      actual._2.get.id should be(id)
      actual._1 should matchTree(expected)
    }

    "decreasing 1.0.3.b" in {
      test("1.0.3.b", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
          4. Step:b
          5. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "decreasing 1.0.3.a" in {
      test("1.0.3.a", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
          4. Step:a
            a. Step:i
            b. Step:b
          5. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "decreasing 1.0.3.a.i" in {
      test("1.0.3.a.i", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
            b. Step:i
            c. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "decreasing 1.0.2.a" in {
      test("1.0.2.a", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
          3. Step:a
            a. Step:i
            b. Step:ii
            c. Step:iii
            d. Step:b
            e. Step:c
              i. Step:i
              ii. Step:ii
          4. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          5. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "decreasing 1.0.2.a.i" in {
      test("1.0.2.a.i", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
            b. Step:i
              i. Step:ii
              ii. Step:iii
            c. Step:b
            d. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "decreasing 1.0.2.a.ii" in {
      test("1.0.2.a.ii", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
            b. Step:ii
              i. Step:iii
            c. Step:b
            d. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "decreasing 1.0.2.a.iii" in {
      test("1.0.2.a.iii", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
            b. Step:iii
            c. Step:b
            d. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "decreasing 1.1.2" in {
      test("1.1.2", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
        1.2. Step:2
          1. Step:3
        1.3. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "decreasing 1.0.2.b" in {
      test("1.0.2.b", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
          3. Step:b
            a. Step:c
              i. Step:i
              ii. Step:ii
          4. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          5. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "decreasing 1.0.2" in {
      test("1.0.2", """
        1.0. Step:1.0
          1. Step:1
        1.1. Step:2
          1. Step:a
            a. Step:i
            b. Step:ii
            c. Step:iii
          2. Step:b
          3. Step:c
            a. Step:i
            b. Step:ii
          4. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          5. Step:4
        1.2. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.3. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

  } // indentDecrease()

  "indentIncrease()" when {

    def test(id: String, expectedTreeTxt: String) {
      val expected = parseStepTree(expectedTreeTxt)
      val actual = indentIncrease(id, Steps.BigTree)
      actual._2.isDefined should be(true)
      actual._2.get.id should be(id)
      actual._1 should matchTree(expected)
    }

    "increasing 1.0.2" in {
      test("1.0.2", """
        1.0. Step:1.0
          1. Step:1
            a. Step:2
              i. Step:a
                1. Step:i
                2. Step:ii
                3. Step:iii
              ii. Step:b
              iii. Step:c
                1. Step:i
                2. Step:ii
          2. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          3. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "increasing 1.0.3" in {
      test("1.0.3", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
            d. Step:3
              i. Step:a
                1. Step:i
              ii. Step:b
          3. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "increasing 1.0.3.b" in {
      test("1.0.3.b", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
              ii. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "increasing 1.0.4" in {
      test("1.0.4", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
            c. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
          3. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "increasing 1.1" in {
      test("1.1", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
          5. Step:1.1
            a. Step:1
            b. Step:2
            c. Step:3
        1.1. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "increasing 1.1.2" in {
      test("1.1.2", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
            a. Step:2
          2. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

    "increasing 1.1.3" in {
      test("1.1.3", """
        1.0. Step:1.0
          1. Step:1
          2. Step:2
            a. Step:a
              i. Step:i
              ii. Step:ii
              iii. Step:iii
            b. Step:b
            c. Step:c
              i. Step:i
              ii. Step:ii
          3. Step:3
            a. Step:a
              i. Step:i
            b. Step:b
          4. Step:4
        1.1. Step:1.1
          1. Step:1
          2. Step:2
            a. Step:3
        1.2. Step:1.2
          1. Step:1
          2. Step:2
    """)
    }

  } // indentIncrease()
}
