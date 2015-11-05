package shipreq.base.util

import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.BaseUtilGen._
import shipreq.base.test.UseCaseStepUtils._
import IndexLabel._
import VectorTree.{apply => _, _}

object VectorTreeTest extends TestSuite {

  val genIntTree = genVectorTree(Gen.int, 4)(0 to 4)

  def allNodes[A](n: Node[A]): Vector[A] =
    n.children.flatMap(allNodes) :+ n.value

  type VTI = VectorTree[Int]
  type PV = Prop[VTI]

  def values: PV =
    Prop.equal("values")(
      _.valueIterator.toVector.sorted,
      _.children.flatMap(allNodes).sorted)

  def locAndValueIterator: PV = {
    def values: PV =
      Prop.equal("values")(
        t => t.locAndValueIterator((_, i) => i).toVector.sorted,
        _.children.flatMap(allNodes).sorted)

    def locations: PV =
      Prop.atom("locs", t => {
        val results = t.locAndValueIterator((_, _))
        val bad = results.filter{ case(l, i) => t.getAtLocation(l) != Option(i) }.toList
        bad.headOption.map{ case(l, i) => s"Bad result: (${l.whole mkString "."}) = ${t.getAtLocation(l)} not $i}" }
      })

    (values ∧ locations) rename "locAndValueIterator"
  }

  def dims: PV =
    Prop.equal("Dims")(
      _.dims,
      t => {
        var ml = 0
        var md = 0
        for (loc <- t.locAndValueIterator((loc, _) => loc)) {
          md = md max loc.length
          ml = ml max (loc.whole.max + 1)
        }
        Dims(maxLength = ml, maxDepth = md)
      })

  def canShift: PV = {
    def x(name: String, can: VTI => Location => Boolean, proof: VTI => Location => Option[VTI]): PV =
      Prop.atom("canShift" + name, t =>
        t.locIterator
          .find(l => can(t)(l) ≠ proof(t)(l).isDefined)
          .map("Discrepancy at " + _))

    "CanShift" rename_: (
      x("Left" , _.canShiftLeft , _.shiftLeft ) ∧
      x("Right", _.canShiftRight, _.shiftRight) )
  }

  def props: PV =
    (values ∧ locAndValueIterator ∧ dims ∧ canShift) rename "VectorTree props"

  // ===================================================================================================================

  object Steps {
    def n(value: String, c: Node[FakeStep]*): Node[FakeStep] =
      Node(FakeStep(value, "Step:" + value), c.toVector)

    def r(c: Node[FakeStep]*): VectorTree[FakeStep] =
      VectorTree(c.toVector)

    val InitialTree = r(n("1.0", n("1")))
    val Tree_10_11 = r(n("1.0"), n("1.1"))

    val BT_102 = Vector(n("a", n("i"), n("ii"), n("iii")), n("b"), n("c", n("i"), n("ii")))
    val BT_103 = Vector(n("a", n("i")), n("b"))
    val BigTree = r(
      n("1.0", n("1"), n("2", BT_102: _*), n("3", BT_103: _*), n("4")),
      n("1.1", n("1"), n("2"), n("3")),
      n("1.2", n("1"), n("2")))

    val N = Node(FakeStep("N", "N"), Vector.empty)
  }

  implicit class StrExt(private val str: String) extends AnyVal {
    def toLoc: Location =
      NonEmptyVector force
        str.split('.').iterator
          .drop(1)
          .zipWithIndex.map { case (s, level) => Labels(level).parse(s).get }
          .toVector
  }

  override def tests = TestSuite {
    'props { props mustBeSatisfiedBy genIntTree }

    // =================================================================================================================
    'insertAfter {
      import Steps._

      def test(at: Location, src: VectorTree[FakeStep], expectedTreeTxt: String): Unit = {
        val expected = parseStepTree(expectedTreeTxt)
        src.insertAfter(at, N) match {
          case Some(actual) => assertTreeValues(actual, expected)
          case None         => fail("insertAfter failed")
        }
      }

      "tree is in initial state (1.0 & 1.0.1)" - {
        "insert before 1.0.1" -
          test("1.0".toLoc, InitialTree,
            """
              |1.0. Step:1.0
              |  1. N
              |  2. Step:1
            """.stripMargin)

        "insert after 1.0.1" -
          test("1.0.1".toLoc, InitialTree,
            """
              |1.0. Step:1.0
              |  1. Step:1
              |  2. N
            """.stripMargin)
      }

      "tree is in state: 1.0 & 1.1" - {
        "creates 1.0.1 after 1.0" -
          test("1.0".toLoc, Tree_10_11,
            """
              |1.0. Step:1.0
              |  1. N
              |1.1. Step:1.1
            """.stripMargin)

        "creates 1.1.1 after 1.1" -
          test("1.1".toLoc, Tree_10_11,
            """
              |1.0. Step:1.0
              |1.1. Step:1.1
              |  1. N
            """.stripMargin)
      }

      "BigTree" - {
        "inserting after 1.0 (lvl 0) should create 1.0.1" -
          test("1.0".toLoc, BigTree,
            """
              |1.0. Step:1.0
              |  1. N
              |  2. Step:1
              |  3. Step:2
              |    a. Step:a
              |      i. Step:i
              |      ii. Step:ii
              |      iii. Step:iii
              |    b. Step:b
              |    c. Step:c
              |      i. Step:i
              |      ii. Step:ii
              |  4. Step:3
              |    a. Step:a
              |      i. Step:i
              |    b. Step:b
              |  5. Step:4
              |1.1. Step:1.1
              |  1. Step:1
              |  2. Step:2
              |  3. Step:3
              |1.2. Step:1.2
              |  1. Step:1
              |  2. Step:2
            """.stripMargin)

        "inserting after 1.0.2 (lvl 1) should create 1.0.2.a" -
          test("1.0.2".toLoc, BigTree,
            """
              |1.0. Step:1.0
              |  1. Step:1
              |  2. Step:2
              |    a. N
              |    b. Step:a
              |      i. Step:i
              |      ii. Step:ii
              |      iii. Step:iii
              |    c. Step:b
              |    d. Step:c
              |      i. Step:i
              |      ii. Step:ii
              |  3. Step:3
              |    a. Step:a
              |      i. Step:i
              |    b. Step:b
              |  4. Step:4
              |1.1. Step:1.1
              |  1. Step:1
              |  2. Step:2
              |  3. Step:3
              |1.2. Step:1.2
              |  1. Step:1
              |  2. Step:2
            """.stripMargin)

        "inserting after 1.0.2.a (lvl 2) should create 1.0.2.a.i" -
          test("1.0.2.a".toLoc, BigTree,
            """
              |1.0. Step:1.0
              |  1. Step:1
              |  2. Step:2
              |    a. Step:a
              |      i. N
              |      ii. Step:i
              |      iii. Step:ii
              |      iv. Step:iii
              |    b. Step:b
              |    c. Step:c
              |      i. Step:i
              |      ii. Step:ii
              |  3. Step:3
              |    a. Step:a
              |      i. Step:i
              |    b. Step:b
              |  4. Step:4
              |1.1. Step:1.1
              |  1. Step:1
              |  2. Step:2
              |  3. Step:3
              |1.2. Step:1.2
              |  1. Step:1
              |  2. Step:2
            """.stripMargin)

        "inserting after 1.0.2.a.i (lvl 3) should create 1.0.2.a.ii" -
          test("1.0.2.a.i".toLoc, BigTree,
            """
              |1.0. Step:1.0
              |  1. Step:1
              |  2. Step:2
              |    a. Step:a
              |      i. Step:i
              |      ii. N
              |      iii. Step:ii
              |      iv. Step:iii
              |    b. Step:b
              |    c. Step:c
              |      i. Step:i
              |      ii. Step:ii
              |  3. Step:3
              |    a. Step:a
              |      i. Step:i
              |    b. Step:b
              |  4. Step:4
              |1.1. Step:1.1
              |  1. Step:1
              |  2. Step:2
              |  3. Step:3
              |1.2. Step:1.2
              |  1. Step:1
              |  2. Step:2
            """.stripMargin)

        "inserting after 1.0.2.c.ii (lvl 3) should create 1.0.2.c.iii" -
          test("1.0.2.c.ii".toLoc, BigTree,
            """
              |1.0. Step:1.0
              |  1. Step:1
              |  2. Step:2
              |    a. Step:a
              |      i. Step:i
              |      ii. Step:ii
              |      iii. Step:iii
              |    b. Step:b
              |    c. Step:c
              |      i. Step:i
              |      ii. Step:ii
              |      iii. N
              |  3. Step:3
              |    a. Step:a
              |      i. Step:i
              |    b. Step:b
              |  4. Step:4
              |1.1. Step:1.1
              |  1. Step:1
              |  2. Step:2
              |  3. Step:3
              |1.2. Step:1.2
              |  1. Step:1
              |  2. Step:2
            """.stripMargin)

        "inserting after 1.1 (lvl 0) should create 1.1.1" -
          test("1.1".toLoc, BigTree,
            """
              |1.0. Step:1.0
              |  1. Step:1
              |  2. Step:2
              |    a. Step:a
              |      i. Step:i
              |      ii. Step:ii
              |      iii. Step:iii
              |    b. Step:b
              |    c. Step:c
              |      i. Step:i
              |      ii. Step:ii
              |  3. Step:3
              |    a. Step:a
              |      i. Step:i
              |    b. Step:b
              |  4. Step:4
              |1.1. Step:1.1
              |  1. N
              |  2. Step:1
              |  3. Step:2
              |  4. Step:3
              |1.2. Step:1.2
              |  1. Step:1
              |  2. Step:2
            """.stripMargin)

        "inserting after 1.1.2 (lvl 1) should create 1.1.3" -
          test("1.1.2".toLoc, BigTree,
            """
              |1.0. Step:1.0
              |  1. Step:1
              |  2. Step:2
              |    a. Step:a
              |      i. Step:i
              |      ii. Step:ii
              |      iii. Step:iii
              |    b. Step:b
              |    c. Step:c
              |      i. Step:i
              |      ii. Step:ii
              |  3. Step:3
              |    a. Step:a
              |      i. Step:i
              |    b. Step:b
              |  4. Step:4
              |1.1. Step:1.1
              |  1. Step:1
              |  2. Step:2
              |  3. N
              |  4. Step:3
              |1.2. Step:1.2
              |  1. Step:1
              |  2. Step:2
            """.stripMargin)
      }
    } // insert

    // =================================================================================================================
    "remove" - {
      def test(loc: Location, expectedTreeTxt: String): Unit = {
        val expected = parseStepTree(expectedTreeTxt)
        Steps.BigTree.remove(loc) match {
          case Some(actual) => assertTreeValues(actual, expected)
          case None         => fail("remove failed")
        }
      }

      "1.0.1" -
        test("1.0.1".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  2. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  3. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.2" -
        test("1.0.2".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  3. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.2.a" -
        test("1.0.2.a".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:b
            |    b. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.2.a.i" -
        test("1.0.2.a.i".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:ii
            |      ii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.3.a.i" -
        test("1.0.3.a.i".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.1" -
        test("1.1".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.1.1" -
        test("1.1.1".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:2
            |  2. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.1.3" -
        test("1.1.3".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.2" -
        test("1.2".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
          """.stripMargin)

      "1.2.2" -
        test("1.2.2".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
          """.stripMargin)

    } // remove

    // =================================================================================================================
    // Indenting

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

    "shiftLeft" - {
      def test(loc: Location, expectedTreeTxt: String): Unit = {
        val expected = parseStepTree(expectedTreeTxt)
        Steps.BigTree.shiftLeft(loc) match {
          case Some(actual) => assertTreeValues(actual, expected)
          case None         => fail("shiftLeft failed")
        }
      }

      "1.0.3.b" -
        test("1.0.3.b".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |  4. Step:b
            |  5. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.3.a" -
        test("1.0.3.a".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |  4. Step:a
            |    a. Step:i
            |    b. Step:b
            |  5. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.3.a.i" -
        test("1.0.3.a.i".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |    b. Step:i
            |    c. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.2.a" -
        test("1.0.2.a".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |  3. Step:a
            |    a. Step:i
            |    b. Step:ii
            |    c. Step:iii
            |    d. Step:b
            |    e. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  4. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  5. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.2.a.i" -
        test("1.0.2.a.i".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |    b. Step:i
            |      i. Step:ii
            |      ii. Step:iii
            |    c. Step:b
            |    d. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.2.a.ii" -
        test("1.0.2.a.ii".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |    b. Step:ii
            |      i. Step:iii
            |    c. Step:b
            |    d. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.2.a.iii" -
        test("1.0.2.a.iii".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |    b. Step:iii
            |    c. Step:b
            |    d. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.1.2" -
        test("1.1.2".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |1.2. Step:2
            |  1. Step:3
            |1.3. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.2.b" -
        test("1.0.2.b".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |  3. Step:b
            |    a. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  4. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  5. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.2" -
        test("1.0.2".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |1.1. Step:2
            |  1. Step:a
            |    a. Step:i
            |    b. Step:ii
            |    c. Step:iii
            |  2. Step:b
            |  3. Step:c
            |    a. Step:i
            |    b. Step:ii
            |  4. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  5. Step:4
            |1.2. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.3. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

    } // shiftLeft

    "shiftRight" - {
      def test(loc: Location, expectedTreeTxt: String): Unit = {
        val expected = parseStepTree(expectedTreeTxt)
        Steps.BigTree.shiftRight(loc) match {
          case Some(actual) => assertTreeValues(actual, expected)
          case None         => fail("shiftRight failed")
        }
      }

      "1.0.2" -
        test("1.0.2".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |    a. Step:2
            |      i. Step:a
            |        1. Step:i
            |        2. Step:ii
            |        3. Step:iii
            |      ii. Step:b
            |      iii. Step:c
            |        1. Step:i
            |        2. Step:ii
            |  2. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  3. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.3" -
        test("1.0.3".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |    d. Step:3
            |      i. Step:a
            |        1. Step:i
            |      ii. Step:b
            |  3. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.3.b" -
        test("1.0.3.b".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.0.4" -
        test("1.0.4".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |    c. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |  3. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.1" -
        test("1.1".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |  5. Step:1.1
            |    a. Step:1
            |    b. Step:2
            |    c. Step:3
            |1.1. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.1.2" -
        test("1.1.2".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |    a. Step:2
            |  2. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

      "1.1.3" -
        test("1.1.3".toLoc,
          """
            |1.0. Step:1.0
            |  1. Step:1
            |  2. Step:2
            |    a. Step:a
            |      i. Step:i
            |      ii. Step:ii
            |      iii. Step:iii
            |    b. Step:b
            |    c. Step:c
            |      i. Step:i
            |      ii. Step:ii
            |  3. Step:3
            |    a. Step:a
            |      i. Step:i
            |    b. Step:b
            |  4. Step:4
            |1.1. Step:1.1
            |  1. Step:1
            |  2. Step:2
            |    a. Step:3
            |1.2. Step:1.2
            |  1. Step:1
            |  2. Step:2
          """.stripMargin)

    } // shiftRight

    // =================================================================================================================

//  "mapIdsAndFullLabels()" should {
//
//    def oneLevel: StepTree = StepNode("X1", 0, 0) :: StepNode("X2", 0, 1) :: Nil
//
//    "map ids to labels" in {
//      val map = mapIdsToFullLabels(oneLevel, "1.")
//      map("X1").value shouldEqual "1.0"
//      map("X2").value shouldEqual "1.1"
//    }
//
//    "map children and generate full labels" in {
//      val map = mapIdsToFullLabels(
//        StepNode("X5", 0, 1,  // 1.E.1
//          new StepNode("X3", 1, 1, // 1.E.1.1
//            new StepNode("X4", 2, 1, Nil) :: Nil // 1.E.1.1.a
//          ) ::
//          new StepNode("X2", 1, 2, Nil) ::  // 1.E.1.2
//          Nil
//        ) ::
//        StepNode("X1", 0, 2, Nil) :: // 1.E.2
//        Nil, "1.E.")
//      map("X5").value shouldEqual "1.E.1"
//      map("X3").value shouldEqual "1.E.1.1"
//      map("X4").value shouldEqual "1.E.1.1.a"
//      map("X2").value shouldEqual "1.E.1.2"
//      map("X1").value shouldEqual "1.E.2"
//    }
//  }

  }
}
