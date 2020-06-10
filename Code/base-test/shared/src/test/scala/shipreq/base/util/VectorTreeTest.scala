package shipreq.base.util

import japgolly.microlibs.nonempty.NonEmptyVector
import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import scala.annotation.tailrec
import scala.util.Random
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.BaseUtilGen._
import shipreq.base.test.UseCaseStepUtils._
import shipreq.base.util.VectorTree.{apply => _, _}
import utest._

object VectorTreeTest extends TestSuite {

  val genIntTree = genVectorTree(Gen.int, 4)(0 to 4)

  def allNodesT[A](n: VectorTree[A]): Vector[A] =
    n.children.flatMap(allNodes)

  def allNodes[A](n: Node[A]): Vector[A] =
    n.children.flatMap(allNodes) :+ n.value

  type VTI = VectorTree[Int]
  type PV = Prop[VTI]

  def valueIterator: PV =
    Prop.equal("valueIterator")(
      _.valueIterator.toVector.sorted,
      allNodesT(_).sorted)

  def nodeValueIterator: PV =
    Prop.equal("node.valueIterator")(
      _.children.headOption.map(_.valueIterator.toVector.sorted),
      _.children.headOption.map(allNodes(_).sorted))

  def locAndValueIterator: PV = {
    def values: PV =
      Prop.equal("values")(
        _.locAndValueIterator((_, i) => i).toVector.sorted,
        allNodesT(_).sorted)

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
    def x(name: String, can: Location => Permission, proof: VTI => Location => Option[VTI]): PV =
      Prop.atom("canShift" + name, t =>
        t.locIterator
          .find(l => (can(l) is Allow) ≠ proof(t)(l).isDefined)
          .map("Discrepancy at " + _))

    "CanShift" rename_: (
      x("Left" , VectorTree.canShiftLeft , _.shiftLeft ) ∧
      x("Right", VectorTree.canShiftRight, _.shiftRight) )
  }

  case class MaxDepthTreeTest(src: VTI, mdt: VTI, loc: Location)

  def maxDepthTree: PV = {
    val testNode =
      Prop.equal[MaxDepthTreeTest]("MaxDepthTree node")(
        i => i.mdt.getAtLocation(i.loc),
        i => i.src.at(i.loc).map(_.dims.maxDepth))

    Prop.evaln("MaxDepthTree", src => {
      val mdt = src.maxDepthTree
      val locs = src.locIterator.toList
      Eval.forall(src, locs)(loc => testNode(MaxDepthTreeTest(src, mdt, loc)).liftL)
    })
  }

  val filterInt: Int => NodeFilter =
    i => if (i > 0) NodeFilter.KeepNode else NodeFilter.DiscardNodeAndChildren


  def filterAndPartLocs: PV =
    Prop.eval[VTI](t => testFilterAndPartLocsE(t, t.partLocs(filterInt)))

  def testFilterAndPartLocsE(t: VectorTree[Int], m: Map[Location, PartialLocation]): EvalL = {
    val E = EvalOver(t)
    val k = t.filter(filterInt)

    val origSize = t.locIterator.size

    val allValid = m.iterator.filter(_._2.validity is Valid).toList

    val origToFiltered =
      E.forall(allValid) { case (l1, l2) =>
        val orig = t.at(l1).map(_.value)
        E.test("Key exists in orig: " + l1, orig.isDefined) &
        E.equal(s"$l1 → $l2", k.at(l2.value).map(_.value), expect = orig)
      }

    val goodToOrigLoc = m.iterator.map { case (l, p) => (p.value, l) }.toMap

    val filteredToOrig =
      E.forall(k.locAndValueIterator((loc, v) => (loc, v)).toList) { case (loc, v) =>
        E.equal(s"$v @ $loc", Some(v), t.at(goodToOrigLoc(loc)).map(_.value))
      }

//      k.locAndValueIterator((loc, v) =>
//        E.equal(s"$v @ $loc", Some(v), t.at(goodToOrigLoc(loc)).map(_.value))
//      ).foldLeft(E.pass)(_ & _)

    val filterEqualsRemove = {
      @tailrec def go(vt: VTI): VTI =
        vt.findLoc(_ <= 0) match {
          case Some(l) => go(vt.remove(l).get)
          case None    => vt
        }
      val removed = go(t)
      E.equal("filtered = each removed", k, removed)
    }

    val lookupV: Location => Validity =
      m(_).validity

    def shiftV(name: String,
               canFn: (Location, Location => Validity) => Permission,
               shiftFn: (VTI, Location) => Option[VTI],
               shiftVFn: (VTI, Location, Location => Validity) => Option[VTI]) =
      E.forall(allValid) { case (loc, ploc) =>
        val can = canFn(loc, lookupV)
        shiftFn(k, ploc.value) match {
          case Some(k2) =>
            val t2 = shiftVFn(t, loc, lookupV)
            s"Shift${name}V" rename_: (
              E.equal("new tree size", t2.fold(-1)(_.locIterator.size), origSize)
                & E.equal("new tree filtered", t2.map(_ filter filterInt), Some(k2))
                & E.equal(s"canShift${name}V", can, Allow))
          case None =>
            E.equal(s"canShift${name}V", can, Deny)
        }
      }

    val shiftLeftV = shiftV("Left", VectorTree.canShiftLeftV, _ shiftLeft _, _.shiftLeftV(_, _))
    val shiftRightV = shiftV("Right", VectorTree.canShiftRightV, _ shiftRight _, _.shiftRightV(_, _))

    val outputSize = E.equal("output size", m.size, origSize)

    outputSize ∧ E.distinct("output", m.values) ∧ origToFiltered ∧ filteredToOrig ∧ filterEqualsRemove ∧
      shiftLeftV ∧ shiftRightV
  }

  def testFilterAndPartLocs(t: VectorTree[Int], m: Map[Location, PartialLocation]): Unit =
    () assertSatisfies Prop.eval(_ => testFilterAndPartLocsE(t, m))

  def props: PV = "VectorTree props" rename_: (
    valueIterator ∧ nodeValueIterator ∧ locAndValueIterator ∧ dims ∧ canShift ∧ maxDepthTree ∧ filterAndPartLocs)

  // ===================================================================================================================

  object Steps {
    def n(value: String, c: Node[FakeStep]*): Node[FakeStep] = Node(FakeStep(value, "Step:" + value), c.toVector)
    def r(c: Node[FakeStep]*): VectorTree[FakeStep] = VectorTree(c.toVector)

    val InitialTree = r(n("1.0", n("1")))
    val Tree_10_11 = r(n("1.0"), n("1.1"))

    val BT_102 = Vector(n("a", n("i"), n("ii"), n("iii")), n("b"), n("c", n("i"), n("ii")))
    val BT_103 = Vector(n("a", n("i")), n("b"))
    val BigTree = r(
      n("1.0", n("1"), n("2", BT_102: _*), n("3", BT_103: _*), n("4")),
      n("1.1", n("1"), n("2"), n("3")),
      n("1.2", n("1"), n("2")))

    val N = FakeStep("N", "N")
  }

  implicit class StrExt(private val str: String) extends AnyVal {
    /** from the olden days. Expects UC# prefix, uses labels */
    def ucLoc: Location =
      NonEmptyVector force
        str.split('.').iterator
          .drop(1)
          .zipWithIndex.map { case (s, level) => Labels(level).parse(s).get }
          .toVector

    def loc: Location =
      NonEmptyVector force str.split('.').iterator.map(_.toInt).toVector

    def xloc: PartialLocation =
      PartialLocation.detect(
        NonEmptyVector force
          str.split('.').iterator.map(s => if (s == "X") -1 else s.toInt).toVector)
  }

  override def tests = Tests {
    "props" - { props mustBeSatisfiedBy genIntTree }

    "modifyChildren" - {
      def n(value: Int, c: Node[Int]*): Node[Int] = Node(value, c.toVector)
      def r(c: Node[Int]*): VectorTree[Int] = VectorTree(c.toVector)

      val t = r(n(1, n(2, n(3))), n(9))

      def test(l: Int*)(expect: Option[VectorTree[Int]], subj: VectorTree[Int] = t): Unit =
        assertEq(l.mkString("."),
          subj.modifyChildrenAt(ParentLocation fromVector l.toVector)(_.map(_.map(_ * 10))),
          expect)

      test(          )(Some(r(n(10, n(20, n(30))), n(90))))
      test(Seq(0): _*)(Some(r(n(1, n(20, n(30))), n(9)))) // "Seq(0): _*" because of a bug in -Wunused:locals
      test(0, 0      )(Some(r(n(1, n(2, n(30))), n(9))))
      test(0, 0, 0   )(None)
      test(0, 0, 0, 0)(None)
      test(          )(None, r())
    }

    "partLoc" - {
      val l = Node(1, Vector.empty)
      val d = Node(0, Vector.empty)

      "1" - {
        val t = VectorTree(Vector(l, d, l, d, d, l))

        val m = t.partLocs(filterInt)
        assertMap(m, Map(
          "0".loc -> "0"  .xloc,
          "1".loc -> "X.0".xloc,
          "2".loc -> "1"  .xloc,
          "3".loc -> "X.1".xloc,
          "4".loc -> "X.2".xloc,
          "5".loc -> "2"  .xloc))
        testFilterAndPartLocs(t, m)
      }

      "2" - {
        val lddl = Vector(l, d, d, l)
        val l2 = Node(1, lddl)
        val d2 = Node(0, lddl)
        val t = VectorTree(Vector(l2, d2, d2, l2, d2))

        val m = t.partLocs(filterInt)
        assertMap(m, Map(
          "0"  .loc -> "0"    .xloc,
          "0.0".loc -> "0.0"  .xloc,
          "0.1".loc -> "0.X.0".xloc,
          "0.2".loc -> "0.X.1".xloc,
          "0.3".loc -> "0.1"  .xloc,
          "1"  .loc -> "X.0"  .xloc,
          "1.0".loc -> "X.0.0".xloc,
          "1.1".loc -> "X.0.1".xloc,
          "1.2".loc -> "X.0.2".xloc,
          "1.3".loc -> "X.0.3".xloc,
          "2"  .loc -> "X.1"  .xloc,
          "2.0".loc -> "X.1.0".xloc,
          "2.1".loc -> "X.1.1".xloc,
          "2.2".loc -> "X.1.2".xloc,
          "2.3".loc -> "X.1.3".xloc,
          "3"  .loc -> "1"    .xloc,
          "3.0".loc -> "1.0"  .xloc,
          "3.1".loc -> "1.X.0".xloc,
          "3.2".loc -> "1.X.1".xloc,
          "3.3".loc -> "1.1"  .xloc,
          "4"  .loc -> "X.2"  .xloc,
          "4.0".loc -> "X.2.0".xloc,
          "4.1".loc -> "X.2.1".xloc,
          "4.2".loc -> "X.2.2".xloc,
          "4.3".loc -> "X.2.3".xloc))
        testFilterAndPartLocs(t, m)
      }

      "order" - {
        val plocs = List("0.0", "0.0.0", "0.0.1", "0.1", "X.0", "X.0.0", "X.1", "0.X.0").map(_.xloc)
        assertEq("id", plocs.sorted, plocs)
        assertEq("reverse", plocs.reverse.sorted, plocs)
        assertEq("shuffle", Random.shuffle(plocs).sorted, plocs)
      }
    }

    // =================================================================================================================
    "insertAfter" - {
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
          test("1.0".ucLoc, InitialTree,
            """
              |1.0. Step:1.0
              |  1. N
              |  2. Step:1
            """.stripMargin)

        "insert after 1.0.1" -
          test("1.0.1".ucLoc, InitialTree,
            """
              |1.0. Step:1.0
              |  1. Step:1
              |  2. N
            """.stripMargin)
      }

      "tree is in state: 1.0 & 1.1" - {
        "creates 1.0.1 after 1.0" -
          test("1.0".ucLoc, Tree_10_11,
            """
              |1.0. Step:1.0
              |  1. N
              |1.1. Step:1.1
            """.stripMargin)

        "creates 1.1.1 after 1.1" -
          test("1.1".ucLoc, Tree_10_11,
            """
              |1.0. Step:1.0
              |1.1. Step:1.1
              |  1. N
            """.stripMargin)
      }

      "BigTree" - {
        "inserting after 1.0 (lvl 0) should create 1.0.1" -
          test("1.0".ucLoc, BigTree,
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
          test("1.0.2".ucLoc, BigTree,
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
          test("1.0.2.a".ucLoc, BigTree,
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
          test("1.0.2.a.i".ucLoc, BigTree,
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
          test("1.0.2.c.ii".ucLoc, BigTree,
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
          test("1.1".ucLoc, BigTree,
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
          test("1.1.2".ucLoc, BigTree,
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
        test("1.0.1".ucLoc,
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
        test("1.0.2".ucLoc,
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
        test("1.0.2.a".ucLoc,
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
        test("1.0.2.a.i".ucLoc,
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
        test("1.0.3.a.i".ucLoc,
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
        test("1.1".ucLoc,
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
        test("1.1.1".ucLoc,
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
        test("1.1.3".ucLoc,
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
        test("1.2".ucLoc,
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
        test("1.2.2".ucLoc,
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
        test("1.0.3.b".ucLoc,
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
        test("1.0.3.a".ucLoc,
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
        test("1.0.3.a.i".ucLoc,
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
        test("1.0.2.a".ucLoc,
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
        test("1.0.2.a.i".ucLoc,
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
        test("1.0.2.a.ii".ucLoc,
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
        test("1.0.2.a.iii".ucLoc,
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
        test("1.1.2".ucLoc,
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
        test("1.0.2.b".ucLoc,
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
        test("1.0.2".ucLoc,
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
        test("1.0.2".ucLoc,
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
        test("1.0.3".ucLoc,
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
        test("1.0.3.b".ucLoc,
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
        test("1.0.4".ucLoc,
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
        test("1.1".ucLoc,
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
        test("1.1.2".ucLoc,
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
        test("1.1.3".ucLoc,
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

  }
}
