package shipreq.base.util

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.testutil.TestUtil._
import utest._

object ConsolidatedSeqTest extends TestSuite {

  override def tests = Tests {

    'noConsolidation - {
      def test(size: Int) = {
        val logic      = ConsolidatedSeq.Logic[Int](_ => false)(identity)
        val as         = Vector.tabulate(size)(100 + _)
        val indices    = 0.until(size).toList
        val c          = logic.consolidate(as)
        def el(i: Int) = Some(ConsolidatedSeq.Group(NonEmptyVector.one(i + 100), i, i))
        assertEq("length", size, c.length)
        assertEq("groups", indices, c.groupIterator().toList)
        assertEq("values", indices.map(el), c.elementIterator().toList)
        c.map(_.values.mkString("[", ",", "]"))
      }

      "0" - test(0)
      "1" - test(1)
      "2" - test(2)
      "3" - test(3)
      "9" - test(9)
    }

    'consolidateAll - {
      def test(size: Int) = {
        val logic      = ConsolidatedSeq.Logic[Int](_ => true)(identity)
        val as         = Vector.tabulate(size)(100 + _)
        val indices    = 0.until(size).toList
        val c          = logic.consolidate(as)
        def el(i: Int) = if (i > 0) None else Some(ConsolidatedSeq.Group(NonEmptyVector.force(as), 0, 0))
        assertEq("length", size, c.length)
        assertEq("groups", List.fill(size)(0), c.groupIterator().toList)
        assertEq("values", indices.map(el), c.elementIterator().toList)
        c.map(_.values.mkString("[", ",", "]"))
      }

      "0" - test(0)
      "1" - test(1)
      "2" - test(2)
      "3" - test(3)
      "9" - test(9)
    }

    'consolidateOdd - {
      val logic  = ConsolidatedSeq.Logic[Int](i => (i.cur & 1) != 0)(identity)
      val as     = Vector(10, 11, 12, 13, 14)
      val c      = logic.consolidate(as)
      val expect = List(
        Some(ConsolidatedSeq.Group(NonEmptyVector(10, 11), 0, 0)),
        None,
        Some(ConsolidatedSeq.Group(NonEmptyVector(12, 13), 2, 1)),
        None,
        Some(ConsolidatedSeq.Group(NonEmptyVector(14), 4, 2)))

      assertEq("length", 5, c.length)
      assertEq("groups", List(0, 0, 1, 1, 2), c.groupIterator().toList)
      assertEq("values", expect, c.elementIterator().toList)
      c.map(_.values.mkString("[", ",", "]"))
    }

  }
}
