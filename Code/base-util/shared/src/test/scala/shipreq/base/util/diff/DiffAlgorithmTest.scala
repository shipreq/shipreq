package shipreq.base.util.diff

import japgolly.microlibs.testutil.TestUtil._
import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import sourcecode.Line
import utest._

abstract class DiffAlgorithmTest(algo: DiffAlgorithm) extends TestSuite {
  import PatchFactory._

  private implicit def autoWrapStrs(s: String): DiffSource[Char] =
    DiffSource.fromString(s)

  private def ppOp[A](src: DiffSource[A], tgt: DiffSource[A])(op: Op): String = {
    def get(d: DiffSource[A], i: Int) = if (i<0 || i>=d.length) "-" else d(i)
    op match {
      case Op.Insert(s, t, l) => s"Insert $l @ $s <- $t (${get(src, s)} <- ${get(tgt, t)})"
      case Op.Delete(s, l)    => s"Delete $l @ $s (${get(src, s)})"
    }
  }

  private def assertCharDiff(src: String, tgt: String)(expectedOps: String*)(implicit l: Line): Unit = {
    val ops = algo.diff(src, tgt)(Ops)
    val pp = ppOp(src, tgt) _
    assertSeq(ops.map(pp), expectedOps)
    val patched = applyPatch(src, tgt, ops)
    assertEq("patched", patched, tgt)
  }

  private def applyPatch(src: String, tgt: String, ops: Ops): String = {
    var s = src
    ops.reverseIterator.foreach {
      case Op.Delete(srcIdx, len) =>
        s = s.patch(srcIdx, "", len)
      case Op.Insert(srcIdx, tgtIdx, len) =>
        val r = tgt.substring(tgtIdx, tgtIdx + len)
        s = s.patch(srcIdx, r, 0)
    }
    s
  }

  override def tests = Tests {

    "1" - assertCharDiff("bcdefgzio", "abcxyfgi")(
      "Insert 1 @ 0 <- 0 (b <- a)",
      "Delete 1 @ 2 (d)",
      "Insert 2 @ 3 <- 3 (e <- x)",
      "Delete 1 @ 3 (e)",
      "Delete 1 @ 6 (z)",
      "Delete 1 @ 8 (o)",
    )

    "2" - assertCharDiff("omg here it is", "omg! There it goes!")(
      "Insert 1 @ 3 <- 3 (  <- !)",
      "Insert 1 @ 4 <- 5 (h <- T)",
      "Insert 2 @ 12 <- 14 (i <- g)",
      "Delete 1 @ 12 (i)",
      "Insert 1 @ 13 <- 16 (s <- e)",
      "Insert 1 @ 14 <- 18 (- <- !)",
    )

    "prop" - {
      val prop = Prop.equal[(String, String)]("p(a, Δᵃᵇ) = b")(
        actual = _._2,
        expect = { case (x, y) => applyPatch(x, y, algo.diff(x, y)(Ops)) },
      )
      val genStr = Gen.chooseChar_!('A' to 'D').string(0 to 92)
      prop.mustBeSatisfiedBy(genStr.pair) //(defaultPropSettings.setSampleSize(1000))
    }

  }

}
