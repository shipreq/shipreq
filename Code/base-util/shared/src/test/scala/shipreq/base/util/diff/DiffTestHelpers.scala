package shipreq.base.util.diff

import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import japgolly.microlibs.testutil.TestUtil._
import sourcecode.Line
import PatchFactory._

object DiffTestHelpers {

  implicit def autoWrapStrs(s: String): DiffSource[String, Char] =
    DiffSource.Str(s)

  private def ppOp[A](src: DiffSource[Any, A], tgt: DiffSource[Any, A])(op: Op): String = {
    def get(d: DiffSource[Any, A], i: Int) = if (i<0 || i>=d.length) "-" else d(i)
    op match {
      case Op.Insert(s, t, l) => s"Insert $l @ $s <- $t (${get(src, s)} <- ${get(tgt, t)})"
      case Op.Delete(s, l)    => s"Delete $l @ $s (${get(src, s)})"
    }
  }

  def assertCharDiff(src: String, tgt: String)(expectedOps: String*)(implicit l: Line, algo: DiffAlgorithm[Any, Char]): Unit = {
    val ops = assertRoundTrip(src, tgt)
    val pp = ppOp(src, tgt) _
    assertMultiline(ops.map(pp).mkString("\n"), expectedOps.mkString("\n"))
    // assertSeq(ops.map(pp), expectedOps)
  }

  def assertRoundTrip[S, A](src: String, tgt: String)
                           (implicit l: Line, algo: DiffAlgorithm[S, A], A: DiffSource.Auto[String, S, A]): Ops = {
    val ops = algo.diff(src, tgt)(Ops)
    val patched = applyPatch(src, tgt, ops)
    try
      assertEq("patched", patched, tgt)
    catch {
      case t: Throwable =>
        println(ops.map("  - " + _).mkString("Ops:\n", "\n", ""))
        throw t
    }
    ops
  }

  def applyPatch(src: String, tgt: String, ops: Ops): String = {
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

  def propTestChars(samples: Int = 100)(implicit algo: DiffAlgorithm[Any, Char]): Unit = {
    val prop = Prop.equal[(String, String)]("p(a, Δᵃᵇ) = b")(
      actual = _._2,
      expect = { case (x, y) => applyPatch(x, y, algo.diff(x, y)(Ops)) },
    )
    val genStr = Gen.chooseChar_!('A' to 'H').string(0 to 4)
    prop.mustBeSatisfiedBy(genStr.pair)(defaultPropSettings.setSampleSize(samples))
  }

  def propTestLines(samples: Int = 100)(implicit algo: DiffAlgorithm.StrLines): Unit = {
    val prop = Prop.equal[(String, String)]("p(a, Δᵃᵇ) = b")(
      actual = _._2,
      expect = { case (x, y) => applyPatch(x, y, algo.diff(x, y)(Ops)) },
    )
    val genStr = Gen.chooseChar_!('A' to 'D').string(0 to 8)
    val genLines = genStr.arraySeq(0 to 16).map(_.mkString("\n"))
    prop.mustBeSatisfiedBy(genLines.pair)(defaultPropSettings.setSampleSize(samples))
  }
}
