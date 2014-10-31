package shipreq.prop.test

import scalaz.EphemeralStream
import shipreq.prop._

sealed trait Result[A] {
  def success: Boolean = this match {
    case Satisfied() | Proved()        => true
    case Falsified(_, _) | Error(_, _) => false
  }
}
final case class Satisfied[A]()                          extends Result[A]
final case class Proved   [A]()                          extends Result[A]
final case class Falsified[A](a: A, f: Falsification[A]) extends Result[A]
final case class Error    [A](a: A, e: Throwable)        extends Result[A]

case class RunState[A](runs: Int, result: Result[A])
object RunState {
  implicit def RunStateToResult[A](r: RunState[A]): Result[A] = r.result

  def empty[A] = RunState[A](0, Satisfied())
}

object PTest {

  def apply[A](p: Prop[A], gen: Gen[A], S: Settings): RunState[A] = {
    if (S.debug) println(s"\n$p")
    /*
    val sizedist = if (S.sizeDist.isEmpty) Seq((1D, 1D)) else S.sizeDist
    val data = sizedist.foldLeft(Rng.insert(EphemeralStream[A])){ case (q, (sr, gr)) =>
        val s = S.sampleSize.map(v => (v * sr + 0.5).toInt max 1)
        val g = S.genSize.map(v => (v * gr + 0.5).toInt max 0)
        if (S.debug) println(s"Generating ${s.value} samples @ sz ${g.value}...")
        val x = gen.gen2(g).f(s).map(_ take s.value)
        x.flatMap(y => q.map(_ ++ y))
      }
      .run.unsafePerformIO()
      */
    S.executor.run(p, s => gen.gen2(S.genSize).f(s).map(_ take s.value).run, S)
  }

  // exhaustive
//  def prove[A](settings: Settings, p: Prop[A], data: EphemeralStream[A]): RunState[A] =
//    testN(p, data) match {
//      case RunState(r, Satisfied) => RunState(r, Proved)
//      case r => r
//    }

  def testN[A](p: Prop[A], data: EphemeralStream[A], runInc: () => Int, S: Settings): RunState[A] = {
    val it = EphemeralStream.toIterable(data).iterator
    var rs = RunState.empty[A]
    while (rs.success && it.hasNext) {
      val a = it.next()
      rs = RunState(runInc(), test1(p, a))
      if (S.debug) debug1(a, rs, S)
    }
    rs
  }

  def debug1[A](a: A, r: RunState[A], S: Settings): Unit = {
    def c(code: String, m: Any) = s"\033[${code}m$m\033[0m"
    var aa = a.toString
    val maxLen = if (r.success) S.debugMaxLen else aa.length
    val al = aa.length
    if (al > maxLen)
      aa = aa.substring(0, maxLen)
    aa = c("37", aa)
    if (al > maxLen)
      aa = s"%s … %.0f%%".format(aa, maxLen.toDouble / al * 100.0)
    val pc = if (r.success) "32;1" else "31;1"
    println(s"${c(pc, S.sampleProgressFmt.format(r.runs))}$aa")
    //if (al > 200) println()
  }

  def test1[A](p: Prop[A], a: A): Result[A] =
    try {
      p.falsify(a).fold(Satisfied(): Result[A])(Falsified(a, _))
    } catch {
      case e: Throwable => Error(a, e)
    }
}
