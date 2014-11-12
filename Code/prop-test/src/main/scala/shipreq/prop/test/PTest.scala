package shipreq.prop.test

import com.nicta.rng.Rng
import scalaz.{-\/, \/-, EphemeralStream}
import shipreq.prop._
import Executor.Data

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
    def samples(s: SampleSize, g: GenSize): Rng[EphemeralStream[A]] = {
      if (S.debug) println(s"Generating ${s.value} samples @ sz ${g.value}...")
      gen.gen2(g).f(s).map(_ take s.value)
    }
    def sampleSizePerc(s: SampleSize, p: Double): SampleSize =
      s.map(v => (v * p + 0.5).toInt max 1)
    def genSizePerc(s: GenSize, p: Double): GenSize =
      s.map(v => (v * p + 0.5).toInt max 0)
    val data: Data[A] = s =>
      if (S.sizeDist.isEmpty)
        samples(s, S.genSize).run
      else {
        val total = S.sizeDist.foldLeft(0)(_ + _._1).toDouble
        def ssPerc(i: Int) = i.toDouble / total
        S.sizeDist.toStream.map {
          case (si, -\/(gp)) => samples(sampleSizePerc(s, ssPerc(si)), genSizePerc(S.genSize, gp))
          case (si, \/-(gs)) => samples(sampleSizePerc(s, ssPerc(si)), gs)
        }
        .foldLeft(Rng insert EphemeralStream[A])((a, b) => b.flatMap(c => a.map(_ ++ c)))
        .run
      }
    S.executor.run(p, data, S)
  }

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


  def prove[A](p: Prop[A], d: Domain[A], S1: Settings): RunState[A] = {
    val S = S1.copy(sampleSize = SampleSize(d.size))
    if (S.debug) println(s"\n$p\nAttempting to prove with ${d.size} values...")
    S.executor.prove(p, d, S) match {
      case RunState(n, Satisfied()) if n == d.size =>
        RunState(n, Proved())
      case r =>
        if (S.debug && r.success) println(s"Test was successful but didn't prove proposition: $r")
        r
    }
  }

  def proveN[A](p: Prop[A], d: Domain[A], start: Int, step: Int, runInc: Int => Int, S: Settings): RunState[A] = {
    var rs = RunState.empty[A]
    var i = start
    while (rs.success && i < d.size) {
      val a = d(i)
      rs = RunState(runInc(i), test1(p, a))
      if (S.debug) debug1(a, rs, S)
      i += step
    }
    rs
  }
}
