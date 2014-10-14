package shipreq.webapp.shared.prop

import scalaz.EphemeralStream
import shipreq.base.prop._

sealed trait Result[+A] {
  def success: Boolean = this match {
    case Satisfied | Proved => true
    case Falsified(_)       => false
  }
}
case object Satisfied             extends Result[Nothing]
case object Proved                extends Result[Nothing]
case class  Falsified[+A](a: A)   extends Result[A]
//case class  Error(reason: String) extends Result[Nothing]

case class RunState[+A](runs: Int, result: Result[A])
object RunState {
  implicit def RunStateToResult[A](r: RunState[A]): Result[A] = r.result
}

object PTest {

  def apply[A](p: Prop[A], gen: Gen[A])(implicit S: Settings): RunState[A] = {
    if (S.debug) println(s"\n$p")
    val data =
      EphemeralStream((if (S.sizeDist.isEmpty) Seq((1D, 1D)) else S.sizeDist): _*)
      .flatMap { case (sr, gr) =>
        val s = S.sampleSize.map(v => (v * sr + 0.5).toInt max 1)
        val g = S.genSize.map(v => (v * gr + 0.5).toInt max 0)
        if (S.debug) println(s"Generating ${s.value} samples @ sz ${g.value}...")
        gen.gen2(g).f(s).take(s)
      }
    testN(p, data)
  }

  // exhaustive
//  def prove[A](settings: Settings, p: Prop[A], data: EphemeralStream[A]): RunState[A] =
//    testN(p, data) match {
//      case RunState(r, Satisfied) => RunState(r, Proved)
//      case r => r
//    }

  private def testN[A](p: Prop[A], data: EphemeralStream[A])(implicit S: Settings): RunState[A] = {
    data.foldLeft(RunState[A](0, Satisfied))(rs => a => {
      rs.result match {
        case Satisfied =>
          val r = RunState(rs.runs + 1, test1(p, Ctx(a, rs.runs, S)))
          if (S.debug) debug1(a, r)
          r
        case Proved | Falsified(_) =>
          rs
      }
    })
  }

  private def debug1[A](a: A, r: RunState[A])(implicit S: Settings): Unit = {
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
    if (al > 200) println()
  }

  private def test1[A](p: Prop[A], x: Ctx[A]): Result[A] =
    try {
      if (p test x) Satisfied else Falsified(x.a)
    } catch {
      case _: Throwable => Falsified(x.a)
    }
}
