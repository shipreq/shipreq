package shipreq.webapp.shared.prop

import scalaz.EphemeralStream

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


case class Settings(
  sampleSize: SampleSize = SampleSize(100),
  genSize: GenSize       = GenSize(40),
  debug: Boolean         = false,
  debugMaxLen: Int       = 960) {

  private[prop] lazy val sampleSizeLen = sampleSize.value.toString.length
  private[prop] lazy val sampleProgressFmt = s"[%${sampleSizeLen}d/${sampleSize.value}] "
}

object Settings {
  val default = Settings()
  object Default {
    implicit def defaultSettings = Settings.default
  }
}


case class RunState[+A](runs: Int, result: Result[A])
object RunState {
  implicit def RunStateToResult[A](r: RunState[A]): Result[A] = r.result
}


object PTest {

  def apply[A](p: Prop[A], gen: Gen[A])(implicit S: Settings): RunState[A] = {
    val data = gen.gen2(S.genSize).f(S.sampleSize).take(S.sampleSize.value)
    testN(p, data)
  }

  // exhaustive
//  def prove[A](settings: Settings, p: Prop[A], data: EphemeralStream[A]): RunState[A] =
//    testN(p, data) match {
//      case RunState(r, Satisfied) => RunState(r, Proved)
//      case r => r
//    }

  private def testN[A](p: Prop[A], data: EphemeralStream[A])(implicit S: Settings): RunState[A] = {
    if (S.debug) println()
    data.foldLeft(RunState[A](0, Satisfied))(rs => a => {
      rs.result match {
        case Satisfied =>
          val r = RunState(rs.runs + 1, test1(p,a))
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
    if (al > 120) println()
  }

  private def test1[A](p: Prop[A], a: A): Result[A] =
    try {
      if (p test a) Satisfied else Falsified(a)
    } catch {
      case _: Throwable => Falsified(a)
    }
}
