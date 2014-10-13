package shipreq.webapp.shared.prop

import scalaz.EphemeralStream

sealed trait Result[+A]
case object Satisfied             extends Result[Nothing]
case object Proved                extends Result[Nothing]
case class  Falsified[+A](a: A)   extends Result[A]
//case class  Error(reason: String) extends Result[Nothing]


case class Settings(
  sampleSize: SampleSize = SampleSize(5),
  genSize: GenSize       = GenSize(50),
  debug: Boolean         = false,
  debugMaxLen: Int       = 2000)

object Settings {
  val default = Settings()
  object Default {
    implicit def defaultSettings = Settings.default
  }
}


case class RunState[+A](runs: Int, result: Result[A])


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
        case Satisfied             =>
          val r = RunState(rs.runs + 1, test1(p,a))
          if (S.debug) {
            var aa = a.toString
            if (aa.length > S.debugMaxLen) aa = aa.substring(0, S.debugMaxLen) + "…"
            println(s"[${r.runs}/${S.sampleSize.value}] $aa\n")
          }
          r
        case Proved | Falsified(_) =>
          rs
      }
    })
  }

  private def test1[A](p: Prop[A], a: A): Result[A] =
    try {
      if (p test a) Satisfied else Falsified(a)
    } catch {
      case _: Throwable => Falsified(a)
    }
}
