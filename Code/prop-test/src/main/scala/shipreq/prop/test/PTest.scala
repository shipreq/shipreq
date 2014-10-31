package shipreq.prop.test

import com.nicta.rng.Rng
import scalaz.EphemeralStream, EphemeralStream._
import shipreq.prop._

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

  def empty[A] = RunState[A](0, Satisfied)
}

trait Executor {
  def run[A](p: Prop[A], data: EphemeralStream[A], S: Settings): RunState[A]
}

object Executor {
  import PTest._

  object SingleThreadedExecutor extends Executor {
    override def run[A](p: Prop[A], data: EphemeralStream[A], S: Settings): RunState[A] = {
      val it = EphemeralStream.toIterable(data).iterator
      var rs = RunState[A](0, Satisfied)
      while (rs.success && it.hasNext) {
        val a = it.next()
        rs = RunState(rs.runs + 1, test1(p, a)) // TODO pass to executor? Ctx(rs.runs, S)
        if (S.debug) debug1(a, rs, S)
      }
      rs
    }
  }

  object ParallelExecutor extends Executor {
    import java.util.concurrent._

    override def run[A](p: Prop[A], datas: EphemeralStream[A], S: Settings): RunState[A] = {
      println("executror...")

      val workers = 6

      val (next, fail) = {
        val lock = new Object
        @volatile var done = datas.isEmpty

        val fail: () => Unit =
          () => done = true

        val next: () => Option[(Int, () => A)] = {
          var nextData: () => EphemeralStream[A] = () => datas
          var runcnt = 0
          () => if (done) None else lock.synchronized {
            val cur = nextData()
            if (cur.isEmpty || done) {
              done = true
              None
            } else {
              runcnt += 1
              nextData = cur.tail
              Some((runcnt, cur.head))
            }
          }
        }
        (next, fail)
      }

      def task(worker: Int) = new Callable[RunState[A]] {
        override def call(): RunState[A] = {
          var rs = RunState[A](0, Satisfied)
          var n = next()
          while (n.isDefined) {
            val (r, fa) = n.get
            val a = fa()
            rs = RunState(r, test1(p, a))
            if (S.debug) debug1(a, rs, S)
            if (!rs.success) fail()
            n = next()
          }
          rs
        }
      }

      println("workers...")
      val es: ExecutorService = Executors.newFixedThreadPool(workers)
      val fs = (1 to workers).toList.map(es submit task(_))
      es.shutdown()
      val rss = fs.map(_.get())
      es.awaitTermination(1, TimeUnit.MINUTES)

      println("done...")

      def merge(a: RunState[A], b: RunState[A]): RunState[A] =
        (a.success, b.success) match {
          case (true, false)             => b
          case (false, true)             => a
          case (_, _) if a.runs < b.runs => a
          case _                         => b
        }

      val rs = rss.foldLeft(RunState.empty[A])(merge)
      rs
    }
  }

}

object PTest {

  def apply[A](p: Prop[A], gen: Gen[A], S: Settings): RunState[A] = {
    if (S.debug) println(s"\n$p")
    val sizedist = if (S.sizeDist.isEmpty) Seq((1D, 1D)) else S.sizeDist
    val data = sizedist.foldLeft(Rng.insert(EphemeralStream[A])){ case (q, (sr, gr)) =>
        val s = S.sampleSize.map(v => (v * sr + 0.5).toInt max 1)
        val g = S.genSize.map(v => (v * gr + 0.5).toInt max 0)
        if (S.debug) println(s"Generating ${s.value} samples @ sz ${g.value}...")
        val x = gen.gen2(g).f(s).map(_ take s.value)
        x.flatMap(y => q.map(_ ++ y))
      }
      .run.unsafePerformIO()
    S.executor.run(p, data, S)
  }

  // exhaustive
//  def prove[A](settings: Settings, p: Prop[A], data: EphemeralStream[A]): RunState[A] =
//    testN(p, data) match {
//      case RunState(r, Satisfied) => RunState(r, Proved)
//      case r => r
//    }

//  private def testN[A](p: Prop[A], data: EphemeralStream[A])(implicit S: Settings): RunState[A] = {
//    val it = EphemeralStream.toIterable(data).iterator
//    var rs = RunState[A](0, Satisfied)
//    while (rs.success && it.hasNext) {
//      val a = it.next()
//      rs = RunState(rs.runs + 1, test1(p, a)) // TODO pass to executor? Ctx(rs.runs, S)
//      if (S.debug) debug1(a, rs)
//    }
//    rs
//  }

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
    if (al > 200) println()
  }

  def test1[A](p: Prop[A], a: A): Result[A] =
    try {
      if (p.test(a)) Satisfied else Falsified(a)
    } catch {
      case _: Throwable => Falsified(a)
    }
}
