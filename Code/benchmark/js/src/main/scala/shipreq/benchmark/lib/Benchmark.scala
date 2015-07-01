package shipreq.benchmark.lib

import org.scalajs.dom
import shipreq.benchmark.jslib.JsBenchmark
import shipreq.benchmark.jslib.JsBenchmark.Options

object Benchmark {
  type Logger = String => Unit
  val console: Logger = dom.console.log(_)

  def onCycleN(updateEvery: Int, f: JsBenchmark.Event => Unit) =
    JsBenchmark.cb { e =>
      val l = e.target.stats.sample.length
      if (l != 0 && l % updateEvery == 0)
        f(e)
    }

  def format(b: JsBenchmark.Benchmark): String =
    if (b.error.isDefined)
      b.toString()
    else {
      import b._
//      def fmtNum(d: Double, prec: Int) = ("%,." + prec + "f").format(d)
      def fmtNum(d: Double) = "%,.9f".format(d)
      s"$name : ${fmtNum(hz)} ops/sec \u00b1 ${fmtNum(stats.rme / 100.0 * hz)} (${stats.sample.length} samples)"
    }

  def logWithStatus(log: Logger, status: String)(e: JsBenchmark.Event, msElapsed: Long): Unit = {
    val s = format(e.target)
    val secElapsed = (msElapsed / 1000).toInt
    val sec = "%02d".format(secElapsed % 60)
    val min = secElapsed / 60
    log(s"$s -- $status [$min:$sec]")
  }

  def defaultOptions(log: Logger): Options = {
    val o = Options()
    o.async = true

    var startTime = 0L
    var lastTime  = startTime
    def currentTime() = System.currentTimeMillis()

    o.onStart = JsBenchmark.cb { e =>
      startTime = currentTime()
      lastTime  = startTime
      log(s"${e.target.name} started")
    }

    def updateEvery(msSinceStart: Long): Long =
      if (msSinceStart < 65000L)
        5000
      else
        15000

    o.onCycle = JsBenchmark.cb { e =>
      val t             = currentTime()
      val timeSinceLast = t - lastTime
      val totalTime     = t - startTime
      val every         = updateEvery(totalTime)
      if (timeSinceLast >= every) {
        lastTime = t
        logWithStatus(log, "Running")(e, totalTime)
      }
    }

    o.onComplete = JsBenchmark.cb { e =>
      val totalTime = currentTime() - startTime
      logWithStatus(log, "Done")(e, totalTime)
    }

    o
  }

  def apply(name: String, f: => Any): Benchmark = {
    new Benchmark {
      override def bm(options: Options) = {
        val fn = JsBenchmark.fn(f)
        new JsBenchmark(name, fn, options)
      }
    }
  }

  def init[A](name: String, a0: => A)(f: A => Any): Benchmark =
    new Benchmark {
      lazy val a1 = a0
      override def bm(options: Options) = {
        val a2 = a1
        val fn = JsBenchmark.fn(f(a2))
        new JsBenchmark(name, fn, options)
      }
    }

  def run(bms: Benchmark*)(o: Options): Unit =
    for (bm <- bms)
      bm.bm(o).run()
}

trait Benchmark {
  def bm(options: Options): JsBenchmark
}