package shipreq.benchmark

import org.scalajs.dom
import shipreq.benchmark.jslib.JsBenchmark
import shipreq.benchmark.jslib.JsBenchmark.Options

object Benchmark {

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

  def log(msg: String): Unit = {
    dom.console.log(msg)
//    val t = dom.document.createTextNode(msg + "\n")
//    val n = dom.document.createElement("pre")
//    n.appendChild(t)
//    dom.document.body.appendChild(n)
  }

  def logWithStatus(status: String)(e: JsBenchmark.Event): Unit = {
    val s = format(e.target)
    log(s"$s -- $status")
  }

  def defaultOptions(): Options = {
    val o        = Options()
    o.async      = false
    o.minSamples = 40
    o.maxTime    = 60 * 10
    o.onStart    = JsBenchmark.cb(e => log(s"${e.target.name} started"))
    o.onCycle    = onCycleN(10, logWithStatus("Running"))
    o.onComplete = JsBenchmark.cb { e => logWithStatus("Done")(e); log("") }
    o
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