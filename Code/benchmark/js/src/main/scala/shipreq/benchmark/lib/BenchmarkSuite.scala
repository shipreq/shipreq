package shipreq.benchmark.lib

import shipreq.benchmark.jslib.JsBenchmark.Options
import Benchmark.Logger

abstract class BenchmarkSuite(val suiteName: String) {

  private var bms = Vector.empty[Benchmark]

  protected def add(b: Benchmark): Unit =
    bms :+= b

  def addSuite(s: BenchmarkSuite): Unit =
    bms ++= s.bms

  def benchmark(name: String, fn: => Any): Unit =
    add(Benchmark(s"$suiteName.$name", fn))

  def initBenchmark[A](name: String, a0: => A)(f: A => Any) =
    add(Benchmark.init(s"$suiteName.$name", a0)(f))

  def configureOptions: Options => Unit = _ => ()

  def run(log: Logger, resultLog: Logger): Unit = {
    val o = Benchmark.defaultOptions(log, resultLog)
    configureOptions(o)
    Benchmark.run(bms: _*)(o)
  }
}
