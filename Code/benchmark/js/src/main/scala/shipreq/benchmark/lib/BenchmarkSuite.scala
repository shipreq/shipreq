package shipreq.benchmark.lib

import shipreq.benchmark.jslib.JsBenchmark.Options

abstract class BenchmarkSuite(val suiteName: String) {

  private var bms = Vector.empty[Benchmark]

  protected def add(b: Benchmark): Unit =
    bms :+= b

  def benchmark(name: String, fn: => Any): Unit =
    add(Benchmark(s"$suiteName.$name", fn))

  def initBenchmark[A](name: String, a0: => A)(f: A => Any) =
    add(Benchmark.init(s"$suiteName.$name", a0)(f))

  def configureOptions: Options => Unit = _ => ()

  def run(log: Benchmark.Logger): Unit = {
    val o = Benchmark.defaultOptions(log)
    configureOptions(o)
    Benchmark.run(bms: _*)(o)
  }
}