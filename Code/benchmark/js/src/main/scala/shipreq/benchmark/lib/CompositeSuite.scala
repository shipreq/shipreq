package shipreq.benchmark.lib

import shipreq.benchmark.jslib.JsBenchmark.Options

object CompositeSuite {
  def apply(name: String, cfgOptions: Options => Unit = _ => ())(suites: BenchmarkSuite*): BenchmarkSuite =
    new BenchmarkSuite(name) {
      override def configureOptions = cfgOptions
      suites foreach addSuite
    }
}