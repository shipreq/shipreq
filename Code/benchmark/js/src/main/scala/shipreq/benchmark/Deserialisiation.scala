package shipreq.benchmark

import shipreq.benchmark.lib.BenchmarkSuite
import shipreq.webapp.base.protocol.DataCodecs
import upickle._

object Deserialisiation extends BenchmarkSuite("Deserialisiation") {
  implicit val projectCodec = DataCodecs.project

  def benchmarkRead[A: ReadWriter](name: String, start: => A): Unit =
    initBenchmark(s"read_$name", Fns write start)(Fns.read[A])

  override def configureOptions =
    _.minSamples = 20

  benchmarkRead("100", data.project_100)

  benchmarkRead("1000", data.project_1000)
}