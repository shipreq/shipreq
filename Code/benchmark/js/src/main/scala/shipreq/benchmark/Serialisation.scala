package shipreq.benchmark

import shipreq.benchmark.lib.BenchmarkSuite
import shipreq.webapp.base.protocol.DataCodecs
import upickle._

object Serialisation extends BenchmarkSuite("Serialisation") {
  implicit val projectCodec = DataCodecs.project

  def benchmarkWrite[A: ReadWriter](name: String, start: => A): Unit =
    initBenchmark(s"write_$name", start)(Fns write _)

  override def configureOptions =
    _.minSamples = 50

  benchmarkWrite("100", data.project_100)

//  benchmarkWrite("1000", data.project_1000)
}


object Deserialisation extends BenchmarkSuite("Deserialisation") {
  implicit val projectCodec = DataCodecs.project

  def benchmarkRead[A: ReadWriter](name: String, start: => A): Unit =
    initBenchmark(s"read_$name", Fns write start)(Fns.read[A])

  override def configureOptions =
    _.minSamples = 20

  benchmarkRead("100", data.project_100)

//  benchmarkRead("1000", data.project_1000)
}