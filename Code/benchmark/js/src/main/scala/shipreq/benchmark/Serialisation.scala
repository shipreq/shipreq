package shipreq.benchmark

import boopickle.{PickleImpl, UnpickleImpl, Pickler}
import shipreq.benchmark.lib.BenchmarkSuite
import shipreq.webapp.base.protocol.BinCodecData

/*
object JsonSerialisation extends BenchmarkSuite("JsonSerialisation") {
  implicit val projectCodec = DataCodecs.project

  def benchmarkWrite[A: ReadWriter](name: String, start: => A): Unit =
    initBenchmark(s"write_$name", start)(Fns write _)

  override def configureOptions =
    _.minSamples = 50

  benchmarkWrite("100", data.project_100)

//  benchmarkWrite("1000", data.project_1000)
}


object JsonDeserialisation extends BenchmarkSuite("JsonDeserialisation") {
  implicit val projectCodec = DataCodecs.project

  def benchmarkRead[A: ReadWriter](name: String, start: => A): Unit =
    initBenchmark(s"read_$name", Fns write start)(Fns.read[A])

  override def configureOptions =
    _.minSamples = 20

  benchmarkRead("100", data.project_100)

//  benchmarkRead("1000", data.project_1000)
}
*/

object BinSerialisation extends BenchmarkSuite("BinSerialisation") {
  implicit val projectCodec = BinCodecData.pickleProject

  def benchmarkWrite[A: Pickler](name: String, start: => A): Unit =
    initBenchmark(s"write_$name", start)(PickleImpl intoBytes _)

  override def configureOptions =
    _.minSamples = 50

  benchmarkWrite("100", data.project_100)

//  benchmarkWrite("1000", data.project_1000)
}


object BinDeserialisation extends BenchmarkSuite("BinDeserialisation") {
  implicit val projectCodec = BinCodecData.pickleProject

  def benchmarkRead[A: Pickler](name: String, start: => A): Unit =
    initBenchmark(s"read_$name", PickleImpl intoBytes start)(b => {
      b.rewind()
      UnpickleImpl[A] fromBytes b
    })

  override def configureOptions =
    _.minSamples = 20

  benchmarkRead("100", data.project_100)

//  benchmarkRead("1000", data.project_1000)
}