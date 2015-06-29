package shipreq.benchmark

import shipreq.webapp.base.protocol.DataCodecs
import upickle._

object Deserialisiation {
  implicit val projectCodec = DataCodecs.project
  val prefix = "Deserialisiation."

  var bms = Vector.empty[Benchmark]

  def addBenchmark[A: ReadWriter](name: String, start: => A): Unit =
    bms :+= Benchmark.init(prefix + name, Fns write start)(Fns.read[A])

  addBenchmark("read_100",  data.project_100)
  addBenchmark("read_1000", data.project_1000)

  def run(): Unit = {
    val o = Benchmark.defaultOptions()
    // o.maxTime = 30
    Benchmark.run(bms: _*)(o)
  }
}