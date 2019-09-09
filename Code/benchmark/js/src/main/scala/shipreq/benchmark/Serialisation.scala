package shipreq.benchmark

import boopickle.{PickleImpl, UnpickleImpl}
import japgolly.scalajs.benchmark._, gui._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.protocol.binary.v1.BaseMemberData2.picklerProject

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

object Serialisation {

  val deserBM = projectBM.map(PickleImpl intoBytes _)

  val suite = GuiSuite(
    Suite("Binary Serialisation")(

      projectBM("project.write")(
        PickleImpl intoBytes _),

      deserBM("project.read") { b =>
        b.rewind()
        UnpickleImpl[Project] fromBytes b
      }
    )
  )
}
