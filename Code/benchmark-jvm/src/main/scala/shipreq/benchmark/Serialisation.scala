package shipreq.benchmark

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import shipreq.webapp.base.protocol.DataCodecs

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.SampleTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class Serialisation {

  implicit val projectCodec = DataCodecs.project
  private val p100  = data.Project_100.project
  private val p1000 = data.Project_1000.project

  @Benchmark
  def serialise_100 = upickle.Fns write p100

  @Benchmark
  def serialise_1000 = upickle.Fns write p1000

}
