package shipreq.benchmark

import java.nio.ByteBuffer
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import shipreq.webapp.base.data.Project

/*
@State(Scope.Benchmark)
//@BenchmarkMode(Array(Mode.SampleTime))
//@OutputTimeUnit(TimeUnit.MICROSECONDS)
class JsonSerialisation {

  implicit val projectCodec = DataCodecs.project
  val p100  = data.project_100
//  val p1000 = data.project_1000

  @Benchmark
  def write_100 = upickle.Fns write p100

//  @Benchmark
//  def write_1000 = upickle.Fns write p1000
}

@State(Scope.Benchmark)
class Deserialisation {

  implicit val projectCodec = DataCodecs.project
  val p100  = upickle.Fns write data.project_100
//  val p1000 = upickle.Fns write data.project_1000

  @Benchmark
  def read_100: Project = upickle.Fns read p100

//  @Benchmark
//  def read_1000: Project = upickle.Fns read p1000
}
*/

// ===================================================================================================

import boopickle.{PickleImpl, UnpickleImpl}
import shipreq.webapp.base.protocol.binary.v1.BaseMemberData2.picklerProject

@State(Scope.Benchmark)
class BinSerialisation {

  val p100  = data.project_100
//  val p1000 = data.project_1000

  @Benchmark
  def write_100 = PickleImpl intoBytes p100

//  @Benchmark
//  def write_1000 = PickleImpl intoBytes p1000
}

@State(Scope.Benchmark)
class BinDeserialisation {

  val p100  = PickleImpl intoBytes data.project_100
//  val p1000 = PickleImpl intoBytes data.project_1000

  val u = UnpickleImpl[Project]
  def read(bb: ByteBuffer): Project = {
    bb.rewind()
    u fromBytes bb
  }

  @Benchmark
  def read_100 = read(p100)

//  @Benchmark
//  def read_1000 = read(p1000)
}
