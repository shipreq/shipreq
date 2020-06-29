package shipreq.benchmark

import boopickle.{PickleImpl, UnpickleImpl}
import java.nio.ByteBuffer
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.protocol.binary.v1.Latest.picklerProject
import shipreq.webapp.sampledata.SampleData

object SerialisationBinaryProjectBM {

  val deser: ByteBuffer => Project = {
    val unpickler = UnpickleImpl[Project]
    bb => {
      bb.rewind()
      unpickler fromBytes bb
    }
  }
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class SerialisationBinaryProjectBM {

  @Param(Array("full", "no_req_codes"))
  var `type`: String = _

  @Param(Array("1000", "2000", "4000", "10000"))
  var events: String = _

  private var p: Project = _
  private var b: ByteBuffer = _

  private[this] val deser = SerialisationBinaryProjectBM.deser

  @Setup
  def setup(): Unit = {
    val d = SampleData.byParams(`type`, events)
    p = d.project
    b = d.projectBinary.toByteBuffer
    ()
  }

  @Benchmark
  def read = deser(b)

  @Benchmark
  def write = PickleImpl intoBytes p

}
