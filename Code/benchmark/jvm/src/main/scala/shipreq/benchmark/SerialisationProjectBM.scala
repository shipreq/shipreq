package shipreq.benchmark

import boopickle.{PickleImpl, UnpickleImpl}
import java.nio.ByteBuffer
import org.openjdk.jmh.annotations._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.protocol.binary.v1.Rev1.picklerProject

object SerialisationProjectBM {
  val p100 = SampleData.project_100
  val b100 = PickleImpl intoBytes p100

  val deser: ByteBuffer => Project = {
    val unpickler = UnpickleImpl[Project]
    bb => {
      bb.rewind()
      unpickler fromBytes bb
    }
  }

}

@State(Scope.Benchmark)
class SerialisationProjectBM {

  import SerialisationProjectBM._

  @Setup
  def setup(): Unit = {
    SerialisationProjectBM
    ()
  }

  @Benchmark
  def read = deser(b100)

  @Benchmark
  def write = PickleImpl intoBytes p100

}
