package shipreq.benchmark

import boopickle.{PickleImpl, UnpickleImpl}
import java.nio.ByteBuffer
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.protocol.binary.v1.Rev1.picklerProject

object SerialisationProjectBM {

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
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class SerialisationProjectBM {

  private var p: Project = _
  private var b: ByteBuffer = _

  private[this] val deser = SerialisationProjectBM.deser

  @Setup
  def setup(): Unit = {
    p = SampleData.`1000`.project
    b = SampleData.`1000`.projectBinary.toByteBuffer
    ()
  }

  @Benchmark
  def read = deser(b)

  @Benchmark
  def write = PickleImpl intoBytes p

}
