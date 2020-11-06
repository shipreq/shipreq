package shipreq.benchmark

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.member.project.event.Event
import shipreq.webapp.sampledata.SampleData

object SerialisationBinaryEventsBM {
  import boopickle.DefaultBasic._
  import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
  import shipreq.webapp.member.protocol.binary.v1.Latest._

  val binCodec = implicitly[Pickler[Vector[Event]]].asV1(0).withMagicNumbers(123, 456)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class SerialisationBinaryEventsBM {

  @Param(Array("full", "no_req_codes"))
  var `type`: String = _

  @Param(Array("1000", "2000", "4000", "10000"))
  var events: String = _

  private var readFn: () => Vector[Event] = _
  private var writeFn: () => Any = _

  @Setup
  def setup(): Unit = {
    val d = SampleData.byParams(`type`, events)
    val es = d.events

    import SerialisationBinaryEventsBM._
    val bin = binCodec.encode(es)
    writeFn = () => binCodec.encode(es)
    readFn = () => binCodec.decode(bin).getOrThrow()
  }

  @Benchmark
  def read = readFn()

  @Benchmark
  def write = writeFn()
}
