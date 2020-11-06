package shipreq.benchmark

import io.circe._
import io.circe.parser._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.member.project.event.Event
import shipreq.webapp.sampledata.SampleData

object SerialisationJsonEventsBM {
  import shipreq.webapp.member.protocol.json.v1.Latest._

  val jsonEnc = Encoder[Vector[Event]]
  val jsonDec = Decoder[Vector[Event]]
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class SerialisationJsonEventsBM {

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

    import SerialisationJsonEventsBM._
    val json = d.eventJsonStr
    writeFn = () => jsonEnc(es).noSpaces
    readFn = () => decode(json)(jsonDec).getOrThrow()
  }

  @Benchmark
  def read = readFn()

  @Benchmark
  def write = writeFn()
}
