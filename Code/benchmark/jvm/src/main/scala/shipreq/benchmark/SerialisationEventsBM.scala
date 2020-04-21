package shipreq.benchmark

import io.circe._
import io.circe.parser._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.event.Event
import shipreq.webapp.sampledata.SampleData

object SerialisationEventsBM {
  import boopickle.DefaultBasic._
  import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
  import shipreq.webapp.base.protocol.binary.v1.Rev1._
  import shipreq.webapp.base.protocol.json.v1.Rev1._

  val jsonEnc = Encoder[Vector[Event]]
  val jsonDec = Decoder[Vector[Event]]

  val binCodec = implicitly[Pickler[Vector[Event]]].asV1(0).withMagicNumbers(123, 456)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class SerialisationEventsBM {

  @Param(Array("1000", "2000"))
  var events: String = _

  @Param(Array("json", "binary"))
  var format: String = _

  var readFn: () => Vector[Event] = _
  var writeFn: () => Any = _

  @Setup
  def setup(): Unit = {
    val d = SampleData.byName(events)
    val es = d.events

    import SerialisationEventsBM._
    format match {
      case "json" =>
        val json = d.eventJsonStr
        writeFn = () => jsonEnc(es).noSpaces
        readFn = () => decode(json)(jsonDec).getOrThrow()
      case "binary" =>
        val bin = binCodec.encode(es)
        writeFn = () => binCodec.encode(es)
        readFn = () => binCodec.decode(bin).getOrThrow()
    }
  }

  @Benchmark
  def read = readFn()

  @Benchmark
  def write = writeFn()

}
