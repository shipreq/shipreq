package shipreq.benchmark

import io.circe._
import io.circe.parser._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.event.Event

/*
 read: b = j x 3.71
write: b = j x 2.15

golly-xps:
[info] Benchmark                    (format)   Mode  Cnt    Score   Error  Units
[info] SerialisationEventsBM.read       json  thrpt    6  127.757 ± 0.278  ops/s
[info] SerialisationEventsBM.read     binary  thrpt    6  473.928 ± 0.366  ops/s
[info] SerialisationEventsBM.write      json  thrpt    6  144.176 ± 0.527  ops/s
[info] SerialisationEventsBM.write    binary  thrpt    6  310.128 ± 1.701  ops/s
[success] Total time: 488 s, completed 16/09/2019 2:01:31 PM
 */

object SerialisationEventsBM {
  import boopickle.DefaultBasic._
  import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
  import shipreq.webapp.base.protocol.json.v1.Events._
  import shipreq.webapp.base.protocol.binary.v1.Events.picklerEvent

  val events = SampleData.events_1000

  val jsonEnc = Encoder[Vector[Event]]
  val jsonDec = Decoder[Vector[Event]]
  val json = jsonEnc(events).noSpaces

  val binCodec = implicitly[Pickler[Vector[Event]]].asV10.withMagicNumbers(123, 456)
  val bin = binCodec.encode(events)
}

@Warmup(iterations = 6)
@Measurement(iterations = 6)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Benchmark)
class SerialisationEventsBM {

  @Param(Array("json", "binary"))
  var format: String = _

  var readFn: () => Vector[Event] = _
  var writeFn: () => Any = _

  @Setup
  def setup(): Unit = {
    import SerialisationEventsBM._
    format match {
      case "json" =>
        writeFn = () => jsonEnc(events).noSpaces
        readFn = () => decode(json)(jsonDec).needRight
      case "binary" =>
        writeFn = () => binCodec.encode(events)
        readFn = () => binCodec.decode(bin).needRight
    }
  }

  @Benchmark
  def read = readFn()

  @Benchmark
  def write = writeFn()

}
