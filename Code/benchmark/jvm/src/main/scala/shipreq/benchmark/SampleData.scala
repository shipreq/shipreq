package shipreq.benchmark

import io.circe.Json
import io.circe.parser.parse
import scala.io.{Codec, Source}
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.protocol.json.v1.Rev1.decoderEvent
import shipreq.webapp.base.test.WebappTestUtil._

final case class SampleData(name: String, events: Vector[Event]) extends AbstractSampleData(name, events)

object SampleData extends SampleDataManifest[SampleData] {

  private def loadJsonFromResource(name: String): Json = {
    val jsonStr = Source.fromResource(name)(Codec("UTF-8")).mkString
    parse(jsonStr).getOrThrow()
  }

  override protected def load(filename: String): SampleData = {
    val events: Vector[Event] =
      loadJsonFromResource(filename)
        .as[Vector[Event]]
        .getOrThrow()

    SampleData(filename, events)
  }
}