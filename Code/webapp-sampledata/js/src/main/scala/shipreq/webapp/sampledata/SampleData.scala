package shipreq.webapp.sampledata

import io.circe.Json
import io.circe.parser.parse
import japgolly.microlibs.testutil.TestUtilImplicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Ajax
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.protocol.json.v1.Rev1.decoderEvent

final case class SampleData(name: String, events: Vector[Event]) extends AbstractSampleData(name, events)

object SampleData extends SampleDataManifest[AsyncCallback[SampleData]] {

  private def loadJsonFromResource(name: String): AsyncCallback[Json] =
    Ajax.get(s"http://localhost:19191/$name")
      .send
      .asAsyncCallback
      .map { xhr =>
        val jsonStr = xhr.responseText
        parse(jsonStr).getOrThrow()
      }

  override protected def load(filename: String): AsyncCallback[SampleData] =
    loadJsonFromResource(filename)
      .map(_.as[Vector[Event]].getOrThrow())
      .map(SampleData(filename, _))
      .memo()
}