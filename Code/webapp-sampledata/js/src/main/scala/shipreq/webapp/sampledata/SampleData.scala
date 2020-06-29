package shipreq.webapp.sampledata

import io.circe.Json
import io.circe.parser.parse
import japgolly.microlibs.testutil.TestUtilImplicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Ajax
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.protocol.json.v1.Latest.decoderEvent

final case class SampleData(meta: SampleDataMeta, events: Vector[Event]) extends AbstractSampleData(meta, events) {
  override val hashCode = meta.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case x: SampleData => x.meta == meta
    case _ => false
  }
}

object SampleData extends SampleDataManifest[AsyncCallback[SampleData]] {

  private def loadJsonFromResource(name: String): AsyncCallback[Json] =
    Ajax.get(s"http://localhost:19191/$name")
      .send
      .asAsyncCallback
      .map { xhr =>
        val jsonStr = xhr.responseText
        parse(jsonStr).getOrThrow()
      }

  override protected def load(meta: SampleDataMeta): AsyncCallback[SampleData] =
    loadJsonFromResource(meta.filename)
      .map(_.as[Vector[Event]].getOrThrow())
      .map(SampleData(meta, _))
      .memo()
}