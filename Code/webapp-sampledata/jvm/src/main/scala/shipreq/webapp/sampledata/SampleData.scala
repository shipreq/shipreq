package shipreq.webapp.sampledata

import io.circe.Json
import io.circe.parser.parse
import japgolly.microlibs.testutil.TestUtilImplicits._
import scala.io.{Codec, Source}
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.protocol.json.v1.Rev1.decoderEvent

final case class SampleData(meta: SampleDataMeta, events: Vector[Event]) extends AbstractSampleData(meta, events)

object SampleData extends SampleDataManifest[SampleData] {

  private def loadJsonFromResource(name: String): Json = {
    val jsonStr = Source.fromResource(name)(Codec("UTF-8")).mkString
    parse(jsonStr).getOrThrow()
  }

  override protected def load(meta: SampleDataMeta): SampleData = {
    val events: Vector[Event] =
      loadJsonFromResource(meta.filename)
        .as[Vector[Event]]
        .getOrThrow()

    SampleData(meta, events)
  }

  lazy val errors: List[String] =
    all.iterator.flatMap(_.errors).toList

  def assertAllValid(): Unit = {
    if (errors.nonEmpty)
      throw new AssertionError(errors.mkString("\n"))
  }
}