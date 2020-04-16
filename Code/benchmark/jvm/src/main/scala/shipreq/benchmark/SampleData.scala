package shipreq.benchmark

import boopickle.PickleImpl
import io.circe.Json
import io.circe.parser.parse
import java.time.Instant
import scala.io.{Codec, Source}
import shipreq.base.util.BinaryData
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{Event, EventOrd, VerifiedEvent}
import shipreq.webapp.base.protocol.json.v1.Rev1.decoderEvent
import shipreq.webapp.base.protocol.binary.v1.Rev1.picklerProject

final case class SampleData(events: Vector[Event]) {

  lazy val verifiedEvents: VerifiedEvent.Seq =
    SampleData.verifyEvents(events)

  lazy val project: Project =
    applyEventsSuccessfully(Project.empty, events: _*)

  lazy val projectBinary: BinaryData =
    BinaryData.unsafeFromByteBuffer(PickleImpl intoBytes project)
}

object SampleData {

  lazy val `1000`  = load("shipreq-events-1000.json")
  lazy val `10000` = load("shipreq-events-10000.json")

  // ===================================================================================================================

  private def loadJsonFromResource(name: String): Json = {
    val jsonStr = Source.fromResource(name)(Codec("UTF-8")).mkString
    parse(jsonStr) match {
      case Right(j) => j
      case Left(e)  => throw e
    }
  }

  private val startTime = Instant.parse("2020-04-16T00:00:00Z")

  private[SampleData] def verifyEvents(es: Vector[Event]): VerifiedEvent.Seq =
    VerifiedEvent.Seq.empty ++ es.indices.iterator.map { i =>
      val e = es(i)
      VerifiedEvent(EventOrd(i), e, startTime.plusSeconds(i))
    }

  private def load(filename: String): SampleData = {
    val events: Vector[Event] =
      loadJsonFromResource(filename)
        .as[Vector[Event]]
        .getOrThrow()
    apply(events)
  }

}
