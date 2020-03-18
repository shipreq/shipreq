package shipreq.benchmark

import io.circe.Json
import io.circe.parser.parse
import scala.io.{Codec, Source}
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{Event, VerifiedEvent}
import shipreq.webapp.base.protocol.json.v1.Events.decoderEvent
import shipreq.webapp.base.protocol.json.v1.PostEvents.decoderVerifiedEvent
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.benchmark.data._

object SampleData {

  private def loadJsonFromResource(name: String): Json = {
    val jsonStr = Source.fromResource(name)(Codec("UTF-8")).mkString
    parse(jsonStr) match {
      case Right(j) => j
      case Left(e)  => throw e
    }
  }

  def project_100: Project =
    Project_100.project

  lazy val events_1000: Vector[Event] =
    loadJsonFromResource("shipreq-events-1000.json")
    .as[Vector[Event]]
    .needRight

  lazy val project3Events: Vector[VerifiedEvent] =
    loadJsonFromResource("project-3.json")
      .as[Vector[VerifiedEvent]]
      .needRight
}
