package shipreq.benchmark

import io.circe.Json
import io.circe.parser.parse
import scala.io.{Codec, Source}
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.protocol.json.v1.Rev1.decoderEvent
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
}
