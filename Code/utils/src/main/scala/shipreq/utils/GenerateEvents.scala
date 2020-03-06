package shipreq.utils

import io.circe.syntax._
import java.time.Instant
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.event.RandomEventStream
import shipreq.webapp.base.protocol.json.v1.Events._
import shipreq.webapp.base.protocol.json.v1.Rev1._
import shipreq.utils.UtilUtils._

object GenerateEvents {

  val qty = 1000

  def main(args: Array[String]): Unit = {

    val events =
      logTime(s"Generating $qty events...") {
        RandomEventStream.justEntireEventStream(qty).sample().take(qty).map(_.event)
      }

    val json =
      events
        .iterator
        .map(_.asJson.noSpacesSortKeys)
        .mkString("[", "\n,", "\n]")

    val filename = s"/tmp/shipreq-events-${Instant.now().toString.filter(_.isDigit)}.json"
    println(s"Writing to $filename")
    writeFile(filename, json)

    println("Done")
  }

}
