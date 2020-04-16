package shipreq.utils

import io.circe.syntax._
import japgolly.microlibs.stdlib_ext.ParseInt
import japgolly.microlibs.utils.FileUtils
import java.time.Instant
import shipreq.webapp.base.event.RandomEventStream
import shipreq.webapp.base.protocol.json.v1.Rev1._
import shipreq.utils.UtilUtils._
import shipreq.webapp.base.RandomDataSettings

object GenerateEvents {

  def main(args: Array[String]): Unit = {
    val qty: Int =
      args.headOption match {
        case Some(ParseInt(i)) => i
        case _ =>
          System.err.println("Size required as arg.")
          System.exit(1)
          0
      }

    RandomDataSettings.disableUnicode = true

    val events =
      logTime(s"Generating $qty events...") {
        RandomEventStream.justEntireEventStream(qty).sample().take(qty).map(_.event)
      }

    val json =
      events
        .iterator
        .map(_.asJson.noSpacesSortKeys)
        .mkString("[", "\n,", "\n]")

    val filename = s"/tmp/shipreq-events-$qty-${Instant.now().toString.filter(_.isDigit)}.json"
    println(s"Writing to $filename")
    FileUtils.write(filename, json)

    println("Done")
  }

}
