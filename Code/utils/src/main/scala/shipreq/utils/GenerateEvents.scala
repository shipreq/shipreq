package shipreq.utils

import io.circe.syntax._
import japgolly.microlibs.stdlib_ext.ParseInt
import japgolly.microlibs.utils.FileUtils
import java.time.Instant
import shipreq.webapp.base.event.{RandomEventStream, RandomEventStreamConfig}
import shipreq.webapp.base.protocol.json.v1.Rev1._
import shipreq.utils.UtilUtils._
import shipreq.webapp.base.RandomDataSettings

object GenerateEvents {

  def main(argsA: Array[String]): Unit = {
    val args = argsA.toVector


    def die(): Nothing = {
      System.err.println("Usage: this <size> [all|no-req-codes]")
      System.exit(1)
      ???
    }

    val qty: Int =
      args.lift(0) match {
        case Some(ParseInt(i)) => i
        case _                 => die()
      }

    val config: RandomEventStreamConfig =
      args.lift(1) match {
        case Some("all"         ) => RandomEventStreamConfig.default.copy(reqCodeEvents = true)
        case Some("no-req-codes") => RandomEventStreamConfig.default.copy(reqCodeEvents = false)
        case _                    => die()
      }

    RandomDataSettings.disableUnicode = true

    val events =
      logTime(s"Generating $qty events...") {
        RandomEventStream.withConfig(config).justEntireEventStream(qty).sample().take(qty).map(_.event)
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
