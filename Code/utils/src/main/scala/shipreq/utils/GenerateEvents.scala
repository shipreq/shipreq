package shipreq.utils

import io.circe.syntax._
import japgolly.microlibs.stdlib_ext.ParseInt
import japgolly.microlibs.utils.FileUtils
import java.time.Instant
import shipreq.utils.UtilUtils._
import shipreq.webapp.base.event.{RandomEventStream, RandomEventStreamConfig}
import shipreq.webapp.base.protocol.json.v1.Latest._
import shipreq.webapp.base.test.RandomDataSettings

object GenerateEvents {

  def main(argsA: Array[String]): Unit = {
    val args = argsA.toVector


    def die(): Nothing = {
      System.err.println("Usage: this <size> <full|no_req_codes>")
      System.exit(1)
      ???
    }

    val qty: Int =
      args.lift(0) match {
        case Some(ParseInt(i)) => i
        case _                 => die()
      }

    val `type` = args.lift(1).getOrElse(die())

    val config: RandomEventStreamConfig =
      `type` match {
        case "full"         => RandomEventStreamConfig.default.copy(reqCodeEvents = true)
        case "no_req_codes" => RandomEventStreamConfig.default.copy(reqCodeEvents = false)
        case _              => die()
      }

    RandomDataSettings.disableUnicode = true

    val events =
      logTime(s"Generating ${`type`} $qty events...") {
        RandomEventStream.withConfig(config).justEntireEventStream(qty).sample().take(qty).map(_.event)
      }

    val json =
      events
        .iterator
        .map(_.asJson.noSpacesSortKeys)
        .mkString("[", "\n,", "\n]")

    val filename = s"/tmp/sampledata-${`type`}-$qty-${Instant.now().toString.filter(_.isDigit)}.json"
    println(s"Writing to $filename")
    FileUtils.write(filename, json)

    println("Done")
  }

}
