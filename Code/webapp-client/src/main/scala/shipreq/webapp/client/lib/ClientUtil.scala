package shipreq.webapp.client.lib

import scalaz.effect.IO

object ClientUtil {

  private[this] var GLOBAL_VAR = 0

  val uniqueInt = IO[Int] {
    // JS is single-threaded
    GLOBAL_VAR += 1
    GLOBAL_VAR
  }

  val uniqueStr: IO[String] =
    uniqueInt.map(i => s"___uqs$i")
}
