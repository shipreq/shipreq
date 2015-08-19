package shipreq.webapp.client.lib

import japgolly.scalajs.react.CallbackTo

object ClientUtil {

  private[this] var GLOBAL_VAR = 0

  val uniqueInt = CallbackTo[Int] {
    // JS is single-threaded
    GLOBAL_VAR += 1
    GLOBAL_VAR
  }

  val uniqueStr: CallbackTo[String] =
    uniqueInt.map(i => s"___uqs$i")
}
