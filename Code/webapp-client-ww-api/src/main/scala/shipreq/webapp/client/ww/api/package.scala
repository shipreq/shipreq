package shipreq.webapp.client.ww

import org.scalajs.dom.Console
import scala.scalajs.js

package object api {

  @inline private[api] lazy val console =
    (js.Dynamic.global.self || js.Dynamic.global.window).console.asInstanceOf[Console]
}
