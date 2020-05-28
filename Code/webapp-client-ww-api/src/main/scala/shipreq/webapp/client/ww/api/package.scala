package shipreq.webapp.client.ww

import org.scalajs.dom.Console
import scala.scalajs.js

package object api {

  @inline private[api] lazy val console = {
    def g = js.Dynamic.global
    (g.self || g.window).console.asInstanceOf[Console]
  }
}
