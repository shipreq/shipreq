package shipreq.webapp.client

import org.scalajs.dom.Console
import org.scalajs.dom.webworkers.DedicatedWorkerGlobalScope
import scala.scalajs.js

package object ww {

  @inline private[ww] def console =
    DedicatedWorkerGlobalScope.self.asInstanceOf[js.Dynamic].console.asInstanceOf[Console]

}
