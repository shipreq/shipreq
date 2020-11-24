package shipreq.webapp.client.ww

import japgolly.scalajs.react.Callback
import org.scalajs.dom.webworkers.DedicatedWorkerGlobalScope
import scala.scalajs.js

object WebWorkerUtil {

  def importScripts(urls: String*): Callback = {
    val urlArray = js.Array(urls: _*)
    Callback {
      DedicatedWorkerGlobalScope.self.importScripts(urlArray)
    }
  }

  def importScriptList(urls: List[String]): Callback =
    importScripts(urls: _*)

}
