package shipreq.webapp.client.project.app

import org.scalajs.dom.webworkers.Worker
import shipreq.webapp.client.ww.api.Protocol.Codec.{default => codec}
import shipreq.webapp.client.ww.api._

object WebWorkerClient {

  type Instance = Client[WebWorkerCmd, codec.Reader]

  def apply(wwJsUrl: String): Instance = {
    lazy val worker = new Worker(wwJsUrl)
    Client.default[WebWorkerCmd](worker)
  }
}
