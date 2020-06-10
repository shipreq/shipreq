package shipreq.webapp.client.project.app

import org.scalajs.dom.webworkers.Worker
import shipreq.webapp.base.AssetManifest
import shipreq.webapp.client.ww.api._
import shipreq.webapp.client.ww.api.Protocol.Codec.{default => codec}

object WebWorkerClient {

  type Instance = Client[WebWorkerCmd, codec.Reader]

  val Instance: Instance = {
    lazy val worker = new Worker(AssetManifest.webappClientWwJs)
    Client.default[WebWorkerCmd](worker)
  }
}
