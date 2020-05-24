package shipreq.webapp.client.project.app

import japgolly.scalajs.react._
import org.scalajs.dom.webworkers.Worker
import shipreq.webapp.base.AssetManifest
import shipreq.webapp.client.ww.api.Client.codec.Reader
import shipreq.webapp.client.ww.api._

trait WebWorkerClient extends Client[WebWorkerCmd, Reader]

object WebWorkerClient {

  val Instance: WebWorkerClient = {
    lazy val worker = new Worker(AssetManifest.webappClientWwJs)
    lazy val client = Client[WebWorkerCmd](worker)
    new WebWorkerClient {
      override def post[A](cmd: WebWorkerCmd[A])(implicit readResult: Reader[A]) =
        client.post(cmd)
    }
  }

  implicit def reuse: Reusability[WebWorkerClient] =
    Reusability.byRef
}
