package shipreq.webapp.client.project.app

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.Reusability
import org.scalajs.dom.webworkers.Worker
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.client.ww.api._
import Client.codec.Reader

trait WebWorkerClient extends Client[Cmd, Reader] {
  def postCB[R: Reader](cmd: Cmd[R])(use: R => Callback): Callback =
    Callback future post(cmd).map(use)
}

object WebWorkerClient {

  val Instance: WebWorkerClient = {
    lazy val worker = new Worker(WebappConfig.assetPath_/ + "ww.js")
    lazy val client = Client[Cmd](worker)
    new WebWorkerClient {
      override def post[A](cmd: Cmd[A])(implicit readResult: Reader[A]): Future[A] =
        client.post(cmd)
    }
  }

  implicit def reuse: Reusability[WebWorkerClient] =
    Reusability.byRef
}
