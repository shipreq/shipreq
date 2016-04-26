package shipreq.webapp.client.app

import japgolly.scalajs.react.Callback
import org.scalajs.dom.webworkers.Worker
import scala.concurrent.ExecutionContext.Implicits.global
import shipreq.webapp.base.AppConsts
import shipreq.webapp.client.ww.api._
import Client.codec.Reader

object WebWorkerClient {

  val worker = new Worker(AppConsts.assetPath_/ + "ww.js")

  val client = Client[Cmd](worker)

  def postCB[R: Reader](cmd: Cmd[R])(use: R => Callback): Callback =
    Callback future client.post(cmd).map(use)
}
