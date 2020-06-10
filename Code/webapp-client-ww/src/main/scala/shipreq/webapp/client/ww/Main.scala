package shipreq.webapp.client.ww

import shipreq.webapp.client.ww.api.Protocol.Codec.default.Writer
import shipreq.webapp.client.ww.api._

/**
 * Initialises the WebWorker thread.
 *
 * @since 25/05/2016
 */
object Main {

  def main(args: Array[String]): Unit = {
    GraphViz.init()
    Server.startDefault(Service, ResultEncoder).runNow()
  }

  object ResultEncoder extends Server.ResultEncoder[WebWorkerCmd, Writer] {
    override def apply[A](cmd: WebWorkerCmd[A]): Writer[A] =
      cmd.resultPickler
  }
}
