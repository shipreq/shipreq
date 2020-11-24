package shipreq.webapp.client.ww

import boopickle.DefaultBasic.unitPickler
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.client.ww.api._
import shipreq.webapp.client.ww.state.WorkerState
import shipreq.webapp.member.protocol.webworker._

/**
 * Initialises the WebWorker thread.
 *
 * @since 25/05/2016
 */
object Main {

  val protocol = WebWorkerProtocol.default

  def main(args: Array[String]): Unit = {
    val logger       = LoggerJs.devOnly
    val onError      = OnError.logToConsole
    val worker       = AbstractWebWorker.Server()
    val state        = new WorkerState(WorkerState.Logic.Real, logger)
    val serviceMaker = Service.maker(state)

    val start =
      ManagedWebWorker.Server.start(
        worker,
        protocol)(
        serviceMaker,
        ResponseEncoder,
        onError,
        logger)

    start.runNow()
  }

  object ResponseEncoder extends ManagedWebWorker.Server.ResponseEncoder[protocol.Writer, WebWorkerCmd] {
    override def apply[A](req: WebWorkerCmd[A]) =
      req.resultPickler
  }
}
