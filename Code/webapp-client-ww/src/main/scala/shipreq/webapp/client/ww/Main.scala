package shipreq.webapp.client.ww

import shipreq.webapp.client.ww.api._
import Server.codec.Writer

/**
 * Initialises the WebWorker thread.
 *
 * @since 25/05/2016
 */
object Main {

  def main(): Unit = {
    Server(Handler)(ResultEncoder)
  }

  object ResultEncoder extends ResultEncoder[WebWorkerCmd, Writer] {
    override def apply[R](cmd: WebWorkerCmd[R]): Writer[R] =
      cmd.resultPickler
  }

  object Handler extends Handler[WebWorkerCmd] {
    import WebWorkerCmd._
    override def apply[R](cmd: WebWorkerCmd[R]): R =
      cmd match {
        case GraphUseCaseStepFlow(a, b, c)       => Graphs.useCaseStepFlow(a, b, c).toSvg
        case GraphReqImplications(a, b, c, d, e) => Graphs.implicationFocused(a, b, c, d, e).toSvg
        case a: GraphAllImplications             => Graphs.implicationAll(a).toSvg
      }
  }
}


