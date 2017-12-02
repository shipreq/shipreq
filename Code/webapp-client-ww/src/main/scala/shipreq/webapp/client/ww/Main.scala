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

  object ResultEncoder extends ResultEncoder[Cmd, Writer] {
    override def apply[R](cmd: Cmd[R]): Writer[R] =
      cmd.resultPickler
  }

  object Handler extends Handler[Cmd] {
    import Cmd._
    override def apply[R](cmd: Cmd[R]): R =
      cmd match {
        case GraphUseCaseStepFlow(a, b, c)       => Graphs.useCaseStepFlow(a, b, c).toSvg
        case GraphAllImplications(a, b, c, d)    => Graphs.implicationAll(a, b, c, d).toSvg
        case GraphReqImplications(a, b, c, d, e) => Graphs.implicationFocused(a, b, c, d, e).toSvg
      }
  }
}


