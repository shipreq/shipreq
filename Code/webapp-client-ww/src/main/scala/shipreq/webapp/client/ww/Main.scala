package shipreq.webapp.client.ww

import scalajs.js.annotation._
import shipreq.webapp.client.ww.api._
import Server.codec.Writer

/**
 * Initialises the WebWorker thread.
 *
 * @since 25/05/2016
 */
@JSExport("Main")
object Main {

  @JSExport
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
        case GraphUseCaseStepFlow(a, b)          => Graphs.useCaseStepFlow(a, b).toSVG
        case GraphReqImplications(a, b, c, d, e) => Graphs.implicationFocused(a, b, c, d, e).toSVG
      }
  }
}


