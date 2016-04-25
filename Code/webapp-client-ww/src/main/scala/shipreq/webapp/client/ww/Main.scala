package shipreq.webapp.client.ww

import scalajs.js.annotation._
import shipreq.webapp.client.ww.api._
import Server.codec.Writer

@JSExport("Main")
object Main {

  @JSExport
  def main(): Unit = {
    /*
    val WW = DedicatedWorkerGlobalScope.self

    WW.onmessage = (e: MessageEvent) => {
//      val m = new Message(666, e.data)

      WW.postMessage(s"Received: [${e.data}]")
//      WW.postMessage(m)
    }
*/

  }

  /*
  object ResultEncoder extends ResultEncoder[Cmd, Writer] {
    override def apply(cmd: Cmd)(result: cmd.Result): Writer[cmd.Result] =
      cmd match {
        // How fucking annoying
        // http://stackoverflow.com/questions/36835402/pattern-matching-existential-type
        case c: Cmd.Aux[_] => c.asInstanceOf[Cmd.Aux[cmd.Result]].resultPickler
      }
  }

  object Handler extends Handler[Cmd] {
    override def apply(cmd: Cmd): cmd.Result =
      cmd match {
        case Cmd.GraphUseCaseSteps(_, _) => SVG("qwe")
      }

    def apply2[R](cmd: Cmd.Aux[R]): R =
      cmd match {
        case Cmd.GraphUseCaseSteps(_, _) => SVG("qwe")
      }
  }
  */

  object ResultEncoder extends ResultEncoder[Cmd, Writer] {
    override def apply[R](cmd: Cmd[R]): Writer[R] =
      cmd.resultPickler
  }

  object Handler extends Handler[Cmd] {
    override def apply[R](cmd: Cmd[R]): R =
      cmd match {
        case Cmd.GraphUseCaseSteps(_, _) => SVG("qwe")
      }
  }

  /*
  object ResultEncoder extends ResultEncoder[Cmd, Writer] {
    override def apply[R](cmd: Cmd[R]): Writer[R] =
      cmd match {
        case Cmd.GraphUseCaseSteps(_, _) => SVG.pickle
      }
  }

  object Handler extends Handler[Cmd] {
    override def apply[R](cmd: Cmd[R]): R =
      cmd match {
        case Cmd.GraphUseCaseSteps(_, _) => SVG("qwe")
      }
  }
  */


  Server.apply(Handler)(ResultEncoder)

}


