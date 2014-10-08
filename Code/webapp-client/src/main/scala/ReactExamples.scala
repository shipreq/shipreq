package hahaa

import shipreq.webapp.client.ClientData
import shipreq.webapp.shared.protocol.Routines

import scala.scalajs.js
import org.scalajs.dom.{document, window, Node, console, alert}
import scala.scalajs.js.annotation.{JSExport, JSName}
import scalaz.effect.IO

object ReactExamples {

  def main(routines: Routines.ForCfgReqType) = IO[Unit] {
    example1(document getElementById "eg1")
    //    example2(document getElementById "eg2")
    cfgReqTypesTest(routines, document getElementById "eg2")
  }

  // ===================================================================================================================

  import japgolly.scalajs.react._
  import vdom.ReactVDom._
  import all._

  def example1(mountNode: Node) = {

    val HelloMessage = ReactComponentB[String]("HelloMessage")
      .render(name => div("Hello ", name))
      .build

    React.renderComponent(HelloMessage("John"), mountNode)
  }

  // ===================================================================================================================

  def cfgReqTypesTest(routines: Routines.ForCfgReqType, mountNode: Node) = {
    import shipreq.webapp.client.CfgReqType._
    Component(Props((routines, ClientData.GLOBAL), false)) render mountNode
  }

  def example2(mountNode: Node) = {

    case class State(secondsElapsed: Long)

    class Backend {
      var interval: js.UndefOr[Int] = js.undefined
      def tick(scope: ComponentScopeM[_, State, _]): js.Function =
        () => scope.modState(s => State(s.secondsElapsed + 1))
    }

    val Timer = ReactComponentB[Unit]("Timer")
      .initialState(State(0))
      .backend(_ => new Backend)
      .render((_,S,_) => div("Seconds elapsed: ", S.secondsElapsed))
      .componentDidMount(scope =>
      scope.backend.interval = window.setInterval(scope.backend.tick(scope), 1000))
      .componentWillUnmount(_.backend.interval foreach window.clearInterval)
      .buildU

    React.renderComponent(Timer(), mountNode)
  }

}
