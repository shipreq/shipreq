package hahaa

import shipreq.webapp.client.lib.{InterfaceClient, LiftAjax}
import shipreq.webapp.shared.rpc._
import InterfaceClient._

import scala.scalajs.js
import org.scalajs.dom.{document, window, Node, console, alert}
import scala.scalajs.js.annotation.{JSExport, JSName}

object ReactExamples {

  def main(r: Interfaces.WIP): Unit = {
    example1(document getElementById "eg1")
    //    example2(document getElementById "eg2")
    cfgReqTypesTest(document getElementById "eg2")

    invokeCallback(r.square)(123, s => alert(s"RESPONSE: [$s]"))
    invokeCallback(r.grrr)(ExampleData(666), s => alert(s.yar))
  }

  // ===================================================================================================================

  import japgolly.scalajs.react._
  import vdom.ReactVDom._
  import all._

  def example1(mountNode: Node) = {

    val HelloMessage = ReactComponentB[String]("HelloMessage")
      .render(name => div("Hello ", name))
      .create

    React.renderComponent(HelloMessage("John"), mountNode)
  }

  // ===================================================================================================================

  def cfgReqTypesTest(mountNode: Node) = {
    import shipreq.webapp.shared.data._
    import shipreq.webapp.client.CfgReqType._
    import CustReqType.Id
    implicit def autoMnemonic(s: String) = ReqType.Mnemonic(s)

    val list = List(
      CustReqType(Id(1), "CO", Set.empty, "Constraint", ImplicationNotRequired, Alive),
      CustReqType(Id(2), "MF", Set.empty, "Major Feature", ImplicationNotRequired, Alive),
      CustReqType(Id(3), "FR", Set.empty, "Functional Requirement", ImplicationNotRequired, Alive),
      CustReqType(Id(4), "BR", Set.empty, "Business Rule", ImplicationNotRequired, Alive),
      CustReqType(Id(5), "DD", Set("DA", "DDF"), "Data Definition", ImplicationNotRequired, Dead),
      CustReqType(Id(6), "SI", Set.empty, "Solution Idea", ImplicationRequired, Dead)
    )

    val map = list.map(i => i.id -> i).toMap

    Component(Props(map, false)) render mountNode
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
      .createU

    React.renderComponent(Timer(), mountNode)
  }

}
