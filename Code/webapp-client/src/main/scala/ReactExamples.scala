package hahaa

import shipreq.webapp.client.lib.{Patches, LiftAjax}
import shipreq.webapp.shared.{ExampleData, Interface}

import scala.scalajs.js
import org.scalajs.dom.{document, window, Node, console, alert}

import scala.scalajs.js.annotation.{JSExport, JSName}

object ReactExamples extends js.JSApp {

  override def main(): Unit = {
    example1(document getElementById "eg1")
//    example2(document getElementById "eg2")
    cfgReqTypesTest(document getElementById "eg2")
  }

  @JSExport
  def wired(a: String) = {
    import upickle._
    read[Interface.Page.WIP](a)
  }

  @JSExport
  def invokeSquare(p: Interface.Page.WIP, n: js.Number): Unit =
    invokeCallback(p.square)(n.toInt, s => alert(s"RESPONSE: [$s]"))

  @JSExport
  def invokeGrrr(p: Interface.Page.WIP, n: js.Number): Unit =
    invokeCallback(p.grrr)(ExampleData(n.toInt), s => alert(s.yar))

  def invokeCallback[D <: Interface.Defn](r: Interface.Wired[D])(i: r.d.I, cb: r.d.O => Unit)(implicit I: upickle.Writer[r.d.I], O: upickle.Reader[r.d.O]): Unit = {
    // TODO test all failure scenarios imaginable
    val ii = js.encodeURIComponent(upickle write i)
    val s: js.Any => Unit = o => {
      console.log("invokeCallback result", o)
      val oo = upickle.readJs[r.d.O](Patches readJs o)
      cb(oo)
//      cb(o.asInstanceOf[r.d.O])
      // TODO
    }
    // needs failure
    LiftAjax.lift_ajaxHandler(s"${r.n}=$ii", s, null, "json")
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
    ReqTypeTableCompOuter(ReqTypeTableProps(List(
      CustReqType(Id(1), "CO", Set.empty, "Constraint", ImplicationNotRequired, Alive),
      CustReqType(Id(2), "MF", Set.empty, "Major Feature", ImplicationNotRequired, Alive),
      CustReqType(Id(3), "FR", Set.empty, "Functional Requirement", ImplicationNotRequired, Alive),
      CustReqType(Id(4), "BR", Set.empty, "Business Rule", ImplicationNotRequired, Alive),
      CustReqType(Id(5), "DD", Set("DA", "DDF"), "Data Definition", ImplicationNotRequired, Dead),
      CustReqType(Id(6), "SI", Set.empty, "Solution Idea", ImplicationRequired, Dead)
    ), false)) render mountNode
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
