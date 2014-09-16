package hahaa

import scala.scalajs.js
import org.scalajs.dom.{document, window, Node}

import japgolly.scalajs.react._
import vdom.ReactVDom._
import all._

object ReactExamples extends js.JSApp {

  override def main(): Unit = {
    example1(document getElementById "eg1")
    example2(document getElementById "eg2")
  }

  // ===================================================================================================================

  def example1(mountNode: Node) = {

    val HelloMessage = ReactComponentB[String]("HelloMessage")
      .render(name => div("Hello ", name))
      .create

    React.renderComponent(HelloMessage("John"), mountNode)
  }

  // ===================================================================================================================

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
