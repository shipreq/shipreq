package golly

import scala.scalajs.js

import japgolly.scalajs.react._
import vdom.ReactVDom._
import all._
import ScalazReact._

import scala.scalajs.js.annotation.JSExportAll
import org.scalajs.dom.{HTMLInputElement, console, document, window, Node}

import scalaz.effect.IO

@JSExportAll
object Golly extends js.JSApp {

  trait OnUnmount {
    private var unmountProcs: List[() => Unit] = Nil
    final def closeResources(): Unit = {
      unmountProcs foreach (_())
      unmountProcs = Nil
    }
    final def onUnmount(f: => Unit): Unit = unmountProcs ::= (() => f)
    final def onUnmountF(f: () => Unit): Unit = unmountProcs ::= f
  }

  object OnUnmount {
    def install[P, S, B <: OnUnmount] =
      (_: ReactComponentB[P, S, B]).componentWillUnmount(_.backend.closeResources())
  }

  // ---------------------------------------------------------------------------------

  trait Listenable[A] {
    def register(f: A => Unit): () => Unit
  }

//  trait Listener[A] {
//    var unregister: Option[() => Unit] = None
//  }

  def installListener[P, S, B <: OnUnmount, A](f: P => Listenable[A], g: ComponentScopeM[P, S, B] => A => Unit) =
    (_: ReactComponentB[P, S, B]).componentDidMount(scope =>
      scope.backend onUnmountF f(scope.props).register(g(scope)))

  def installListenerS[P, S, B <: OnUnmount, A](f: P => Listenable[A], g: A => ReactS[S, Unit]) =
    installListener[P, S, B, A](f, t => a => t.runState(g(a)).unsafePerformIO())

  object Yay extends Listenable[String] {
    private var listeners = List.empty[String => Unit]

    override def register(f: String => Unit) = {
      listeners ::= f
      () => listeners = listeners.filterNot(_ == f)
    }

    def broadcast(a: String): Unit = listeners foreach (_(a))
  }

  def broadcast(a: String): Unit = Yay.broadcast(a)

  // ---------------------------------------------------------------------------------

  case class State(secondsElapsed: Long)

  class Backend extends OnUnmount {
//    var interval: js.UndefOr[Int] = js.undefined
    def tick(scope: ComponentScopeM[_, State, _]): js.Function =
      () => scope.modState(s => State(s.secondsElapsed + 1))
  }

  /*
   * This is the callback logic for an external event. Static. Testable in isolation.
   */
  def recvExtDataS(str: String) = ReactS.mod[State](s => {
    val i = str.replaceAll("[^0-9]","").toInt
    State(s.secondsElapsed + i)
  })

  val Timer = ReactComponentB[String]("Timer")
    .initialState(State(0))
    .backend(_ => new Backend)
    .render((P,S,_) => div(p("Prop: ",P), p("Seconds elapsed: ", S.secondsElapsed)))
    .componentDidMount(scope => {
      //scope.backend.interval = window.setInterval(scope.backend.tick(scope), 1000)
      val i = window.setInterval(scope.backend.tick(scope), 1000)
      scope.backend onUnmount window.clearInterval(i)
    })
//    .componentWillUnmount(_.backend.interval foreach window.clearInterval)
    .configure(installListenerS((_:String) => Yay, recvExtDataS), OnUnmount.install)
    .create

  def main(): Unit = {
    React.renderComponent(Timer("init"), document getElementById "target1")
    React.renderComponent(Timer("init2"), document getElementById "target2")
    println("TRY THIS: golly.Golly().broadcast('Add 100')")
  }

}
