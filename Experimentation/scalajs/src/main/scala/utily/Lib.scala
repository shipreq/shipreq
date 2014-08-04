package utily

import org.scalajs.dom
import org.scalajs.dom.HTMLInputElement

import scalaz.StateT
import scalaz.Scalaz.Id
import scalaz.effect.IO
import monocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._

object Lib {
//  type SSetter[S, A] = Setter[S, S, _, A]

  implicit def autoLiftStateIntoIO[S, A](s: StateT[Id, S, A]): StateT[IO, S, A] = s.lift[IO]

  implicit class StateExt[S, A](val u: StateT[Id, S, A]) extends AnyVal {
    def liftIO = autoLiftStateIntoIO(u)
  }

//  implicit final class ComponentScope_SS_Ext3[S](val u: ComponentScope_SS[S]) extends AnyVal {
    //    @inline def setStateL [V](l: Setter[S, S, _, V])(v: V)                    = u.modState((s: S) => l.set(s, v))
    //    @inline def setStateLC[V](l: Setter[S, S, _, V])(v: V)(callback: => Unit) = u.modState((s: S) => l.set(s, v), callback)
//    @inline def setStateL[V](l: Setter[S, S, _, V])(v: V)                    = u.modState(l.set(_, v))
//    @inline def setStateL[V](l: Setter[S, S, _, V], callback: () => Unit)(v: V) = u.modState(l.set(_, v), callback)

    // Using StateT[Id] instead of State so that Intellij doesn't paint the entire screen red
//    @inline def runState(m: StateT[Id, S, _])                    = u.modState(m(_)._1)
//    @inline def runState(m: StateT[Id, S, _], callback: () => Unit) = u.modState(m(_)._1, callback)
//    @inline def runStateC(m: StateT[Id, S, _])(callback: () => Unit) = runState(m, callback())
//  }

  type InputEvent = SyntheticEvent[HTMLInputElement]

  def textChangeRecv(f: String => Unit): InputEvent => Unit = e => f(e.target.value)
//  def textChangeRecvL[State](t: ComponentScope_SS[State], l: Setter[State, State, _, String]) =
//    textChangeRecv(t setStateL l)

  def textChangeRecvIO(f: String => IO[Unit]): InputEvent => IO[Unit] = e => f(e.target.value)

  def textChangeRecvX[R](f: String => R): InputEvent => R = e => f(e.target.value)

  def checkbox(checked: Boolean) =
    input(`type` := "checkbox", checked && (all.checked := "checked"))
}
