package golly

import japgolly.scalajs.react.{SyntheticEvent, ComponentScope_SS}
import org.scalajs.dom
import scalaz._

object ScalazReact {
  implicit final class ComponentScope_SS_Ext2[State](val u: ComponentScope_SS[State]) extends AnyVal {
    @inline def setStateL[V](l: LensFamily[State, State, _, V])(v: V) =
      u.modState(l.set(_, v))
    @inline def setStateLC[V](l: LensFamily[State, State, _, V])(v: V)(callback: => Unit) =
      u.modState((s: State) => l.set(s, v), callback)
  }

  def nonEmptyStringLens[A](l: Lens[A, Option[String]]): Lens[A, String] =
    l.xmapB(_ getOrElse "")((i: String) => {
      val j = i.trim
      if (j.isEmpty) None else Some(j)
    })

  def textChangeRecv(f: String => Unit): SyntheticEvent[dom.HTMLInputElement] => Unit = e => f(e.target.value)
  def textChangeRecvL[State](t: ComponentScope_SS[State], l: Lens[State, String]) = textChangeRecv(t setStateL l)
}
