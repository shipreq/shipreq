package shipreq.base.util

import scala.scalajs.js

final class VarJs[A](val get: () => A, val set: A => Unit) { self =>
  def xmap[B](f: A => B)(g: B => A): VarJs[B] =
    VarJs(f(self.get()))(b => self.set(g(b)))
}

object VarJs {

  def apply[A](get: => A)(set: A => Unit): VarJs[A] =
    new VarJs(() => get, set)

  def free[A](initialState: A): VarJs[A] = {
    var a = initialState
    apply(a)(a2 => a = a2)
  }

  def unsafeField[A](j: Any, field: String): VarJs[A] = {
    val d = j.asInstanceOf[js.Dynamic]
    new VarJs(
      () => d.selectDynamic(field).asInstanceOf[A],
      a => d.updateDynamic(field)(a.asInstanceOf[js.Any]))
  }

}
