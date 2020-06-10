package shipreq.webapp.base.lib

import japgolly.scalajs.react.{CallbackTo, Reusability}

/** Abstraction over JS's `window.confirm`. */
trait ConfirmJs {
  def apply(msg: String): CallbackTo[Boolean]
}

object ConfirmJs {

  val real: ConfirmJs =
    CallbackTo.confirm

  def const(b: Boolean): ConfirmJs =
    const(CallbackTo.pure(b))

  def const(cb: CallbackTo[Boolean]): ConfirmJs =
    _ => cb

  implicit def reusability: Reusability[ConfirmJs] =
    Reusability.byRef
}