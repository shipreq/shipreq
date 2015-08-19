package shipreq.webapp.client.app.ui

import japgolly.scalajs.react.Callback
import shipreq.webapp.client.lib.TCB

/**
 * [V]alue
 * [U]pdate
 * [C]ommit
 * [A]bort
 */
case class VUCA[A, -B](value: A, update: A => Callback, commit: B => TCB.Commit, abort: TCB.Abort)

object VUCA {

  /** Value & Update only. Abort & Commit will do nothing. */
  @inline def vu[A](value: A, update: A => Callback): VUCA[A, Any] =
    VUCA(value, update, TCB.Commit._nop, TCB.Abort.nop)
}