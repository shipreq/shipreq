package shipreq.webapp.client.util

import scalaz.Isomorphism.<=>
import shipreq.base.util.UnivEq

/** Is a subject on or off? */
sealed trait On

case object On extends On with (Boolean <=> On) {
  @inline implicit def equality = UnivEq.force[On]

  override val from: On => Boolean = _ == On
  override val to  : Boolean => On = if (_) On else Off

  def memo[A](f: On => A): On => A = {
    val on  = f(On)
    val off = f(Off)
    o => if (On from o) on else off
  }
}

case object Off extends On with (Boolean <=> On) {
  override val from: On => Boolean = _ == Off
  override val to  : Boolean => On = if (_) Off else On
}
