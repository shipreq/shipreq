package shipreq.webapp.client.util

import scalaz.Isomorphism.<=>
import shipreq.base.util.UnivEq

sealed trait Enabled

case object Enabled extends Enabled with (Boolean <=> Enabled) {
  @inline implicit def equality = UnivEq.force[Enabled]

  override val from: Enabled => Boolean = _ == Enabled
  override val to  : Boolean => Enabled = if (_) Enabled else Disabled
}

case object Disabled extends Enabled with (Boolean <=> Enabled) {
  override val from: Enabled => Boolean = _ == Disabled
  override val to  : Boolean => Enabled = if (_) Disabled else Enabled
}
