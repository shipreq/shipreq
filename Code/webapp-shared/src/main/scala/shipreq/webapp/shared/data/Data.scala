package shipreq.webapp.shared.data

import scalaz.Isomorphism.<=>

sealed trait Alive
case object Alive extends Alive with (Boolean <=> Alive) {
  override def from = _ == Alive
  override def to = b => if (b) Alive else Dead
}
case object Dead extends Alive
