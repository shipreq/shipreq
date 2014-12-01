package shipreq.webapp.base.data

import scalaz.Equal
import scalaz.Isomorphism.<=>
import shipreq.base.util.TaggedTypes.TaggedLong


final case class Rev(value: Long) extends TaggedLong {
  @inline def succ      = Rev(value + 1L)
  @inline def +(r: Rev) = Rev(value + r.value)
}


sealed trait Alive
case object Alive extends Alive with (Boolean <=> Alive) {
  implicit val equal = Equal.equalA[Alive]
  override def from = _ == Alive
  override def to = b => if (b) Alive else Dead
}
case object Dead extends Alive


sealed trait ImplicationRequired
case object ImplicationRequired extends ImplicationRequired with (Boolean <=> ImplicationRequired) {
  implicit val equal = Equal.equalA[ImplicationRequired]
  override def from = _ == ImplicationRequired
  override def to = b => if (b) ImplicationRequired else ImplicationNotRequired
}
case object ImplicationNotRequired extends ImplicationRequired
