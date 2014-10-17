package shipreq.webapp.shared.data

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


// =====================================================================================================================

trait DataAndId {
  type Data
  type Id <: TaggedLong
}

trait IdAccessor[T <: DataAndId] {
  def id(d: T#Data): T#Id
  def setId(d: T#Data, id: T#Id): T#Data
  def mkId(l: Long): T#Id // For testing
}

trait DataObjImplicits {
  implicit def tcCustomIncmpType = CustomIncmpType
  implicit def tcCustomReqType = CustomReqType
}

object DataImplicits extends Project.Implicits with DataObjImplicits {

  implicit class DataAndIdDataExt[Q <: DataAndId](val d: Q#Data) extends AnyVal {
    def id(implicit i: IdAccessor[Q]): Q#Id = i.id(d)
  }
}