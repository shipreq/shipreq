package shipreq.webapp.shared.data

import scalaz.Equal
import shipreq.base.util.TaggedTypes._

/**
 * A key by which users can insert references to corresponding data.
 *
 * Examples:
 * #TODO refers to a custom incompletion type.
 * #pri=high refers to a grouping.
 */
final case class RefKey(value: String) extends TaggedString
object RefKey {
  implicit val equal = Equal.equalA[RefKey]
}

// =====================================================================================================================

sealed trait CustomIncmpTypeAndId extends DataAndId {
  override type Data = CustomIncmpType
  override type Id = CustomIncmpType.Id
}

final case class CustomIncmpType(id: CustomIncmpType.Id,
                                 key: RefKey,
                                 desc: Option[String],
                                 alive: Alive)

object CustomIncmpType extends IdAccessor[CustomIncmpTypeAndId] {
  final case class Id(value: Long) extends TaggedLong
  override def id(d: CustomIncmpType) = d.id
  override def mkId(l: Long) = Id(l)
  override def setId(a: CustomIncmpType, b: Id) = a.copy(id = b)
}
