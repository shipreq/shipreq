package shipreq.webapp.shared.data

import scalaz.Equal
import shipreq.base.util.TaggedTypes._

final case class CustomIncmpType(id: CustomIncmpType.Id,
                                 key: RefKey,
                                 desc: Option[String],
                                 alive: Alive)

object CustomIncmpType {
  final case class Id(value: Long) extends TaggedLong
}

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
