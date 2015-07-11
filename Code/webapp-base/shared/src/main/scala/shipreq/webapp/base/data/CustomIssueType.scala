package shipreq.webapp.base.data

import monocle.macros.Lenses
import shipreq.base.util.TaggedTypes._
import shipreq.base.util.UnivEq
import shipreq.webapp.base.util.TypeclassDerivation._

final case class CustomIssueTypeId(value: Long) extends TaggedLong

@Lenses
final case class CustomIssueType(id  : CustomIssueTypeId,
                                 key : HashRefKey,
                                 desc: Option[String],
                                 live: Live)

object CustomIssueType {
  implicit def equality: UnivEq[CustomIssueType] = deriveUnivEq

  object IdAccess extends ObjDataId[CustomIssueType.type, CustomIssueType, CustomIssueTypeId] {
    override def id(d: CustomIssueType) = d.id
    override val unapplyData: AnyRef => Option[CustomIssueType] = {case r: CustomIssueType => Some(r); case _ => None}
  }
}