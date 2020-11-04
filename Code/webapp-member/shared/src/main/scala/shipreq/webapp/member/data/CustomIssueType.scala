package shipreq.webapp.member.data

import monocle.macros.Lenses
import shipreq.base.util.TaggedTypes._
final case class CustomIssueTypeId(value: Int) extends TaggedInt

@Lenses
final case class CustomIssueType(id  : CustomIssueTypeId,
                                 key : HashRefKey,
                                 desc: Option[String],
                                 live: Live)

object CustomIssueType {
  implicit def equality: UnivEq[CustomIssueType] = UnivEq.derive

  object IdAccess extends ObjDataId[CustomIssueType.type, CustomIssueType, CustomIssueTypeId] {
    override def id(d: CustomIssueType) = d.id
    override val unapplyData: AnyRef => Option[CustomIssueType] = {case r: CustomIssueType => Some(r); case _ => None}
  }
}