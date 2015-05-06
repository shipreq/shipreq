package shipreq.webapp.base.data

import monocle.macros.GenLens
import shipreq.base.util.TaggedTypes._

final case class CustomIssueTypeId(value: Long) extends TaggedLong

final case class CustomIssueType(id   : CustomIssueTypeId,
                                 key  : HashRefKey,
                                 desc : Option[String],
                                 alive: Alive)

object CustomIssueType {
  object IdAccess extends ObjDataId[CustomIssueType.type, CustomIssueType, CustomIssueTypeId] {
    override def id(d: CustomIssueType) = d.id
    override val unapplyData: AnyRef => Option[CustomIssueType] = {case r: CustomIssueType => Some(r); case _ => None}
  }

  val key = GenLens[CustomIssueType](_.key)
}