package shipreq.webapp.base.data

import monocle.macros.Lenser
import shipreq.base.util.TaggedTypes._

final case class CustomIssueType(id: CustomIssueType.Id,
                                 key: HashRefKey,
                                 desc: Option[String],
                                 alive: Alive)

object CustomIssueType {
  final case class Id(value: Long) extends TaggedLong

  object IdAccess extends ObjDataIdM[CustomIssueType.type, CustomIssueType, Id] {
    override def id(d: CustomIssueType) = d.id
    override def mkId(l: Long) = Id(l)
    override def setId(a: CustomIssueType, b: Id) = a.copy(id = b)
  }

  private[this] def l = Lenser[CustomIssueType]
  val _key = l(_.key)
}