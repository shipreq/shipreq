package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.scalajs.react.Reusability
import japgolly.univeq.UnivEq
import shipreq.webapp.base.lib.BaseReusability._

sealed abstract class Column(final val key: String)

object Column {

  case object IssueCategory                       extends Column("a")
  case object IssueClass                          extends Column("b")
  case object FieldName                           extends Column("c")
  case object FieldEditor                         extends Column("d")
  case object Actions                             extends Column("e")
  case object Id                                  extends Column("f")
  case object Title                               extends Column("g")

//  case object Code                                extends Column("h")
//  case object ReqType                             extends Column("i")
//  case object Tags                                extends Column("j")
//  final case class Implications(dir: Direction)   extends Column(if (dir is Forwards) "k" else "l")
//  final case class CustomField(id: CustomFieldId) extends Column("m" + id.foldId(_.name, _.value.toString))

  implicit def univEq: UnivEq[Column] = UnivEq.derive

  implicit def reusability: Reusability[Column] = Reusability.byRefOrUnivEq
}