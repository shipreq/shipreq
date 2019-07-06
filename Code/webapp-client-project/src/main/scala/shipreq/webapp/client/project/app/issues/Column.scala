package shipreq.webapp.client.project.app.issues

import shipreq.base.util.Direction
import shipreq.webapp.base.data.CustomFieldId

sealed trait Column

object Column {

  case object IssueCategory                       extends Column
  case object IssueClass                          extends Column
  case object FieldName                           extends Column
  case object FieldEditor                         extends Column
  case object Actions                             extends Column

  case object Pubid                               extends Column
  case object Code                                extends Column
  case object Title                               extends Column
  case object ReqType                             extends Column
  case object Tags                                extends Column
  final case class Implications(dir: Direction)   extends Column
  final case class CustomField(id: CustomFieldId) extends Column

}