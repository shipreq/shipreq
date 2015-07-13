package shipreq.webapp.base.protocol

import scalaz.{\/-, -\/, \/}
import scalaz.std.AllInstances._
import shipreq.base.util.{UnivEq, Position => Pos}
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta.{Partition, PPI}
import shipreq.webapp.base.util.TypeclassDerivation._
import Field.ApplicableReqTypes

object FieldProtocol {

  sealed trait Values

  case class TextFieldValues(name     : String,
                             key      : FieldRefKey,
                             mandatory: Mandatory,
                             reqTypes : ApplicableReqTypes) extends Values

  case class TagFieldValues(tagId    : TagId,
                            mandatory: Mandatory,
                            reqTypes : ApplicableReqTypes) extends Values

  case class ImplicationFieldValues(reqTypeId: ReqTypeId,
                                    mandatory: Mandatory,
                                    reqTypes : ApplicableReqTypes) extends Values

  implicit val equalText       : UnivEq[TextFieldValues]        = deriveUnivEq
  implicit val equalTag        : UnivEq[TagFieldValues]         = deriveUnivEq
  implicit val equalImplication: UnivEq[ImplicationFieldValues] = deriveUnivEq
  implicit def equalValues     : UnivEq[Values]                 = deriveUnivEq

  type Position = Pos[FieldId]

  sealed trait CfgAction
  object CfgAction {
    final case class Create      (newValues: Values)                    extends CfgAction
    final case class UpdateValues(id: CustomFieldId, newValues: Values) extends CfgAction
    final case class UpdateOrder (id: FieldId, newPos: Position)        extends CfgAction
    final case class Delete      (id: FieldId, action: DeletionAction)  extends CfgAction
    implicit lazy val equality: UnivEq[CfgAction] = {import AutoDerive._; deriveUnivEq}
  }

  case class Delta(field: StaticField \/ CustomField, pos: Position)

  implicit object Delta extends DataId[Delta] {
    override type I = FieldId
    override def id(d: Delta) = d.field.fold(s => s, _.id)
    override val unapplyData: AnyRef => Option[Delta] = {case r: Delta => Some(r); case _ => None}
  }

  val ppi = PPI.lens(Partition.Fields, Project.fields) { (delta, orig) =>
    var FieldSet(customFields, order) = orig

    // Delete fields
    for (fieldId <- delta.delete)
      fieldId match {
        case i: CustomFieldId => customFields = customFields - i
        case _: StaticField   => ()
      }
    order = order.filterNot(delta.delete.contains)

    // Insert/update
    def setOrder(id: FieldId, pos: Position): Unit =
      order = Pos.set(order, id, pos)
    for (delta <- delta.update)
      delta match {
        case Delta(-\/(staticField), pos) =>
          setOrder(staticField, pos)
        case Delta(\/-(customField), pos) =>
          customFields += customField
          setOrder(customField.id, pos)
      }

    // Done
    FieldSet(customFields, order)
  }
}