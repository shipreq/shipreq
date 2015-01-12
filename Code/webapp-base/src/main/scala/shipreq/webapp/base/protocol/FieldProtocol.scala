package shipreq.webapp.base.protocol

import scalaz.{\/-, -\/, \/}
import scalaz.std.AllInstances._
import shapeless.TypeClass.deriveConstructors
import shapeless.contrib.scalaz.Instances._
import shipreq.base.util.Util
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.Tag.{Id => TagId}
import shipreq.webapp.base.delta.{Partition, RemoteDeltaP}
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

  implicit val textFieldValueEquality = deriveEqual[TextFieldValues]
  implicit val tagFieldValueEquality  = deriveEqual[TagFieldValues]

  // The field immediately before which the subject field should be ordered. None means append.
  type Position = Option[Field.Id]

  sealed trait CfgAction
  object CfgAction {
    final case class Create      (newValues: Values)                     extends CfgAction
    final case class UpdateValues(id: CustomField.Id, newValues: Values) extends CfgAction
    final case class UpdateOrder (id: Field.Id, newPos: Position)        extends CfgAction
    final case class Delete      (id: Field.Id, action: DeletionAction)  extends CfgAction
  }

  case class Delta(field: StaticField \/ CustomField, pos: Position)

  implicit object Delta extends DataId[Delta] {
    override type I = Field.Id
    override def id(d: Delta) = d.field.fold(s => s, _.id)
    override def mkId(l: Long) = ??? // Method only exists for testing
  }

  object PartitionFns extends Partition.Fns[Partition.Fields.type] {
    def rev(p: Project): Rev =
      p.fields.rev

    def update(p: Project, rev: Rev, ds: RemoteDeltaP[Partition.Fields.type]): Project = {
      var FieldSet(customFields, order) = p.fields.data

      // Delete fields
      for (fieldId <- ds.del)
        fieldId match {
          case i: CustomField.Id => customFields = customFields - i
          case _: StaticField    => ()
        }
      order = order.filterNot(ds.del.contains)

      // Insert/update
      def setOrder(id: Field.Id, pos: Position): Unit =
        order = Util.reposition(order, id, pos)
      for (delta <- ds.upd)
        delta match {
          case Delta(-\/(staticField), pos) =>
            setOrder(staticField, pos)
          case Delta(\/-(customField), pos) =>
            customFields += customField
            setOrder(customField.id, pos)
        }

      // Done
      p.copy(fields = RevAnd(rev, FieldSet(customFields, order)))
    }
  }

}