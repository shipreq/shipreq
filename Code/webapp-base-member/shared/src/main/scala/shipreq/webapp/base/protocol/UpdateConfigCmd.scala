package shipreq.webapp.base.protocol

import scalaz.\&/
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import Field.ApplicableReqTypes

sealed trait UpdateConfigCmd

object UpdateConfigCmd {

  sealed trait ToModifyCustomIssueTypes                                                            extends UpdateConfigCmd
  final case class CustomIssueTypeCreate (values: CustomIssueTypeValues)                           extends ToModifyCustomIssueTypes
  final case class CustomIssueTypeUpdate (id: CustomIssueTypeId, newValues: CustomIssueTypeValues) extends ToModifyCustomIssueTypes
  final case class CustomIssueTypeDelete (id: CustomIssueTypeId)                                   extends ToModifyCustomIssueTypes
  final case class CustomIssueTypeRestore(id: CustomIssueTypeId)                                   extends ToModifyCustomIssueTypes

  sealed trait ToModifyCustomReqTypes                                                        extends UpdateConfigCmd
  final case class CustomReqTypeCreate (values: CustomReqTypeValues)                         extends ToModifyCustomReqTypes
  final case class CustomReqTypeUpdate (id: CustomReqTypeId, newValues: CustomReqTypeValues) extends ToModifyCustomReqTypes
  final case class CustomReqTypeDelete (id: CustomReqTypeId)                                 extends ToModifyCustomReqTypes
  final case class CustomReqTypeRestore(id: CustomReqTypeId)                                 extends ToModifyCustomReqTypes

  sealed trait ToModifyFields                                                                        extends UpdateConfigCmd
  final case class CustomFieldCreate    (values: CustomFieldValues)                                  extends ToModifyFields
  final case class CustomFieldUpdateImp (id: CustomField.Implication.Id, newValues: ImpFieldValues)  extends ToModifyFields
  final case class CustomFieldUpdateTag (id: CustomField.Tag        .Id, newValues: TagFieldValues)  extends ToModifyFields
  final case class CustomFieldUpdateText(id: CustomField.Text       .Id, newValues: TextFieldValues) extends ToModifyFields
  final case class FieldDelete          (id: FieldId)                                                extends ToModifyFields
  final case class FieldRestore         (id: FieldId)                                                extends ToModifyFields
  final case class FieldUpdateOrder     (id: FieldId, newPos: RelPos[FieldId])                       extends ToModifyFields

  sealed trait ToModifyTags                                  extends UpdateConfigCmd

  // TODO REMOVE
  final case class TagCreate (values: TagData)               extends ToModifyTags
  final case class TagUpdate (id: TagId, newValues: TagData) extends ToModifyTags

  // TODO KEEP
  final case class TagSetApplicableChildrenOrder(id: TagGroupId, children: Vector[ApplicableTagId]) extends ToModifyTags
  final case class TagDelete (id: TagId)                     extends ToModifyTags
  final case class TagRestore(id: TagId)                     extends ToModifyTags

  // TODO USE
//  final case class TagCreate(values: TagValues, parents: Set[TagGroupId], children: Vector[TagId]) extends ToModifyTags
//
//
//  final case class TagDelete(id: TagId) extends ToModifyTags
//  final case class TagRestore(id: TagId) extends ToModifyTags
//
//  //final case class TagUpdateGroup(id: TagGroupId, newValues: TagData) extends ToModifyTags
//
////  final case class TagGroupCreate(id: TagGroupId, vs: TagGroupGD.NonEmptyValues) extends ActiveEvent
////  final case class TagGroupUpdate(id: TagGroupId, vs: TagGroupGD.NonEmptyValues) extends ActiveEvent
////
////  final case class ApplicableTagCreate(id: ApplicableTagId, vs: ApplicableTagGD.NonEmptyValues) extends ActiveEvent
////  final case class ApplicableTagUpdate(id: ApplicableTagId, vs: ApplicableTagGD.NonEmptyValues) extends ActiveEvent
//
//  //final case class TagUpdate (id: TagId, newValues: TagData) extends ToModifyTags

  // ===================================================================================================================

  final case class CustomIssueTypeValues(key : HashRefKey,
                                         desc: Option[String])

  final case class CustomReqTypeValues(mnemonic: ReqType.Mnemonic,
                                       name    : String,
                                       imp     : ImplicationRequired)

  sealed trait CustomFieldValues

  final case class TextFieldValues(name     : String,
                                   key      : FieldRefKey,
                                   mandatory: Mandatory,
                                   reqTypes : ApplicableReqTypes) extends CustomFieldValues

  final case class TagFieldValues(tagId    : TagId,
                                  mandatory: Mandatory,
                                  reqTypes : ApplicableReqTypes) extends CustomFieldValues

  final case class ImpFieldValues(reqTypeId: ReqTypeId,
                                  mandatory: Mandatory,
                                  reqTypes : ApplicableReqTypes) extends CustomFieldValues

  sealed trait TagValues

  final case class TagGroupValues(name         : String,
                                  mutexChildren: MutexChildren,
                                  desc         : Option[String]) extends TagValues

  final case class ApplicableTagValues(name: String,
                                       key : HashRefKey,
                                       desc: Option[String]) extends TagValues

  type TagData = TagValues \&/ TagInTree.Relations

  // ===================================================================================================================

  implicit def univEqCustomIssueTypeValues : UnivEq[CustomIssueTypeValues] = UnivEq.derive
  implicit def univEqCustomReqTypeValues   : UnivEq[CustomReqTypeValues  ] = UnivEq.derive
  implicit def univEqTextFieldValues       : UnivEq[TextFieldValues      ] = UnivEq.derive
  implicit def univEqTagFieldValues        : UnivEq[TagFieldValues       ] = UnivEq.derive
  implicit def univEqImplicationFieldValues: UnivEq[ImpFieldValues       ] = UnivEq.derive
  implicit def univEqCustomFieldValues     : UnivEq[CustomFieldValues    ] = UnivEq.derive
  implicit def univEqTagGroupValues        : UnivEq[TagGroupValues       ] = UnivEq.derive
  implicit def univEqApplicableTagValues   : UnivEq[ApplicableTagValues  ] = UnivEq.derive
  implicit def univEqTagValues             : UnivEq[TagValues            ] = UnivEq.derive
  implicit def univEq                      : UnivEq[UpdateConfigCmd      ] = UnivEq.derive

  // ===================================================================================================================
  import boopickle.DefaultBasic._
  import shipreq.webapp.base.protocol.binary.v1.BaseData._
  import shipreq.webapp.base.protocol.binary.v1.BaseMemberData1._
  import shipreq.webapp.base.protocol.binary.v1.BaseMemberData2._

  private implicit val picklerCustomIssueTypeValues: Pickler[CustomIssueTypeValues] =
    new Pickler[CustomIssueTypeValues] {
      override def pickle(a: CustomIssueTypeValues)(implicit state: PickleState): Unit = {
        state.pickle(a.key)
        state.pickle(a.desc)
      }
      override def unpickle(implicit state: UnpickleState): CustomIssueTypeValues = {
        val key  = state.unpickle[HashRefKey]
        val desc = state.unpickle[Option[String]]
        CustomIssueTypeValues(key, desc)
      }
    }

  private implicit val picklerCustomReqTypeValues: Pickler[CustomReqTypeValues] =
    new Pickler[CustomReqTypeValues] {
      override def pickle(a: CustomReqTypeValues)(implicit state: PickleState): Unit = {
        state.pickle(a.mnemonic)
        state.pickle(a.name)
        state.pickle(a.imp)
      }
      override def unpickle(implicit state: UnpickleState): CustomReqTypeValues = {
        val mnemonic = state.unpickle[ReqType.Mnemonic]
        val name     = state.unpickle[String]
        val imp      = state.unpickle[ImplicationRequired]
        CustomReqTypeValues(mnemonic, name, imp)
      }
    }

  private implicit val picklerTextFieldValues: Pickler[TextFieldValues] =
    new Pickler[TextFieldValues] {
      override def pickle(a: TextFieldValues)(implicit state: PickleState): Unit = {
        state.pickle(a.name)
        state.pickle(a.key)
        state.pickle(a.mandatory)
        state.pickle(a.reqTypes)
      }
      override def unpickle(implicit state: UnpickleState): TextFieldValues = {
        val name      = state.unpickle[String]
        val key       = state.unpickle[FieldRefKey]
        val mandatory = state.unpickle[Mandatory]
        val reqTypes  = state.unpickle[ApplicableReqTypes]
        TextFieldValues(name, key, mandatory, reqTypes)
      }
    }

  private implicit val picklerTagFieldValues: Pickler[TagFieldValues] =
    new Pickler[TagFieldValues] {
      override def pickle(a: TagFieldValues)(implicit state: PickleState): Unit = {
        state.pickle(a.tagId)
        state.pickle(a.mandatory)
        state.pickle(a.reqTypes)
      }
      override def unpickle(implicit state: UnpickleState): TagFieldValues = {
        val tagId     = state.unpickle[TagId]
        val mandatory = state.unpickle[Mandatory]
        val reqTypes  = state.unpickle[ApplicableReqTypes]
        TagFieldValues(tagId, mandatory, reqTypes)
      }
    }

  private implicit val picklerImpFieldValues: Pickler[ImpFieldValues] =
    new Pickler[ImpFieldValues] {
      override def pickle(a: ImpFieldValues)(implicit state: PickleState): Unit = {
        state.pickle(a.reqTypeId)
        state.pickle(a.mandatory)
        state.pickle(a.reqTypes)
      }
      override def unpickle(implicit state: UnpickleState): ImpFieldValues = {
        val reqTypeId = state.unpickle[ReqTypeId]
        val mandatory = state.unpickle[Mandatory]
        val reqTypes  = state.unpickle[ApplicableReqTypes]
        ImpFieldValues(reqTypeId, mandatory, reqTypes)
      }
    }

  private implicit val picklerTagGroupValues: Pickler[TagGroupValues] =
    new Pickler[TagGroupValues] {
      override def pickle(a: TagGroupValues)(implicit state: PickleState): Unit = {
        state.pickle(a.name)
        state.pickle(a.mutexChildren)
        state.pickle(a.desc)
      }
      override def unpickle(implicit state: UnpickleState): TagGroupValues = {
        val name          = state.unpickle[String]
        val mutexChildren = state.unpickle[MutexChildren]
        val desc          = state.unpickle[Option[String]]
        TagGroupValues(name, mutexChildren, desc)
      }
    }

  private implicit val picklerApplicableTagValues: Pickler[ApplicableTagValues] =
    new Pickler[ApplicableTagValues] {
      override def pickle(a: ApplicableTagValues)(implicit state: PickleState): Unit = {
        state.pickle(a.name)
        state.pickle(a.key)
        state.pickle(a.desc)
      }
      override def unpickle(implicit state: UnpickleState): ApplicableTagValues = {
        val name = state.unpickle[String]
        val key  = state.unpickle[HashRefKey]
        val desc = state.unpickle[Option[String]]
        ApplicableTagValues(name, key, desc)
      }
    }

  private implicit val picklerCustomFieldValues: Pickler[CustomFieldValues] =
    new Pickler[CustomFieldValues] {
      private[this] final val KeyImpFieldValues  = 'i'
      private[this] final val KeyTagFieldValues  = 't'
      private[this] final val KeyTextFieldValues = 'x'
      override def pickle(a: CustomFieldValues)(implicit state: PickleState): Unit =
        a match {
          case b: ImpFieldValues  => state.enc.writeByte(KeyImpFieldValues ); state.pickle(b)
          case b: TagFieldValues  => state.enc.writeByte(KeyTagFieldValues ); state.pickle(b)
          case b: TextFieldValues => state.enc.writeByte(KeyTextFieldValues); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): CustomFieldValues =
        state.dec.readByte match {
          case KeyImpFieldValues  => state.unpickle[ImpFieldValues]
          case KeyTagFieldValues  => state.unpickle[TagFieldValues]
          case KeyTextFieldValues => state.unpickle[TextFieldValues]
        }
    }

  private implicit val picklerTagValues: Pickler[TagValues] =
    new Pickler[TagValues] {
      private[this] final val KeyApplicableTagValues = 'a'
      private[this] final val KeyTagGroupValues      = 'g'
      override def pickle(a: TagValues)(implicit state: PickleState): Unit =
        a match {
          case b: ApplicableTagValues => state.enc.writeByte(KeyApplicableTagValues); state.pickle(b)
          case b: TagGroupValues      => state.enc.writeByte(KeyTagGroupValues     ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): TagValues =
        state.dec.readByte match {
          case KeyApplicableTagValues => state.unpickle[ApplicableTagValues]
          case KeyTagGroupValues      => state.unpickle[TagGroupValues]
        }
    }

  private implicit val picklerTagData: Pickler[TagData] =
    pickleIor

  // -------------------------------------------------------------------------------------------------------------------

  private implicit val picklerCustomIssueTypeCreate: Pickler[CustomIssueTypeCreate] =
    transformPickler(CustomIssueTypeCreate.apply)(_.values)

  private implicit val picklerCustomIssueTypeUpdate: Pickler[CustomIssueTypeUpdate] =
    new Pickler[CustomIssueTypeUpdate] {
      override def pickle(a: CustomIssueTypeUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.newValues)
      }
      override def unpickle(implicit state: UnpickleState): CustomIssueTypeUpdate = {
        val id        = state.unpickle[CustomIssueTypeId]
        val newValues = state.unpickle[CustomIssueTypeValues]
        CustomIssueTypeUpdate(id, newValues)
      }
    }

  private implicit val picklerCustomIssueTypeDelete: Pickler[CustomIssueTypeDelete] =
    transformPickler(CustomIssueTypeDelete.apply)(_.id)

  private implicit val picklerCustomIssueTypeRestore: Pickler[CustomIssueTypeRestore] =
    transformPickler(CustomIssueTypeRestore.apply)(_.id)

  private implicit val picklerCustomReqTypeCreate: Pickler[CustomReqTypeCreate] =
    transformPickler(CustomReqTypeCreate.apply)(_.values)

  private implicit val picklerCustomReqTypeUpdate: Pickler[CustomReqTypeUpdate] =
    new Pickler[CustomReqTypeUpdate] {
      override def pickle(a: CustomReqTypeUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.newValues)
      }
      override def unpickle(implicit state: UnpickleState): CustomReqTypeUpdate = {
        val id        = state.unpickle[CustomReqTypeId]
        val newValues = state.unpickle[CustomReqTypeValues]
        CustomReqTypeUpdate(id, newValues)
      }
    }

  private implicit val picklerCustomReqTypeDelete: Pickler[CustomReqTypeDelete] =
    transformPickler(CustomReqTypeDelete.apply)(_.id)

  private implicit val picklerCustomReqTypeRestore: Pickler[CustomReqTypeRestore] =
    transformPickler(CustomReqTypeRestore.apply)(_.id)

  private implicit val picklerCustomFieldCreate: Pickler[CustomFieldCreate] =
    transformPickler(CustomFieldCreate.apply)(_.values)

  private implicit val picklerCustomFieldUpdateImp: Pickler[CustomFieldUpdateImp] =
    new Pickler[CustomFieldUpdateImp] {
      override def pickle(a: CustomFieldUpdateImp)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.newValues)
      }
      override def unpickle(implicit state: UnpickleState): CustomFieldUpdateImp = {
        val id        = state.unpickle[CustomField.Implication.Id]
        val newValues = state.unpickle[ImpFieldValues]
        CustomFieldUpdateImp(id, newValues)
      }
    }

  private implicit val picklerCustomFieldUpdateTag: Pickler[CustomFieldUpdateTag] =
    new Pickler[CustomFieldUpdateTag] {
      override def pickle(a: CustomFieldUpdateTag)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.newValues)
      }
      override def unpickle(implicit state: UnpickleState): CustomFieldUpdateTag = {
        val id        = state.unpickle[CustomField.Tag        .Id]
        val newValues = state.unpickle[TagFieldValues]
        CustomFieldUpdateTag(id, newValues)
      }
    }

  private implicit val picklerCustomFieldUpdateText: Pickler[CustomFieldUpdateText] =
    new Pickler[CustomFieldUpdateText] {
      override def pickle(a: CustomFieldUpdateText)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.newValues)
      }
      override def unpickle(implicit state: UnpickleState): CustomFieldUpdateText = {
        val id        = state.unpickle[CustomField.Text       .Id]
        val newValues = state.unpickle[TextFieldValues]
        CustomFieldUpdateText(id, newValues)
      }
    }

  private implicit val picklerFieldDelete: Pickler[FieldDelete] =
    transformPickler(FieldDelete.apply)(_.id)

  private implicit val picklerFieldRestore: Pickler[FieldRestore] =
    transformPickler(FieldRestore.apply)(_.id)

  private implicit val picklerFieldUpdateOrder: Pickler[FieldUpdateOrder] =
    new Pickler[FieldUpdateOrder] {
      override def pickle(a: FieldUpdateOrder)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.newPos)
      }
      override def unpickle(implicit state: UnpickleState): FieldUpdateOrder = {
        val id     = state.unpickle[FieldId]
        val newPos = state.unpickle[RelPos[FieldId]]
        FieldUpdateOrder(id, newPos)
      }
    }

  private implicit val picklerTagCreate: Pickler[TagCreate] =
    transformPickler(TagCreate.apply)(_.values)

  private implicit val picklerTagUpdate: Pickler[TagUpdate] =
    new Pickler[TagUpdate] {
      override def pickle(a: TagUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.newValues)
      }
      override def unpickle(implicit state: UnpickleState): TagUpdate = {
        val id        = state.unpickle[TagId]
        val newValues = state.unpickle[TagData]
        TagUpdate(id, newValues)
      }
    }

  private implicit val picklerTagDelete: Pickler[TagDelete] =
    transformPickler(TagDelete.apply)(_.id)

  private implicit val picklerTagRestore: Pickler[TagRestore] =
    transformPickler(TagRestore.apply)(_.id)

  private implicit val picklerTagSetApplicableChildrenOrder: Pickler[TagSetApplicableChildrenOrder] =
    new Pickler[TagSetApplicableChildrenOrder] {
      override def pickle(a: TagSetApplicableChildrenOrder)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.children)
      }
      override def unpickle(implicit state: UnpickleState): TagSetApplicableChildrenOrder = {
        val id       = state.unpickle[TagGroupId]
        val children = state.unpickle[Vector[ApplicableTagId]]
        TagSetApplicableChildrenOrder(id, children)
      }
    }

  implicit val pickler: Pickler[UpdateConfigCmd] =
    new Pickler[UpdateConfigCmd] {
      private[this] final val KeyCustomFieldCreate             = 0
      private[this] final val KeyCustomFieldUpdateImp          = 1
      private[this] final val KeyCustomFieldUpdateTag          = 2
      private[this] final val KeyCustomFieldUpdateText         = 3
      private[this] final val KeyCustomIssueTypeCreate         = 4
      private[this] final val KeyCustomIssueTypeDelete         = 5
      private[this] final val KeyCustomIssueTypeRestore        = 6
      private[this] final val KeyCustomIssueTypeUpdate         = 7
      private[this] final val KeyCustomReqTypeCreate           = 8
      private[this] final val KeyCustomReqTypeDelete           = 9
      private[this] final val KeyCustomReqTypeRestore          = 10
      private[this] final val KeyCustomReqTypeUpdate           = 11
      private[this] final val KeyFieldDelete                   = 12
      private[this] final val KeyFieldRestore                  = 13
      private[this] final val KeyFieldUpdateOrder              = 14
      private[this] final val KeyTagCreate                     = 15
      private[this] final val KeyTagDelete                     = 16
      private[this] final val KeyTagRestore                    = 17
      private[this] final val KeyTagUpdate                     = 18
      private[this] final val KeyTagSetApplicableChildrenOrder = 19
      override def pickle(a: UpdateConfigCmd)(implicit state: PickleState): Unit =
        a match {
          case b: CustomFieldCreate             => state.enc.writeByte(KeyCustomFieldCreate            ); state.pickle(b)
          case b: CustomFieldUpdateImp          => state.enc.writeByte(KeyCustomFieldUpdateImp         ); state.pickle(b)
          case b: CustomFieldUpdateTag          => state.enc.writeByte(KeyCustomFieldUpdateTag         ); state.pickle(b)
          case b: CustomFieldUpdateText         => state.enc.writeByte(KeyCustomFieldUpdateText        ); state.pickle(b)
          case b: CustomIssueTypeCreate         => state.enc.writeByte(KeyCustomIssueTypeCreate        ); state.pickle(b)
          case b: CustomIssueTypeDelete         => state.enc.writeByte(KeyCustomIssueTypeDelete        ); state.pickle(b)
          case b: CustomIssueTypeRestore        => state.enc.writeByte(KeyCustomIssueTypeRestore       ); state.pickle(b)
          case b: CustomIssueTypeUpdate         => state.enc.writeByte(KeyCustomIssueTypeUpdate        ); state.pickle(b)
          case b: CustomReqTypeCreate           => state.enc.writeByte(KeyCustomReqTypeCreate          ); state.pickle(b)
          case b: CustomReqTypeDelete           => state.enc.writeByte(KeyCustomReqTypeDelete          ); state.pickle(b)
          case b: CustomReqTypeRestore          => state.enc.writeByte(KeyCustomReqTypeRestore         ); state.pickle(b)
          case b: CustomReqTypeUpdate           => state.enc.writeByte(KeyCustomReqTypeUpdate          ); state.pickle(b)
          case b: FieldDelete                   => state.enc.writeByte(KeyFieldDelete                  ); state.pickle(b)
          case b: FieldRestore                  => state.enc.writeByte(KeyFieldRestore                 ); state.pickle(b)
          case b: FieldUpdateOrder              => state.enc.writeByte(KeyFieldUpdateOrder             ); state.pickle(b)
          case b: TagCreate                     => state.enc.writeByte(KeyTagCreate                    ); state.pickle(b)
          case b: TagDelete                     => state.enc.writeByte(KeyTagDelete                    ); state.pickle(b)
          case b: TagRestore                    => state.enc.writeByte(KeyTagRestore                   ); state.pickle(b)
          case b: TagUpdate                     => state.enc.writeByte(KeyTagUpdate                    ); state.pickle(b)
          case b: TagSetApplicableChildrenOrder => state.enc.writeByte(KeyTagSetApplicableChildrenOrder); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): UpdateConfigCmd =
        state.dec.readByte match {
          case KeyCustomFieldCreate             => state.unpickle[CustomFieldCreate]
          case KeyCustomFieldUpdateImp          => state.unpickle[CustomFieldUpdateImp]
          case KeyCustomFieldUpdateTag          => state.unpickle[CustomFieldUpdateTag]
          case KeyCustomFieldUpdateText         => state.unpickle[CustomFieldUpdateText]
          case KeyCustomIssueTypeCreate         => state.unpickle[CustomIssueTypeCreate]
          case KeyCustomIssueTypeDelete         => state.unpickle[CustomIssueTypeDelete]
          case KeyCustomIssueTypeRestore        => state.unpickle[CustomIssueTypeRestore]
          case KeyCustomIssueTypeUpdate         => state.unpickle[CustomIssueTypeUpdate]
          case KeyCustomReqTypeCreate           => state.unpickle[CustomReqTypeCreate]
          case KeyCustomReqTypeDelete           => state.unpickle[CustomReqTypeDelete]
          case KeyCustomReqTypeRestore          => state.unpickle[CustomReqTypeRestore]
          case KeyCustomReqTypeUpdate           => state.unpickle[CustomReqTypeUpdate]
          case KeyFieldDelete                   => state.unpickle[FieldDelete]
          case KeyFieldRestore                  => state.unpickle[FieldRestore]
          case KeyFieldUpdateOrder              => state.unpickle[FieldUpdateOrder]
          case KeyTagCreate                     => state.unpickle[TagCreate]
          case KeyTagDelete                     => state.unpickle[TagDelete]
          case KeyTagRestore                    => state.unpickle[TagRestore]
          case KeyTagUpdate                     => state.unpickle[TagUpdate]
          case KeyTagSetApplicableChildrenOrder => state.unpickle[TagSetApplicableChildrenOrder]
        }
    }
}
