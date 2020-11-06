package shipreq.webapp.member.protocol.websocket

import shipreq.base.util._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._

sealed trait UpdateConfigCmd

object UpdateConfigCmd {

  sealed trait ToModifyCustomIssueTypes extends UpdateConfigCmd
  final case class CustomIssueTypeCreate (key: HashRefKey, desc: Option[String])                              extends ToModifyCustomIssueTypes
  final case class CustomIssueTypeUpdate (id: CustomIssueTypeId, newValues: CustomIssueTypeGD.NonEmptyValues) extends ToModifyCustomIssueTypes
  final case class CustomIssueTypeDelete (id: CustomIssueTypeId)                                              extends ToModifyCustomIssueTypes
  final case class CustomIssueTypeRestore(id: CustomIssueTypeId)                                              extends ToModifyCustomIssueTypes

  sealed trait ToModifyReqTypes extends UpdateConfigCmd
  final case class CustomReqTypeCreate    (mnemonic   : ReqType.Mnemonic,
                                           name       : String,
                                           description: Option[String],
                                           implication: Mandatory)                                         extends ToModifyReqTypes
  final case class CustomReqTypeUpdate    (id: CustomReqTypeId, newValues: CustomReqTypeGD.NonEmptyValues) extends ToModifyReqTypes
  final case class CustomReqTypeDeleteHard(id: CustomReqTypeId)                                            extends ToModifyReqTypes
  final case class CustomReqTypeDeleteSoft(id: CustomReqTypeId)                                            extends ToModifyReqTypes
  final case class CustomReqTypeRestore   (id: CustomReqTypeId)                                            extends ToModifyReqTypes

  sealed trait ToModifyFields extends UpdateConfigCmd
  final case class CustomFieldCreateImp (reqTypeId: ReqTypeId , fieldReqTypeRules: FieldReqTypeRules.ForImpField)     extends ToModifyFields
  final case class CustomFieldCreateText(name     : String    , fieldReqTypeRules: FieldReqTypeRules.ForTextField)    extends ToModifyFields
  final case class CustomFieldUpdateImp (id: CustomField.Implication.Id, newValues: CustomImpFieldGD .NonEmptyValues) extends ToModifyFields
  final case class CustomFieldUpdateTag (id: CustomField.Tag        .Id, newValues: CustomTagFieldGD .NonEmptyValues) extends ToModifyFields
  final case class CustomFieldUpdateText(id: CustomField.Text       .Id, newValues: CustomTextFieldGD.NonEmptyValues) extends ToModifyFields
  final case class CustomFieldDelete    (id: CustomFieldId)                                                           extends ToModifyFields
  final case class CustomFieldRestore   (id: CustomFieldId)                                                           extends ToModifyFields
  final case class StaticFieldAdd       (id: StaticField.Optional)                                                    extends ToModifyFields
  final case class StaticFieldRemove    (id: StaticField.Optional)                                                    extends ToModifyFields
  final case class FieldUpdateOrder     (id: FieldId, newPos: RelPos[FieldId])                                        extends ToModifyFields

  final case class CustomFieldCreateTag(tagId            : TagGroupId,
                                        fieldReqTypeRules: FieldReqTypeRules.ForTagField,
                                        derivativeTags   : DerivativeTags) extends ToModifyFields


  /** Note: you're not allowed to specify any dead values in:
    *
    * - [[ApplicableTagUpdate.newValues]]
    * - [[TagGroupUpdate.newValues]]
    * - [[TagSetLiveChildrenOrder.children]]
    *
    * Only live values will be accepted. `MakeEvent` will take care of dead data preservation when transforming these
    * commands into events.
    */
  sealed trait ToModifyTags extends UpdateConfigCmd
  final case class ApplicableTagCreate    (                     newValues: ApplicableTagGD.NonEmptyValues) extends ToModifyTags
  final case class ApplicableTagUpdate    (id: ApplicableTagId, newValues: ApplicableTagGD.NonEmptyValues) extends ToModifyTags
  final case class TagGroupCreate         (                     newValues: TagGroupGD.NonEmptyValues)      extends ToModifyTags
  final case class TagGroupUpdate         (id: TagGroupId,      newValues: TagGroupGD.NonEmptyValues)      extends ToModifyTags
  final case class TagSetLiveChildrenOrder(id: TagGroupId,      children: Vector[ApplicableTagId])         extends ToModifyTags
  final case class TagDelete              (id: TagId)                                                      extends ToModifyTags
  final case class TagRestore             (id: TagId)                                                      extends ToModifyTags


  implicit def univEq: UnivEq[UpdateConfigCmd] = UnivEq.derive

  // ===================================================================================================================
  object CodecsV2 {
    import boopickle.DefaultBasic._
    import shipreq.webapp.base.protocol.binary.v1.BaseData._
    import shipreq.webapp.member.protocol.binary.v1.BaseMemberData1._
    import shipreq.webapp.member.protocol.binary.v1.Events._
    import shipreq.webapp.member.protocol.binary.v1.Rev1._
    import shipreq.webapp.member.protocol.binary.v1.Rev6._

    private implicit val picklerCustomIssueTypeCreate: Pickler[CustomIssueTypeCreate] =
      new Pickler[CustomIssueTypeCreate] {
        override def pickle(a: CustomIssueTypeCreate)(implicit state: PickleState): Unit = {
          state.pickle(a.key)
          state.pickle(a.desc)
        }
        override def unpickle(implicit state: UnpickleState): CustomIssueTypeCreate = {
          val key  = state.unpickle[HashRefKey]
          val desc = state.unpickle[Option[String]]
          CustomIssueTypeCreate(key, desc)
        }
      }

    private implicit val picklerCustomIssueTypeUpdate: Pickler[CustomIssueTypeUpdate] =
      new Pickler[CustomIssueTypeUpdate] {
        override def pickle(a: CustomIssueTypeUpdate)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.newValues)
        }
        override def unpickle(implicit state: UnpickleState): CustomIssueTypeUpdate = {
          val id        = state.unpickle[CustomIssueTypeId]
          val newValues = state.unpickle[CustomIssueTypeGD.NonEmptyValues]
          CustomIssueTypeUpdate(id, newValues)
        }
      }

    private implicit val picklerCustomIssueTypeDelete: Pickler[CustomIssueTypeDelete] =
      transformPickler(CustomIssueTypeDelete.apply)(_.id)

    private implicit val picklerCustomIssueTypeRestore: Pickler[CustomIssueTypeRestore] =
      transformPickler(CustomIssueTypeRestore.apply)(_.id)

    private implicit val picklerCustomReqTypeCreate: Pickler[CustomReqTypeCreate] =
      new Pickler[CustomReqTypeCreate] {
        override def pickle(a: CustomReqTypeCreate)(implicit state: PickleState): Unit = {
          state.pickle(a.mnemonic)
          state.pickle(a.name)
          state.pickle(a.description)
          state.pickle(a.implication)
        }
        override def unpickle(implicit state: UnpickleState): CustomReqTypeCreate = {
          val mnemonic    = state.unpickle[ReqType.Mnemonic]
          val name        = state.unpickle[String]
          val description = state.unpickle[Option[String]]
          val implication = state.unpickle[Mandatory]
          CustomReqTypeCreate(mnemonic, name, description, implication)
        }
      }

    private implicit val picklerCustomReqTypeUpdate: Pickler[CustomReqTypeUpdate] =
      new Pickler[CustomReqTypeUpdate] {
        override def pickle(a: CustomReqTypeUpdate)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.newValues)
        }
        override def unpickle(implicit state: UnpickleState): CustomReqTypeUpdate = {
          val id        = state.unpickle[CustomReqTypeId]
          val newValues = state.unpickle[CustomReqTypeGD.NonEmptyValues]
          CustomReqTypeUpdate(id, newValues)
        }
      }

    private implicit val picklerCustomReqTypeDeleteHard: Pickler[CustomReqTypeDeleteHard] =
      transformPickler(CustomReqTypeDeleteHard.apply)(_.id)

    private implicit val picklerCustomReqTypeDeleteSoft: Pickler[CustomReqTypeDeleteSoft] =
      transformPickler(CustomReqTypeDeleteSoft.apply)(_.id)

    private implicit val picklerCustomReqTypeRestore: Pickler[CustomReqTypeRestore] =
      transformPickler(CustomReqTypeRestore.apply)(_.id)

    private implicit val picklerCustomFieldCreateImp: Pickler[CustomFieldCreateImp] =
      new Pickler[CustomFieldCreateImp] {
        override def pickle(a: CustomFieldCreateImp)(implicit state: PickleState): Unit = {
          state.pickle(a.reqTypeId)
          state.pickle(a.fieldReqTypeRules)
        }
        override def unpickle(implicit state: UnpickleState): CustomFieldCreateImp = {
          val reqTypeId         = state.unpickle[ReqTypeId]
          val fieldReqTypeRules = state.unpickle[FieldReqTypeRules.ForImpField]
          CustomFieldCreateImp(reqTypeId, fieldReqTypeRules)
        }
      }

    private implicit val picklerCustomFieldCreateTag: Pickler[CustomFieldCreateTag] =
      new Pickler[CustomFieldCreateTag] {
        override def pickle(a: CustomFieldCreateTag)(implicit state: PickleState): Unit = {
          state.pickle(a.tagId)
          state.pickle(a.fieldReqTypeRules)
          state.pickle(a.derivativeTags)
        }
        override def unpickle(implicit state: UnpickleState): CustomFieldCreateTag = {
          val tagId             = state.unpickle[TagGroupId]
          val fieldReqTypeRules = state.unpickle[FieldReqTypeRules.ForTagField]
          val derivativeTags    = state.unpickle[DerivativeTags]
          CustomFieldCreateTag(tagId, fieldReqTypeRules, derivativeTags)
        }
      }

    private implicit val picklerCustomFieldCreateText: Pickler[CustomFieldCreateText] =
      new Pickler[CustomFieldCreateText] {
        override def pickle(a: CustomFieldCreateText)(implicit state: PickleState): Unit = {
          state.pickle(a.name)
          state.pickle(a.fieldReqTypeRules)
        }
        override def unpickle(implicit state: UnpickleState): CustomFieldCreateText = {
          val name              = state.unpickle[String]
          val fieldReqTypeRules = state.unpickle[FieldReqTypeRules.ForTextField]
          CustomFieldCreateText(name, fieldReqTypeRules)
        }
      }

    private implicit val picklerCustomFieldUpdateImp: Pickler[CustomFieldUpdateImp] =
      new Pickler[CustomFieldUpdateImp] {
        override def pickle(a: CustomFieldUpdateImp)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.newValues)
        }
        override def unpickle(implicit state: UnpickleState): CustomFieldUpdateImp = {
          val id        = state.unpickle[CustomField.Implication.Id]
          val newValues = state.unpickle[CustomImpFieldGD.NonEmptyValues]
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
          val id        = state.unpickle[CustomField.Tag.Id]
          val newValues = state.unpickle[CustomTagFieldGD.NonEmptyValues]
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
          val id        = state.unpickle[CustomField.Text.Id]
          val newValues = state.unpickle[CustomTextFieldGD.NonEmptyValues]
          CustomFieldUpdateText(id, newValues)
        }
      }

    private implicit val picklerCustomFieldDelete: Pickler[CustomFieldDelete] =
      transformPickler(CustomFieldDelete.apply)(_.id)

    private implicit val picklerCustomFieldRestore: Pickler[CustomFieldRestore] =
      transformPickler(CustomFieldRestore.apply)(_.id)

    private implicit val picklerStaticFieldAdd: Pickler[StaticFieldAdd] =
      transformPickler(StaticFieldAdd.apply)(_.id)

    private implicit val picklerStaticFieldRemove: Pickler[StaticFieldRemove] =
      transformPickler(StaticFieldRemove.apply)(_.id)

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

    private implicit val picklerApplicableTagCreate: Pickler[ApplicableTagCreate] =
      transformPickler(ApplicableTagCreate.apply)(_.newValues)

    private implicit val picklerApplicableTagUpdate: Pickler[ApplicableTagUpdate] =
      new Pickler[ApplicableTagUpdate] {
        override def pickle(a: ApplicableTagUpdate)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.newValues)
        }
        override def unpickle(implicit state: UnpickleState): ApplicableTagUpdate = {
          val id        = state.unpickle[ApplicableTagId]
          val newValues = state.unpickle[ApplicableTagGD.NonEmptyValues]
          ApplicableTagUpdate(id, newValues)
        }
      }

    private implicit val picklerTagGroupCreate: Pickler[TagGroupCreate] =
      transformPickler(TagGroupCreate.apply)(_.newValues)

    private implicit val picklerTagGroupUpdate: Pickler[TagGroupUpdate] =
      new Pickler[TagGroupUpdate] {
        override def pickle(a: TagGroupUpdate)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.newValues)
        }
        override def unpickle(implicit state: UnpickleState): TagGroupUpdate = {
          val id        = state.unpickle[TagGroupId]
          val newValues = state.unpickle[TagGroupGD.NonEmptyValues]
          TagGroupUpdate(id, newValues)
        }
      }

    private implicit val picklerTagSetLiveChildrenOrder: Pickler[TagSetLiveChildrenOrder] =
      new Pickler[TagSetLiveChildrenOrder] {
        override def pickle(a: TagSetLiveChildrenOrder)(implicit state: PickleState): Unit = {
          state.pickle(a.id)
          state.pickle(a.children)
        }
        override def unpickle(implicit state: UnpickleState): TagSetLiveChildrenOrder = {
          val id       = state.unpickle[TagGroupId]
          val children = state.unpickle[Vector[ApplicableTagId]]
          TagSetLiveChildrenOrder(id, children)
        }
      }

    private implicit val picklerTagDelete: Pickler[TagDelete] =
      transformPickler(TagDelete.apply)(_.id)

    private implicit val picklerTagRestore: Pickler[TagRestore] =
      transformPickler(TagRestore.apply)(_.id)

    implicit val picklerUpdateConfigCmd: Pickler[UpdateConfigCmd] =
      new Pickler[UpdateConfigCmd] {
        private[this] final val KeyCustomFieldUpdateImp    = 1
        private[this] final val KeyCustomFieldUpdateTag    = 2
        private[this] final val KeyCustomFieldUpdateText   = 3
        private[this] final val KeyCustomIssueTypeCreate   = 4
        private[this] final val KeyCustomIssueTypeDelete   = 5
        private[this] final val KeyCustomIssueTypeRestore  = 6
        private[this] final val KeyCustomIssueTypeUpdate   = 7
        private[this] final val KeyCustomReqTypeCreate     = 8
      //private[this] final val KeyCustomReqTypeDelete     = 9
        private[this] final val KeyCustomReqTypeRestore    = 10
        private[this] final val KeyCustomReqTypeUpdate     = 11
      //private[this] final val KeyFieldDelete             = 12
      //private[this] final val KeyFieldRestore            = 13
        private[this] final val KeyFieldUpdateOrder        = 14
        private[this] final val KeyApplicableTagCreate     = 15
        private[this] final val KeyApplicableTagUpdate     = 16
        private[this] final val KeyTagDelete               = 17
        private[this] final val KeyTagGroupCreate          = 18
        private[this] final val KeyTagGroupUpdate          = 19
        private[this] final val KeyTagRestore              = 20
        private[this] final val KeyTagSetLiveChildrenOrder = 21
        private[this] final val KeyCustomFieldCreateImp    = 22
        private[this] final val KeyCustomFieldCreateTag    = 23
        private[this] final val KeyCustomFieldCreateText   = 24
        private[this] final val KeyCustomFieldDelete       = 25
        private[this] final val KeyCustomFieldRestore      = 26
        private[this] final val KeyStaticFieldAdd          = 27
        private[this] final val KeyStaticFieldRemove       = 28
        private[this] final val KeyCustomReqTypeDeleteHard = 29
        private[this] final val KeyCustomReqTypeDeleteSoft = 30
        override def pickle(a: UpdateConfigCmd)(implicit state: PickleState): Unit =
          a match {
            case b: CustomFieldCreateImp    => state.enc.writeByte(KeyCustomFieldCreateImp   ); state.pickle(b)
            case b: CustomFieldCreateTag    => state.enc.writeByte(KeyCustomFieldCreateTag   ); state.pickle(b)
            case b: CustomFieldCreateText   => state.enc.writeByte(KeyCustomFieldCreateText  ); state.pickle(b)
            case b: CustomFieldUpdateImp    => state.enc.writeByte(KeyCustomFieldUpdateImp   ); state.pickle(b)
            case b: CustomFieldUpdateTag    => state.enc.writeByte(KeyCustomFieldUpdateTag   ); state.pickle(b)
            case b: CustomFieldUpdateText   => state.enc.writeByte(KeyCustomFieldUpdateText  ); state.pickle(b)
            case b: CustomIssueTypeCreate   => state.enc.writeByte(KeyCustomIssueTypeCreate  ); state.pickle(b)
            case b: CustomIssueTypeDelete   => state.enc.writeByte(KeyCustomIssueTypeDelete  ); state.pickle(b)
            case b: CustomIssueTypeRestore  => state.enc.writeByte(KeyCustomIssueTypeRestore ); state.pickle(b)
            case b: CustomIssueTypeUpdate   => state.enc.writeByte(KeyCustomIssueTypeUpdate  ); state.pickle(b)
            case b: CustomReqTypeCreate     => state.enc.writeByte(KeyCustomReqTypeCreate    ); state.pickle(b)
            case b: CustomReqTypeDeleteHard => state.enc.writeByte(KeyCustomReqTypeDeleteHard); state.pickle(b)
            case b: CustomReqTypeDeleteSoft => state.enc.writeByte(KeyCustomReqTypeDeleteSoft); state.pickle(b)
            case b: CustomReqTypeRestore    => state.enc.writeByte(KeyCustomReqTypeRestore   ); state.pickle(b)
            case b: CustomReqTypeUpdate     => state.enc.writeByte(KeyCustomReqTypeUpdate    ); state.pickle(b)
            case b: CustomFieldDelete       => state.enc.writeByte(KeyCustomFieldDelete      ); state.pickle(b)
            case b: CustomFieldRestore      => state.enc.writeByte(KeyCustomFieldRestore     ); state.pickle(b)
            case b: StaticFieldAdd          => state.enc.writeByte(KeyStaticFieldAdd         ); state.pickle(b)
            case b: StaticFieldRemove       => state.enc.writeByte(KeyStaticFieldRemove      ); state.pickle(b)
            case b: FieldUpdateOrder        => state.enc.writeByte(KeyFieldUpdateOrder       ); state.pickle(b)
            case b: ApplicableTagCreate     => state.enc.writeByte(KeyApplicableTagCreate    ); state.pickle(b)
            case b: ApplicableTagUpdate     => state.enc.writeByte(KeyApplicableTagUpdate    ); state.pickle(b)
            case b: TagDelete               => state.enc.writeByte(KeyTagDelete              ); state.pickle(b)
            case b: TagGroupCreate          => state.enc.writeByte(KeyTagGroupCreate         ); state.pickle(b)
            case b: TagGroupUpdate          => state.enc.writeByte(KeyTagGroupUpdate         ); state.pickle(b)
            case b: TagRestore              => state.enc.writeByte(KeyTagRestore             ); state.pickle(b)
            case b: TagSetLiveChildrenOrder => state.enc.writeByte(KeyTagSetLiveChildrenOrder); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): UpdateConfigCmd =
          state.dec.readByte match {
            case KeyCustomFieldCreateImp    => state.unpickle[CustomFieldCreateImp]
            case KeyCustomFieldCreateTag    => state.unpickle[CustomFieldCreateTag]
            case KeyCustomFieldCreateText   => state.unpickle[CustomFieldCreateText]
            case KeyCustomFieldUpdateImp    => state.unpickle[CustomFieldUpdateImp]
            case KeyCustomFieldUpdateTag    => state.unpickle[CustomFieldUpdateTag]
            case KeyCustomFieldUpdateText   => state.unpickle[CustomFieldUpdateText]
            case KeyCustomIssueTypeCreate   => state.unpickle[CustomIssueTypeCreate]
            case KeyCustomIssueTypeDelete   => state.unpickle[CustomIssueTypeDelete]
            case KeyCustomIssueTypeRestore  => state.unpickle[CustomIssueTypeRestore]
            case KeyCustomIssueTypeUpdate   => state.unpickle[CustomIssueTypeUpdate]
            case KeyCustomReqTypeCreate     => state.unpickle[CustomReqTypeCreate]
            case KeyCustomReqTypeDeleteHard => state.unpickle[CustomReqTypeDeleteHard]
            case KeyCustomReqTypeDeleteSoft => state.unpickle[CustomReqTypeDeleteSoft]
            case KeyCustomReqTypeRestore    => state.unpickle[CustomReqTypeRestore]
            case KeyCustomReqTypeUpdate     => state.unpickle[CustomReqTypeUpdate]
            case KeyCustomFieldDelete       => state.unpickle[CustomFieldDelete]
            case KeyCustomFieldRestore      => state.unpickle[CustomFieldRestore]
            case KeyStaticFieldAdd          => state.unpickle[StaticFieldAdd]
            case KeyStaticFieldRemove       => state.unpickle[StaticFieldRemove]
            case KeyFieldUpdateOrder        => state.unpickle[FieldUpdateOrder]
            case KeyApplicableTagCreate     => state.unpickle[ApplicableTagCreate]
            case KeyApplicableTagUpdate     => state.unpickle[ApplicableTagUpdate]
            case KeyTagDelete               => state.unpickle[TagDelete]
            case KeyTagGroupCreate          => state.unpickle[TagGroupCreate]
            case KeyTagGroupUpdate          => state.unpickle[TagGroupUpdate]
            case KeyTagRestore              => state.unpickle[TagRestore]
            case KeyTagSetLiveChildrenOrder => state.unpickle[TagSetLiveChildrenOrder]
          }
      }
  }
}
