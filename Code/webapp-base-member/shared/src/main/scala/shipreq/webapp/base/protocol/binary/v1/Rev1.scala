package shipreq.webapp.base.protocol.binary.v1

import java.time.Instant
import scalaz.\/
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol.Version
import shipreq.webapp.base.protocol.binary.UnsupportedVersionException

/** v1.1
  *
  * Created because [[ApplicableTag]] lost and gained fields.
  */
object Rev1 {
  import boopickle.DefaultBasic._
  import BaseData._
  import Events._
  import BaseMemberData1._
  import BaseMemberData1.ReqTableDataPicklers._
  import BaseMemberData2._
  import PostEvents._

  implicit lazy val picklerColour: Pickler[Colour] =
    transformPickler(Colour.force)(_.value)

  implicit lazy val picklerApplicableTag: Pickler[ApplicableTag] =
    new Pickler[ApplicableTag] {
      override def pickle(a: ApplicableTag)(implicit state: PickleState): Unit = {
        state.enc.writeInt(1) // v1.1
        state.pickle(a.id)    // first byte is <=0 because of PicklerReuse
        state.pickle(a.key)
        state.pickle(a.desc)
        state.pickle(a.colour)
        state.pickle(a.applicableReqTypes)
        state.pickle(a.live)
      }
      override def unpickle(implicit state: UnpickleState): ApplicableTag = {
        state.dec.peek(_.readInt) match {

          // v1.1
          case 1 =>
            state.dec.readInt
            val id       = state.unpickle[ApplicableTagId]
            val key      = state.unpickle[HashRefKey]
            val desc     = state.unpickle[Option[String]]
            val colour   = state.unpickle[Option[Colour]]
            val reqTypes = state.unpickle[ApplicableReqTypes]
            val live     = state.unpickle[Live]
            ApplicableTag(id, key, desc, colour, reqTypes, live)

          // v1.0
          case n if n <= 0 =>
            val id   = state.unpickle[ApplicableTagId]
            val name = state.unpickle[String]
            val desc = state.unpickle[Option[String]]
            val key  = state.unpickle[HashRefKey]
            val live = state.unpickle[Live]
            ApplicableTag.v1(id, name, desc, key, live)

          case n =>
            throw UnsupportedVersionException(found = Version.v1(n), maxSupported = Version.v1(n))
        }
      }
    }

  implicit lazy val picklerTag: Pickler[Tag] =
    new Pickler[Tag] {
      private[this] final val KeyApplicableTag = 'a'
      private[this] final val KeyTagGroup      = 'g'
      override def pickle(a: Tag)(implicit state: PickleState): Unit =
        a match {
          case b: ApplicableTag => state.enc.writeByte(KeyApplicableTag); state.pickle(b)
          case b: TagGroup      => state.enc.writeByte(KeyTagGroup     ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): Tag =
        state.dec.readByte match {
          case KeyApplicableTag => state.unpickle[ApplicableTag]
          case KeyTagGroup      => state.unpickle[TagGroup]
        }
    }

  implicit lazy val picklerTagInTree: Pickler[TagInTree] =
    new Pickler[TagInTree] {
      override def pickle(a: TagInTree)(implicit state: PickleState): Unit = {
        state.pickle(a.tag)
        state.pickle(a.children)
      }
      override def unpickle(implicit state: UnpickleState): TagInTree = {
        val tag      = state.unpickle[Tag]
        val children = state.unpickle[TagInTree.Children]
        TagInTree(tag, children)
      }
    }

  implicit lazy val picklerTags: Pickler[Tags] =
    transformPickler(Tags.apply)(_.tree)

  implicit lazy val picklerTagTree: Pickler[TagTree] =
    pickleIMap(TagTree.empty)

  implicit def picklerFieldReqTypeRulesDefaultTo[D: Pickler]: Pickler[FieldReqTypeRules.Resolution.DefaultTo[D]] =
    transformPickler(FieldReqTypeRules.Resolution.DefaultTo.apply[D])(_.default)

  implicit def picklerFieldReqTypeRulesResolution[D](implicit p1: Pickler[FieldReqTypeRules.Resolution.DefaultTo[D]]): Pickler[FieldReqTypeRules.Resolution[D]] =
    new Pickler[FieldReqTypeRules.Resolution[D]] {
      private[this] final val KeyNotApplicable = 0
      private[this] final val KeyOptional      = 1
      private[this] final val KeyMandatory     = 2
      private[this] final val KeyDefaultTo     = 3
      override def pickle(a: FieldReqTypeRules.Resolution[D])(implicit state: PickleState): Unit =
        a match {
          case FieldReqTypeRules.Resolution.NotApplicable    => state.enc.writeByte(KeyNotApplicable)
          case FieldReqTypeRules.Resolution.Optional         => state.enc.writeByte(KeyOptional     )
          case FieldReqTypeRules.Resolution.Mandatory        => state.enc.writeByte(KeyMandatory    )
          case b: FieldReqTypeRules.Resolution.DefaultTo[D]  => state.enc.writeByte(KeyDefaultTo    ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): FieldReqTypeRules.Resolution[D] =
        state.dec.readByte match {
          case KeyNotApplicable => FieldReqTypeRules.Resolution.NotApplicable
          case KeyOptional      => FieldReqTypeRules.Resolution.Optional
          case KeyMandatory     => FieldReqTypeRules.Resolution.Mandatory
          case KeyDefaultTo     => state.unpickle[FieldReqTypeRules.Resolution.DefaultTo[D]]
        }
    }

  implicit def picklerFieldReqTypeRules[D: Pickler]: Pickler[FieldReqTypeRules[D]] =
    new Pickler[FieldReqTypeRules[D]] {
      override def pickle(a: FieldReqTypeRules[D])(implicit state: PickleState): Unit = {
        state.pickle(a.perReqType)
        state.pickle(a.otherwise)
      }
      override def unpickle(implicit state: UnpickleState): FieldReqTypeRules[D] = {
        val perReqType = state.unpickle[Map[ReqTypeId, FieldReqTypeRules.Resolution[D]]]
        val otherwise  = state.unpickle[FieldReqTypeRules.Resolution[D]]
        FieldReqTypeRules(perReqType, otherwise)
      }
    }

  implicit lazy val picklerCustomFieldImplication: Pickler[CustomField.Implication] =
    new Pickler[CustomField.Implication] {
      override def pickle(a: CustomField.Implication)(implicit state: PickleState): Unit = {
        state.enc.writeInt(1) // v1.1
        state.pickle(a.id)    // first byte is <=0 because of PicklerReuse
        state.pickle(a.reqTypeId)
        state.pickle(a.fieldReqTypeRules)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): CustomField.Implication =
        state.dec.peek(_.readInt) match {

          // v1.1
          case 1 =>
            state.dec.readInt
            val id             = state.unpickle[CustomField.Implication.Id]
            val reqTypeId      = state.unpickle[ReqTypeId]
            val reqTypes       = state.unpickle[FieldReqTypeRules.ForImpField]
            val liveExplicitly = state.unpickle[Live]
            CustomField.Implication(id, reqTypeId, reqTypes, liveExplicitly)

          // v1.0
          case n if n <= 0 =>
            val id             = state.unpickle[CustomField.Implication.Id]
            val reqTypeId      = state.unpickle[ReqTypeId]
            val mandatory      = state.unpickle[Mandatory]
            val reqTypes       = state.unpickle[ApplicableReqTypes]
            val liveExplicitly = state.unpickle[Live]
            CustomField.Implication.v1(id, reqTypeId, mandatory, reqTypes, liveExplicitly)

          case n =>
            throw UnsupportedVersionException(found = Version.v1(n), maxSupported = Version.v1(n))
        }
    }

  implicit lazy val picklerCustomFieldTag: Pickler[CustomField.Tag] =
    new Pickler[CustomField.Tag] {
      override def pickle(a: CustomField.Tag)(implicit state: PickleState): Unit = {
        state.enc.writeInt(1) // v1.1
        state.pickle(a.id)    // first byte is <=0 because of PicklerReuse
        state.pickle(a.tagId)
        state.pickle(a.fieldReqTypeRules)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): CustomField.Tag =
        state.dec.peek(_.readInt) match {

          // v1.1
          case 1 =>
            state.dec.readInt
            val id             = state.unpickle[CustomField.Tag.Id]
            val tagId          = state.unpickle[TagId]
            val reqTypes       = state.unpickle[FieldReqTypeRules.ForTagField]
            val liveExplicitly = state.unpickle[Live]
            CustomField.Tag(id, tagId, reqTypes, liveExplicitly)

          // v1.0
          case n if n <= 0 =>
            val id             = state.unpickle[CustomField.Tag.Id]
            val tagId          = state.unpickle[TagId]
            val mandatory      = state.unpickle[Mandatory]
            val reqTypes       = state.unpickle[ApplicableReqTypes]
            val liveExplicitly = state.unpickle[Live]
            CustomField.Tag.v1(id, tagId, mandatory, reqTypes, liveExplicitly)

          case n =>
            throw UnsupportedVersionException(found = Version.v1(n), maxSupported = Version.v1(n))
        }
    }

  implicit lazy val picklerCustomFieldText: Pickler[CustomField.Text] =
    new Pickler[CustomField.Text] {
      override def pickle(a: CustomField.Text)(implicit state: PickleState): Unit = {
        state.enc.writeInt(1) // v1.1
        state.pickle(a.id)    // first byte is <=0 because of PicklerReuse
        state.pickle(a.name)
        state.pickle(a.fieldReqTypeRules)
        state.pickle(a.liveExplicitly)
      }
      override def unpickle(implicit state: UnpickleState): CustomField.Text =
        state.dec.peek(_.readInt) match {

          // v1.1
          case 1 =>
            state.dec.readInt
            val id             = state.unpickle[CustomField.Text.Id]
            val name           = state.unpickle[String]
            val reqTypes       = state.unpickle[FieldReqTypeRules.ForTextField]
            val liveExplicitly = state.unpickle[Live]
            CustomField.Text(id, name, reqTypes, liveExplicitly)

          // v1.0
          case n if n <= 0 =>
            val id             = state.unpickle[CustomField.Text.Id]
            val name           = state.unpickle[String]
            val key            = state.unpickle[String]
            val mandatory      = state.unpickle[Mandatory]
            val reqTypes       = state.unpickle[ApplicableReqTypes]
            val liveExplicitly = state.unpickle[Live]
            CustomField.Text.v1(id, name, key, mandatory, reqTypes, liveExplicitly)

          case n =>
            throw UnsupportedVersionException(found = Version.v1(n), maxSupported = Version.v1(n))
        }
    }

  implicit lazy val picklerCustomField: Pickler[CustomField] =
    new Pickler[CustomField] {
      private[this] final val KeyImplication = 'i'
      private[this] final val KeyTag         = 't'
      private[this] final val KeyText        = 'x'
      override def pickle(a: CustomField)(implicit state: PickleState): Unit =
        a match {
          case b: CustomField.Implication => state.enc.writeByte(KeyImplication); state.pickle(b)
          case b: CustomField.Tag         => state.enc.writeByte(KeyTag        ); state.pickle(b)
          case b: CustomField.Text        => state.enc.writeByte(KeyText       ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): CustomField =
        state.dec.readByte match {
          case KeyImplication => state.unpickle[CustomField.Implication]
          case KeyTag         => state.unpickle[CustomField.Tag        ]
          case KeyText        => state.unpickle[CustomField.Text       ]
        }
    }

  implicit lazy val picklerFieldSet: Pickler[FieldSet] =
    new Pickler[FieldSet] {
      override def pickle(a: FieldSet)(implicit state: PickleState): Unit = {
        state.pickle(a.customFields)
        state.pickle(a.order)
      }
      override def unpickle(implicit state: UnpickleState): FieldSet = {
        val customFields = state.unpickle[FieldSet.CustomFields]
        val order        = state.unpickle[FieldSet.Order]
        FieldSet(customFields, order)
      }
    }

  implicit lazy val picklerFieldSetCustomFields: Pickler[FieldSet.CustomFields] =
    pickleIMap(FieldSet.emptyCustomFields)

  implicit lazy val picklerFieldType: Pickler[FieldType] =
    new Pickler[FieldType] {
      private[this] final val KeyImplication      = 'i'
      private[this] final val KeyImplicationGraph = 'I'
      private[this] final val KeyUseCaseStepGraph = 'G'
      private[this] final val KeyUseCaseSteps     = 'T'
      private[this] final val KeyTag              = 't'
      private[this] final val KeyText             = 'x'
      override def pickle(a: FieldType)(implicit state: PickleState): Unit =
        a match {
          case CustomFieldType.Implication      => state.enc.writeByte(KeyImplication     )
          case StaticFieldType.ImplicationGraph => state.enc.writeByte(KeyImplicationGraph)
          case StaticFieldType.UseCaseSteps     => state.enc.writeByte(KeyUseCaseSteps    )
          case StaticFieldType.UseCaseStepGraph => state.enc.writeByte(KeyUseCaseStepGraph)
          case CustomFieldType.Tag              => state.enc.writeByte(KeyTag             )
          case CustomFieldType.Text             => state.enc.writeByte(KeyText            )
        }
      override def unpickle(implicit state: UnpickleState): FieldType =
        state.dec.readByte match {
          case KeyImplication      => CustomFieldType.Implication
          case KeyImplicationGraph => StaticFieldType.ImplicationGraph
          case KeyUseCaseSteps     => StaticFieldType.UseCaseSteps
          case KeyUseCaseStepGraph => StaticFieldType.UseCaseStepGraph
          case KeyTag              => CustomFieldType.Tag
          case KeyText             => CustomFieldType.Text
        }
    }

  implicit lazy val picklerProject: Pickler[Project] =
    new Pickler[Project] {
      override def pickle(a: Project)(implicit state: PickleState): Unit = {
        state.pickle(a.name)
        state.pickle(a.config)
        state.pickle(a.content)
        state.pickle(a.manualIssues)
        state.pickle(a.reqtableViews)
        state.pickle(a.idCeilings)
      }
      override def unpickle(implicit state: UnpickleState): Project = {
        val name          = state.unpickle[Project.Name]
        val config        = state.unpickle[ProjectConfig]
        val content       = state.unpickle[ProjectContent]
        val manualIssues  = state.unpickle[ManualIssues]
        val reqtableViews = state.unpickle[reqtable.SavedViews.Optional]
        val idCeilings    = state.unpickle[IdCeilings]
        Project(name, config, content, manualIssues, reqtableViews, idCeilings)
      }
    }

  implicit lazy val picklerProjectConfig: Pickler[ProjectConfig] =
    new Pickler[ProjectConfig] {
      override def pickle(a: ProjectConfig)(implicit state: PickleState): Unit = {
        state.pickle(a.customIssueTypes)
        state.pickle(a.reqTypes)
        state.pickle(a.fields)
        state.pickle(a.tags)
      }
      override def unpickle(implicit state: UnpickleState): ProjectConfig = {
        val customIssueTypes = state.unpickle[CustomIssueTypeIMap]
        val reqTypes         = state.unpickle[ReqTypes]
        val fields           = state.unpickle[FieldSet]
        val tags             = state.unpickle[Tags]
        ProjectConfig(customIssueTypes, reqTypes, fields, tags)
      }
    }

  // ===================================================================================================================

  implicit lazy val pickleApplicableTagGD: Pickler[ApplicableTagGD.NonEmptyValues] = {
    import ApplicableTagGD._

    implicit val picklerValueForApplicableReqTypes = transformPickler(ValueForApplicableReqTypes.apply)(_.value)
    implicit val picklerValueForChildren           = transformPickler(ValueForChildren          .apply)(_.value)
    implicit val picklerValueForColour             = transformPickler(ValueForColour            .apply)(_.value)
    implicit val picklerValueForDesc               = transformPickler(ValueForDesc              .apply)(_.value)
    implicit val picklerValueForKey                = transformPickler(ValueForKey               .apply)(_.value)
    implicit val picklerValueForParents            = transformPickler(ValueForParents           .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyForApplicableReqTypes = 'r'
        private[this] final val KeyForChildren           = 'C'
        private[this] final val KeyForColour             = 'c'
        private[this] final val KeyForDesc               = 'd'
        private[this] final val KeyForKey                = '#'
        private[this] final val KeyForParents            = 'P'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForApplicableReqTypes => state.enc.writeByte(KeyForApplicableReqTypes); state.pickle(b)
            case b: ValueForChildren           => state.enc.writeByte(KeyForChildren          ); state.pickle(b)
            case b: ValueForColour             => state.enc.writeByte(KeyForColour            ); state.pickle(b)
            case b: ValueForDesc               => state.enc.writeByte(KeyForDesc              ); state.pickle(b)
            case b: ValueForKey                => state.enc.writeByte(KeyForKey               ); state.pickle(b)
            case b: ValueForParents            => state.enc.writeByte(KeyForParents           ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyForApplicableReqTypes => state.unpickle[ValueForApplicableReqTypes]
            case KeyForChildren           => state.unpickle[ValueForChildren]
            case KeyForColour             => state.unpickle[ValueForColour]
            case KeyForDesc               => state.unpickle[ValueForDesc]
            case KeyForKey                => state.unpickle[ValueForKey]
            case KeyForParents            => state.unpickle[ValueForParents]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit val pickleCustomImpFieldGD: Pickler[CustomImpFieldGD.NonEmptyValues] = {
    import CustomImpFieldGD._

    implicit val picklerValueForReqTypeId         = transformPickler(ValueForReqTypeId        .apply)(_.value)
    implicit val picklerValueForFieldReqTypeRules = transformPickler(ValueForFieldReqTypeRules.apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyReqTypeId = 'I'
        private[this] final val KeyReqTypes  = 'R'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForReqTypeId         => state.enc.writeByte(KeyReqTypeId); state.pickle(b)
            case b: ValueForFieldReqTypeRules => state.enc.writeByte(KeyReqTypes ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyReqTypeId => state.unpickle[ValueForReqTypeId]
            case KeyReqTypes  => state.unpickle[ValueForFieldReqTypeRules]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }


  implicit val pickleCustomTagFieldGD: Pickler[CustomTagFieldGD.NonEmptyValues] = {
    import CustomTagFieldGD._

    implicit val picklerValueForFieldReqTypeRules = transformPickler(ValueForFieldReqTypeRules.apply)(_.value)
    implicit val picklerValueForTagId             = transformPickler(ValueForTagId            .apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyReqTypes  = 'R'
        private[this] final val KeyTagId     = 'T'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForFieldReqTypeRules => state.enc.writeByte(KeyReqTypes ); state.pickle(b)
            case b: ValueForTagId             => state.enc.writeByte(KeyTagId    ); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyReqTypes  => state.unpickle[ValueForFieldReqTypeRules]
            case KeyTagId     => state.unpickle[ValueForTagId]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  implicit val pickleCustomTextFieldGD: Pickler[CustomTextFieldGD.NonEmptyValues] = {
    import CustomTextFieldGD._

    implicit val picklerValueForName              = transformPickler(ValueForName             .apply)(_.value)
    implicit val picklerValueForFieldReqTypeRules = transformPickler(ValueForFieldReqTypeRules.apply)(_.value)

    implicit val picklerValue: Pickler[Value] =
      new Pickler[Value] {
        private[this] final val KeyName      = 'N'
        private[this] final val KeyReqTypes  = 'R'
        override def pickle(a: Value)(implicit state: PickleState): Unit =
          a match {
            case b: ValueForName              => state.enc.writeByte(KeyName    ); state.pickle(b)
            case b: ValueForFieldReqTypeRules => state.enc.writeByte(KeyReqTypes); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): Value =
          state.dec.readByte match {
            case KeyName      => state.unpickle[ValueForName]
            case KeyReqTypes  => state.unpickle[ValueForFieldReqTypeRules]
          }
      }

    val values: Pickler[Values] = pickleIMap(emptyValues)
    pickleNonEmptyMono[Values](values, implicitly)
  }

  private[v1] implicit lazy val picklerEventApplicableTagCreate: Pickler[Event.ApplicableTagCreate] =
    new Pickler[Event.ApplicableTagCreate] {
      override def pickle(a: Event.ApplicableTagCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.ApplicableTagCreate = {
        val id = state.unpickle[ApplicableTagId]
        val vs = state.unpickle[ApplicableTagGD.NonEmptyValues]
        Event.ApplicableTagCreate(id, vs)
      }
    }

  private[v1] implicit lazy val picklerEventApplicableTagUpdate: Pickler[Event.ApplicableTagUpdate] =
    new Pickler[Event.ApplicableTagUpdate] {
      override def pickle(a: Event.ApplicableTagUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.ApplicableTagUpdate = {
        val id = state.unpickle[ApplicableTagId]
        val vs = state.unpickle[ApplicableTagGD.NonEmptyValues]
        Event.ApplicableTagUpdate(id, vs)
      }
    }

  private[v1] implicit lazy val picklerEventFieldCustomTextCreate: Pickler[Event.FieldCustomTextCreate] =
    new Pickler[Event.FieldCustomTextCreate] {
      override def pickle(a: Event.FieldCustomTextCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomTextCreate = {
        val id = state.unpickle[CustomField.Text.Id]
        val vs = state.unpickle[CustomTextFieldGD.NonEmptyValues]
        Event.FieldCustomTextCreate(id, vs)
      }
    }

  private[v1] implicit lazy val picklerEventFieldCustomTextUpdate: Pickler[Event.FieldCustomTextUpdate] =
    new Pickler[Event.FieldCustomTextUpdate] {
      override def pickle(a: Event.FieldCustomTextUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomTextUpdate = {
        val id = state.unpickle[CustomField.Text.Id]
        val vs = state.unpickle[CustomTextFieldGD.NonEmptyValues]
        Event.FieldCustomTextUpdate(id, vs)
      }
    }

  private[v1] implicit lazy val picklerEventFieldCustomTagCreate: Pickler[Event.FieldCustomTagCreate] =
    new Pickler[Event.FieldCustomTagCreate] {
      override def pickle(a: Event.FieldCustomTagCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomTagCreate = {
        val id = state.unpickle[CustomField.Tag.Id]
        val vs = state.unpickle[CustomTagFieldGD.NonEmptyValues]
        Event.FieldCustomTagCreate(id, vs)
      }
    }

  private[v1] implicit lazy val picklerEventFieldCustomTagUpdate: Pickler[Event.FieldCustomTagUpdate] =
    new Pickler[Event.FieldCustomTagUpdate] {
      override def pickle(a: Event.FieldCustomTagUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomTagUpdate = {
        val id = state.unpickle[CustomField.Tag.Id]
        val vs = state.unpickle[CustomTagFieldGD.NonEmptyValues]
        Event.FieldCustomTagUpdate(id, vs)
      }
    }

  private[v1] implicit lazy val picklerEventFieldCustomImpCreate: Pickler[Event.FieldCustomImpCreate] =
    new Pickler[Event.FieldCustomImpCreate] {
      override def pickle(a: Event.FieldCustomImpCreate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomImpCreate = {
        val id = state.unpickle[CustomField.Implication.Id]
        val vs = state.unpickle[CustomImpFieldGD.NonEmptyValues]
        Event.FieldCustomImpCreate(id, vs)
      }
    }

  private[v1] implicit lazy val picklerEventFieldCustomImpUpdate: Pickler[Event.FieldCustomImpUpdate] =
    new Pickler[Event.FieldCustomImpUpdate] {
      override def pickle(a: Event.FieldCustomImpUpdate)(implicit state: PickleState): Unit = {
        state.pickle(a.id)
        state.pickle(a.vs)
      }
      override def unpickle(implicit state: UnpickleState): Event.FieldCustomImpUpdate = {
        val id = state.unpickle[CustomField.Implication.Id]
        val vs = state.unpickle[CustomImpFieldGD.NonEmptyValues]
        Event.FieldCustomImpUpdate(id, vs)
      }
    }

  private[v1] implicit lazy val picklerEventCustomReqTypeDeleteHard: Pickler[Event.CustomReqTypeDeleteHard] =
    transformPickler(Event.CustomReqTypeDeleteHard.apply)(_.id)

  private[v1] implicit lazy val picklerEventCustomReqTypeDeleteSoft: Pickler[Event.CustomReqTypeDeleteSoft] =
    transformPickler(Event.CustomReqTypeDeleteSoft.apply)(_.id)

  implicit lazy val picklerEvent: Pickler[Event] =
    new Pickler[Event] {
      import Event._
      private[this] final val KeyApplicableTagCreateV1   = 0
      private[this] final val KeyApplicableTagUpdateV1   = 1
      private[this] final val KeyCodeGroupCreate         = 2
      private[this] final val KeyCodeGroupUpdate         = 3
      private[this] final val KeyCodeGroupsDelete        = 4
      private[this] final val KeyContentRestore          = 5
      private[this] final val KeyCustomIssueTypeCreate   = 6
      private[this] final val KeyCustomIssueTypeDelete   = 7
      private[this] final val KeyCustomIssueTypeRestore  = 8
      private[this] final val KeyCustomIssueTypeUpdate   = 9
      private[this] final val KeyCustomReqTypeCreate     = 10
      private[this] final val KeyCustomReqTypeDelete     = 11
      private[this] final val KeyCustomReqTypeRestore    = 12
      private[this] final val KeyCustomReqTypeUpdate     = 13
      private[this] final val KeyFieldCustomDelete       = 14
      private[this] final val KeyFieldCustomImpCreateV1  = 15
      private[this] final val KeyFieldCustomImpUpdateV1  = 16
      private[this] final val KeyFieldCustomRestore      = 17
      private[this] final val KeyFieldCustomTagCreateV1  = 18
      private[this] final val KeyFieldCustomTagUpdateV1  = 19
      private[this] final val KeyFieldCustomTextCreateV1 = 20
      private[this] final val KeyFieldCustomTextUpdateV1 = 21
      private[this] final val KeyFieldReposition         = 22
      private[this] final val KeyFieldStaticAdd          = 23
      private[this] final val KeyFieldStaticRemove       = 24
      private[this] final val KeyGenericReqCreate        = 25
      private[this] final val KeyGenericReqTitleSet      = 26
      private[this] final val KeyGenericReqTypeSet       = 27
      private[this] final val KeyManualIssueCreate       = 28
      private[this] final val KeyManualIssueDelete       = 29
      private[this] final val KeyManualIssueUpdate       = 30
      private[this] final val KeyProjectNameSet          = 31
      private[this] final val KeyProjectTemplateApply    = 32
      private[this] final val KeyReqCodesPatch           = 33
      private[this] final val KeyReqFieldCustomTextSet   = 34
      private[this] final val KeyReqImplicationsPatch    = 35
      private[this] final val KeyReqTagsPatch            = 36
      private[this] final val KeyReqsDelete              = 37
      private[this] final val KeySavedViewCreate         = 38
      private[this] final val KeySavedViewDefaultSet     = 39
      private[this] final val KeySavedViewDelete         = 40
      private[this] final val KeySavedViewUpdate         = 41
      private[this] final val KeyTagDelete               = 42
      private[this] final val KeyTagGroupCreate          = 43
      private[this] final val KeyTagGroupUpdate          = 44
      private[this] final val KeyTagRestore              = 45
      private[this] final val KeyUseCaseCreate           = 46
      private[this] final val KeyUseCaseStepCreate       = 47
      private[this] final val KeyUseCaseStepDelete       = 48
      private[this] final val KeyUseCaseStepRestore      = 49
      private[this] final val KeyUseCaseStepShiftLeft    = 50
      private[this] final val KeyUseCaseStepShiftRight   = 51
      private[this] final val KeyUseCaseStepUpdate       = 52
      private[this] final val KeyUseCaseTitleSet         = 53
      private[this] final val KeyApplicableTagCreate     = 54
      private[this] final val KeyApplicableTagUpdate     = 55
      private[this] final val KeyCustomReqTypeDeleteHard = 56
      private[this] final val KeyCustomReqTypeDeleteSoft = 57
      private[this] final val KeyFieldCustomImpCreate    = 58
      private[this] final val KeyFieldCustomImpUpdate    = 59
      private[this] final val KeyFieldCustomTagCreate    = 60
      private[this] final val KeyFieldCustomTagUpdate    = 61
      private[this] final val KeyFieldCustomTextCreate   = 62
      private[this] final val KeyFieldCustomTextUpdate   = 63

      override def pickle(a: Event)(implicit state: PickleState): Unit =
        a match {
          case b: ApplicableTagCreate     => state.enc.writeByte(KeyApplicableTagCreate    ); state.pickle(b)
          case b: ApplicableTagCreateV1   => state.enc.writeByte(KeyApplicableTagCreateV1  ); state.pickle(b)
          case b: ApplicableTagUpdate     => state.enc.writeByte(KeyApplicableTagUpdate    ); state.pickle(b)
          case b: ApplicableTagUpdateV1   => state.enc.writeByte(KeyApplicableTagUpdateV1  ); state.pickle(b)
          case b: CodeGroupCreate         => state.enc.writeByte(KeyCodeGroupCreate        ); state.pickle(b)
          case b: CodeGroupUpdate         => state.enc.writeByte(KeyCodeGroupUpdate        ); state.pickle(b)
          case b: CodeGroupsDelete        => state.enc.writeByte(KeyCodeGroupsDelete       ); state.pickle(b)
          case b: ContentRestore          => state.enc.writeByte(KeyContentRestore         ); state.pickle(b)
          case b: CustomIssueTypeCreate   => state.enc.writeByte(KeyCustomIssueTypeCreate  ); state.pickle(b)
          case b: CustomIssueTypeDelete   => state.enc.writeByte(KeyCustomIssueTypeDelete  ); state.pickle(b)
          case b: CustomIssueTypeRestore  => state.enc.writeByte(KeyCustomIssueTypeRestore ); state.pickle(b)
          case b: CustomIssueTypeUpdate   => state.enc.writeByte(KeyCustomIssueTypeUpdate  ); state.pickle(b)
          case b: CustomReqTypeCreate     => state.enc.writeByte(KeyCustomReqTypeCreate    ); state.pickle(b)
          case b: CustomReqTypeDelete     => state.enc.writeByte(KeyCustomReqTypeDelete    ); state.pickle(b)
          case b: CustomReqTypeDeleteHard => state.enc.writeByte(KeyCustomReqTypeDeleteHard); state.pickle(b)
          case b: CustomReqTypeDeleteSoft => state.enc.writeByte(KeyCustomReqTypeDeleteSoft); state.pickle(b)
          case b: CustomReqTypeRestore    => state.enc.writeByte(KeyCustomReqTypeRestore   ); state.pickle(b)
          case b: CustomReqTypeUpdate     => state.enc.writeByte(KeyCustomReqTypeUpdate    ); state.pickle(b)
          case b: FieldCustomDelete       => state.enc.writeByte(KeyFieldCustomDelete      ); state.pickle(b)
          case b: FieldCustomImpCreate    => state.enc.writeByte(KeyFieldCustomImpCreate   ); state.pickle(b)
          case b: FieldCustomImpCreateV1  => state.enc.writeByte(KeyFieldCustomImpCreateV1 ); state.pickle(b)
          case b: FieldCustomImpUpdate    => state.enc.writeByte(KeyFieldCustomImpUpdate   ); state.pickle(b)
          case b: FieldCustomImpUpdateV1  => state.enc.writeByte(KeyFieldCustomImpUpdateV1 ); state.pickle(b)
          case b: FieldCustomRestore      => state.enc.writeByte(KeyFieldCustomRestore     ); state.pickle(b)
          case b: FieldCustomTagCreate    => state.enc.writeByte(KeyFieldCustomTagCreate   ); state.pickle(b)
          case b: FieldCustomTagCreateV1  => state.enc.writeByte(KeyFieldCustomTagCreateV1 ); state.pickle(b)
          case b: FieldCustomTagUpdate    => state.enc.writeByte(KeyFieldCustomTagUpdate   ); state.pickle(b)
          case b: FieldCustomTagUpdateV1  => state.enc.writeByte(KeyFieldCustomTagUpdateV1 ); state.pickle(b)
          case b: FieldCustomTextCreate   => state.enc.writeByte(KeyFieldCustomTextCreate  ); state.pickle(b)
          case b: FieldCustomTextCreateV1 => state.enc.writeByte(KeyFieldCustomTextCreateV1); state.pickle(b)
          case b: FieldCustomTextUpdate   => state.enc.writeByte(KeyFieldCustomTextUpdate  ); state.pickle(b)
          case b: FieldCustomTextUpdateV1 => state.enc.writeByte(KeyFieldCustomTextUpdateV1); state.pickle(b)
          case b: FieldReposition         => state.enc.writeByte(KeyFieldReposition        ); state.pickle(b)
          case b: FieldStaticAdd          => state.enc.writeByte(KeyFieldStaticAdd         ); state.pickle(b)
          case b: FieldStaticRemove       => state.enc.writeByte(KeyFieldStaticRemove      ); state.pickle(b)
          case b: GenericReqCreate        => state.enc.writeByte(KeyGenericReqCreate       ); state.pickle(b)
          case b: GenericReqTitleSet      => state.enc.writeByte(KeyGenericReqTitleSet     ); state.pickle(b)
          case b: GenericReqTypeSet       => state.enc.writeByte(KeyGenericReqTypeSet      ); state.pickle(b)
          case b: ManualIssueCreate       => state.enc.writeByte(KeyManualIssueCreate      ); state.pickle(b)
          case b: ManualIssueDelete       => state.enc.writeByte(KeyManualIssueDelete      ); state.pickle(b)
          case b: ManualIssueUpdate       => state.enc.writeByte(KeyManualIssueUpdate      ); state.pickle(b)
          case b: ProjectNameSet          => state.enc.writeByte(KeyProjectNameSet         ); state.pickle(b)
          case b: ProjectTemplateApply    => state.enc.writeByte(KeyProjectTemplateApply   ); state.pickle(b)
          case b: ReqCodesPatch           => state.enc.writeByte(KeyReqCodesPatch          ); state.pickle(b)
          case b: ReqFieldCustomTextSet   => state.enc.writeByte(KeyReqFieldCustomTextSet  ); state.pickle(b)
          case b: ReqImplicationsPatch    => state.enc.writeByte(KeyReqImplicationsPatch   ); state.pickle(b)
          case b: ReqTagsPatch            => state.enc.writeByte(KeyReqTagsPatch           ); state.pickle(b)
          case b: ReqsDelete              => state.enc.writeByte(KeyReqsDelete             ); state.pickle(b)
          case b: SavedViewCreate         => state.enc.writeByte(KeySavedViewCreate        ); state.pickle(b)
          case b: SavedViewDefaultSet     => state.enc.writeByte(KeySavedViewDefaultSet    ); state.pickle(b)
          case b: SavedViewDelete         => state.enc.writeByte(KeySavedViewDelete        ); state.pickle(b)
          case b: SavedViewUpdate         => state.enc.writeByte(KeySavedViewUpdate        ); state.pickle(b)
          case b: TagDelete               => state.enc.writeByte(KeyTagDelete              ); state.pickle(b)
          case b: TagGroupCreate          => state.enc.writeByte(KeyTagGroupCreate         ); state.pickle(b)
          case b: TagGroupUpdate          => state.enc.writeByte(KeyTagGroupUpdate         ); state.pickle(b)
          case b: TagRestore              => state.enc.writeByte(KeyTagRestore             ); state.pickle(b)
          case b: UseCaseCreate           => state.enc.writeByte(KeyUseCaseCreate          ); state.pickle(b)
          case b: UseCaseStepCreate       => state.enc.writeByte(KeyUseCaseStepCreate      ); state.pickle(b)
          case b: UseCaseStepDelete       => state.enc.writeByte(KeyUseCaseStepDelete      ); state.pickle(b)
          case b: UseCaseStepRestore      => state.enc.writeByte(KeyUseCaseStepRestore     ); state.pickle(b)
          case b: UseCaseStepShiftLeft    => state.enc.writeByte(KeyUseCaseStepShiftLeft   ); state.pickle(b)
          case b: UseCaseStepShiftRight   => state.enc.writeByte(KeyUseCaseStepShiftRight  ); state.pickle(b)
          case b: UseCaseStepUpdate       => state.enc.writeByte(KeyUseCaseStepUpdate      ); state.pickle(b)
          case b: UseCaseTitleSet         => state.enc.writeByte(KeyUseCaseTitleSet        ); state.pickle(b)
        }

      override def unpickle(implicit state: UnpickleState): Event =
        state.dec.readByte match {
          case KeyApplicableTagCreate     => state.unpickle[ApplicableTagCreate]
          case KeyApplicableTagCreateV1   => state.unpickle[ApplicableTagCreateV1]
          case KeyApplicableTagUpdate     => state.unpickle[ApplicableTagUpdate]
          case KeyApplicableTagUpdateV1   => state.unpickle[ApplicableTagUpdateV1]
          case KeyCodeGroupCreate         => state.unpickle[CodeGroupCreate]
          case KeyCodeGroupUpdate         => state.unpickle[CodeGroupUpdate]
          case KeyCodeGroupsDelete        => state.unpickle[CodeGroupsDelete]
          case KeyContentRestore          => state.unpickle[ContentRestore]
          case KeyCustomIssueTypeCreate   => state.unpickle[CustomIssueTypeCreate]
          case KeyCustomIssueTypeDelete   => state.unpickle[CustomIssueTypeDelete]
          case KeyCustomIssueTypeRestore  => state.unpickle[CustomIssueTypeRestore]
          case KeyCustomIssueTypeUpdate   => state.unpickle[CustomIssueTypeUpdate]
          case KeyCustomReqTypeCreate     => state.unpickle[CustomReqTypeCreate]
          case KeyCustomReqTypeDelete     => state.unpickle[CustomReqTypeDelete]
          case KeyCustomReqTypeDeleteHard => state.unpickle[CustomReqTypeDeleteHard]
          case KeyCustomReqTypeDeleteSoft => state.unpickle[CustomReqTypeDeleteSoft]
          case KeyCustomReqTypeRestore    => state.unpickle[CustomReqTypeRestore]
          case KeyCustomReqTypeUpdate     => state.unpickle[CustomReqTypeUpdate]
          case KeyFieldCustomDelete       => state.unpickle[FieldCustomDelete]
          case KeyFieldCustomImpCreate    => state.unpickle[FieldCustomImpCreate]
          case KeyFieldCustomImpCreateV1  => state.unpickle[FieldCustomImpCreateV1]
          case KeyFieldCustomImpUpdate    => state.unpickle[FieldCustomImpUpdate]
          case KeyFieldCustomImpUpdateV1  => state.unpickle[FieldCustomImpUpdateV1]
          case KeyFieldCustomRestore      => state.unpickle[FieldCustomRestore]
          case KeyFieldCustomTagCreate    => state.unpickle[FieldCustomTagCreate]
          case KeyFieldCustomTagCreateV1  => state.unpickle[FieldCustomTagCreateV1]
          case KeyFieldCustomTagUpdate    => state.unpickle[FieldCustomTagUpdate]
          case KeyFieldCustomTagUpdateV1  => state.unpickle[FieldCustomTagUpdateV1]
          case KeyFieldCustomTextCreate   => state.unpickle[FieldCustomTextCreate]
          case KeyFieldCustomTextCreateV1 => state.unpickle[FieldCustomTextCreateV1]
          case KeyFieldCustomTextUpdate   => state.unpickle[FieldCustomTextUpdate]
          case KeyFieldCustomTextUpdateV1 => state.unpickle[FieldCustomTextUpdateV1]
          case KeyFieldReposition         => state.unpickle[FieldReposition]
          case KeyFieldStaticAdd          => state.unpickle[FieldStaticAdd]
          case KeyFieldStaticRemove       => state.unpickle[FieldStaticRemove]
          case KeyGenericReqCreate        => state.unpickle[GenericReqCreate]
          case KeyGenericReqTitleSet      => state.unpickle[GenericReqTitleSet]
          case KeyGenericReqTypeSet       => state.unpickle[GenericReqTypeSet]
          case KeyManualIssueCreate       => state.unpickle[ManualIssueCreate]
          case KeyManualIssueDelete       => state.unpickle[ManualIssueDelete]
          case KeyManualIssueUpdate       => state.unpickle[ManualIssueUpdate]
          case KeyProjectNameSet          => state.unpickle[ProjectNameSet]
          case KeyProjectTemplateApply    => state.unpickle[ProjectTemplateApply]
          case KeyReqCodesPatch           => state.unpickle[ReqCodesPatch]
          case KeyReqFieldCustomTextSet   => state.unpickle[ReqFieldCustomTextSet]
          case KeyReqImplicationsPatch    => state.unpickle[ReqImplicationsPatch]
          case KeyReqTagsPatch            => state.unpickle[ReqTagsPatch]
          case KeyReqsDelete              => state.unpickle[ReqsDelete]
          case KeySavedViewCreate         => state.unpickle[SavedViewCreate]
          case KeySavedViewDefaultSet     => state.unpickle[SavedViewDefaultSet]
          case KeySavedViewDelete         => state.unpickle[SavedViewDelete]
          case KeySavedViewUpdate         => state.unpickle[SavedViewUpdate]
          case KeyTagDelete               => state.unpickle[TagDelete]
          case KeyTagGroupCreate          => state.unpickle[TagGroupCreate]
          case KeyTagGroupUpdate          => state.unpickle[TagGroupUpdate]
          case KeyTagRestore              => state.unpickle[TagRestore]
          case KeyUseCaseCreate           => state.unpickle[UseCaseCreate]
          case KeyUseCaseStepCreate       => state.unpickle[UseCaseStepCreate]
          case KeyUseCaseStepDelete       => state.unpickle[UseCaseStepDelete]
          case KeyUseCaseStepRestore      => state.unpickle[UseCaseStepRestore]
          case KeyUseCaseStepShiftLeft    => state.unpickle[UseCaseStepShiftLeft]
          case KeyUseCaseStepShiftRight   => state.unpickle[UseCaseStepShiftRight]
          case KeyUseCaseStepUpdate       => state.unpickle[UseCaseStepUpdate]
          case KeyUseCaseTitleSet         => state.unpickle[UseCaseTitleSet]
        }
    }

  implicit lazy val picklerActiveEvent: Pickler[ActiveEvent] =
    picklerEvent.narrow

  // ===================================================================================================================

  implicit lazy val picklerVerifiedEvent: Pickler[VerifiedEvent] =
    new Pickler[VerifiedEvent] {
      override def pickle(a: VerifiedEvent)(implicit state: PickleState): Unit = {
        state.pickle(a.ord)
        state.pickle(a.event)
        state.pickle(a.createdAt)
      }
      override def unpickle(implicit state: UnpickleState): VerifiedEvent = {
        val ord       = state.unpickle[EventOrd]
        val event     = state.unpickle[Event]
        val createdAt = state.unpickle[Instant]
        VerifiedEvent(ord, event, createdAt)
      }
    }

  implicit lazy val picklerVerifiedEventSeq: Pickler[VerifiedEvent.Seq] =
    iterablePickler

  implicit lazy val picklerVerifiedEventNonEmptySeq: Pickler[VerifiedEvent.NonEmptySeq] =
    new Pickler[VerifiedEvent.NonEmptySeq] {
      override def pickle(a: VerifiedEvent.NonEmptySeq)(implicit state: PickleState): Unit = {
        state.pickle(a.head)
        state.pickle(a.tail)
      }
      override def unpickle(implicit state: UnpickleState): VerifiedEvent.NonEmptySeq = {
        val head = state.unpickle[VerifiedEvent]
        val tail = state.unpickle[VerifiedEvent.Seq]
        VerifiedEvent.NonEmptySeq(head, tail)
      }
    }

  implicit lazy val pickleErrorMsgOrVerifiedEventSeq: Pickler[ErrorMsg \/ VerifiedEvent.Seq] =
    pickleDisj

  implicit lazy val pickleProjectAndOrd: Pickler[ProjectAndOrd] =
    new Pickler[ProjectAndOrd] {
      override def pickle(a: ProjectAndOrd)(implicit state: PickleState): Unit = {
        state.pickle(a.project)
        state.pickle(a.ord)
      }
      override def unpickle(implicit state: UnpickleState): ProjectAndOrd = {
        val project = state.unpickle[Project]
        val ord     = state.unpickle[Option[EventOrd.Latest]]
        ProjectAndOrd(project, ord)
      }
    }

}
