package shipreq.webapp.base.protocol.json.v1

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.ParseInt
import shipreq.base.util.JsonUtil._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview.SavedView
import shipreq.webapp.base.event.RetiredGenericData._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol.json.JsonCodec

object Events {
  import JsonCodec.Implicits._
  import BaseData._
  import BaseMemberData1._
  import BaseMemberData1.SavedViewCodecs._

  private[v1] implicit val codecNonEmptySetApplicableTagId    : JsonCodec[NonEmptySet[ApplicableTagId]     ] = codecNES
  private[v1] implicit val codecNonEmptySetReqCodeGroupId     : JsonCodec[NonEmptySet[ReqCodeGroupId]      ] = codecNES
  private[v1] implicit val codecNonEmptySetApReqCodeIdAndValue: JsonCodec[NonEmptySet[ApReqCodeId.AndValue]] = codecNES
  private[v1] implicit val codecNonEmptySetReqId              : JsonCodec[NonEmptySet[ReqId]               ] = codecNES
  private[v1] implicit val codecSetDiffUseCaseStepId          : JsonCodec[SetDiff[UseCaseStepId]           ] = codecSetDiff
  private[v1] implicit val codecSetDiffApplicableTagId        : JsonCodec[SetDiff[ApplicableTagId]         ] = codecSetDiff
  private[v1] implicit val codecSetDiffReqId                  : JsonCodec[SetDiff[ReqId]                   ] = codecSetDiff
  private[v1] implicit val codecSetDiffNEUseCaseStepId        : JsonCodec[SetDiff.NE[UseCaseStepId]        ] = codecNonEmptyMono
  private[v1] implicit val codecSetDiffNEApplicableTagId      : JsonCodec[SetDiff.NE[ApplicableTagId]      ] = codecNonEmptyMono
  private[v1] implicit val codecSetDiffNEReqId                : JsonCodec[SetDiff.NE[ReqId]                ] = codecNonEmptyMono

  private[v1] implicit val codecProjectTemplate: JsonCodec[ProjectTemplate] =
    JsonCodec.enumAdt(AdtMacros.adtIsoSet[ProjectTemplate, Int] {
      case ProjectTemplate.V1 => 1
      // Don't mindlessly add new cases here. When a new case is added the codec-evolution doc needs to be followed
    })

  private[v1] implicit val keyDecoderTagId: KeyDecoder[TagId] =
    KeyDecoder.instance(k =>
      (k.headOption, k.drop(1)) match {
        case (Some('a'), ParseInt(i)) => Some(ApplicableTagId(i))
        case (Some('g'), ParseInt(i)) => Some(TagGroupId(i))
        case (_        , _          ) => None
      }
    )

  private[v1] implicit val keyEncoderTagId: KeyEncoder[TagId] =
    KeyEncoder.instance {
      case ApplicableTagId(i) => "a" + i
      case TagGroupId     (i) => "g" + i
    }

  // ===================================================================================================================
  // GenericData

  private[v1] implicit val codecApplicableTagGDv1: JsonCodec[ApplicableTagGDv1.NonEmptyValues] = {
    import ApplicableTagGDv1._

    implicit val codecValueForChildren = JsonCodec.xmap(ValueForChildren.apply)(_.value)
    implicit val codecValueForDesc     = JsonCodec.xmap(ValueForDesc    .apply)(_.value)
    implicit val codecValueForKey      = JsonCodec.xmap(ValueForKey     .apply)(_.value)
    implicit val codecValueForName     = JsonCodec.xmap(ValueForName    .apply)(_.value)
    implicit val codecValueForParents  = JsonCodec.xmap(ValueForParents .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("children", c) => c.as[ValueForChildren]
      case ("desc"    , c) => c.as[ValueForDesc]
      case ("key"     , c) => c.as[ValueForKey]
      case ("name"    , c) => c.as[ValueForName]
      case ("parents" , c) => c.as[ValueForParents]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForChildren => Json.obj("children" -> a.asJson)
      case a: ValueForDesc     => Json.obj("desc"     -> a.asJson)
      case a: ValueForKey      => Json.obj("key"      -> a.asJson)
      case a: ValueForName     => Json.obj("name"     -> a.asJson)
      case a: ValueForParents  => Json.obj("parents"  -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit val codecCustomReqTypeGDv1: JsonCodec[CustomReqTypeGDv1.NonEmptyValues] = {
    import CustomReqTypeGDv1._

    implicit val codecValueForImplication = JsonCodec.xmap(ValueForImplication.apply)(_.value)
    implicit val codecValueForMnemonic    = JsonCodec.xmap(ValueForMnemonic   .apply)(_.value)
    implicit val codecValueForName        = JsonCodec.xmap(ValueForName       .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("imp"     , c) => c.as[ValueForImplication]
      case ("mnemonic", c) => c.as[ValueForMnemonic]
      case ("name"    , c) => c.as[ValueForName]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForImplication => Json.obj("imp"      -> a.asJson)
      case a: ValueForMnemonic    => Json.obj("mnemonic" -> a.asJson)
      case a: ValueForName        => Json.obj("name"     -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit val codecTagGroupGD: JsonCodec[TagGroupGD.NonEmptyValues] = {
    import TagGroupGD._

    implicit val codecValueForChildren    = JsonCodec.xmap(ValueForChildren   .apply)(_.value)
    implicit val codecValueForDesc        = JsonCodec.xmap(ValueForDesc       .apply)(_.value)
    implicit val codecValueForExclusivity = JsonCodec.xmap(ValueForExclusivity.apply)(_.value)
    implicit val codecValueForName        = JsonCodec.xmap(ValueForName       .apply)(_.value)
    implicit val codecValueForParents     = JsonCodec.xmap(ValueForParents    .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("children"     , c) => c.as[ValueForChildren]
      case ("desc"         , c) => c.as[ValueForDesc]
      case ("exclusivity"  , c) => c.as[ValueForExclusivity]
      case ("name"         , c) => c.as[ValueForName]
      case ("parents"      , c) => c.as[ValueForParents]
      case ("mutexChildren", c) => c.as[ValueForExclusivity] // old name, must remain for backward-compatibility
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForChildren    => Json.obj("children"    -> a.asJson)
      case a: ValueForDesc        => Json.obj("desc"        -> a.asJson)
      case a: ValueForExclusivity => Json.obj("exclusivity" -> a.asJson)
      case a: ValueForName        => Json.obj("name"        -> a.asJson)
      case a: ValueForParents     => Json.obj("parents"     -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  // ===================================================================================================================
  // Events

  object EventData {

    implicit val decoderEventProjectNameSet: Decoder[Event.ProjectNameSet] =
      Decoder[String].map(Event.ProjectNameSet.apply)

    implicit val encoderEventProjectNameSet: Encoder[Event.ProjectNameSet] =
      Encoder[String].contramap(_.name)

    implicit val decoderEventProjectTemplateApply: Decoder[Event.ProjectTemplateApply] =
      Decoder[ProjectTemplate].map(Event.ProjectTemplateApply.apply)

    implicit val encoderEventProjectTemplateApply: Encoder[Event.ProjectTemplateApply] =
      Encoder[ProjectTemplate].contramap(_.template)

    implicit val decoderEventCustomIssueTypeDelete: Decoder[Event.CustomIssueTypeDelete] =
      Decoder[CustomIssueTypeId].map(Event.CustomIssueTypeDelete.apply)

    implicit val encoderEventCustomIssueTypeDelete: Encoder[Event.CustomIssueTypeDelete] =
      Encoder[CustomIssueTypeId].contramap(_.id)

    implicit val decoderEventCustomIssueTypeRestore: Decoder[Event.CustomIssueTypeRestore] =
      Decoder[CustomIssueTypeId].map(Event.CustomIssueTypeRestore.apply)

    implicit val encoderEventCustomIssueTypeRestore: Encoder[Event.CustomIssueTypeRestore] =
      Encoder[CustomIssueTypeId].contramap(_.id)

    implicit val decoderEventCustomReqTypeCreateV1: Decoder[Event.CustomReqTypeCreateV1] =
      Decoder.forProduct2("id", "values")(Event.CustomReqTypeCreateV1.apply)

    implicit val encoderEventCustomReqTypeCreateV1: Encoder[Event.CustomReqTypeCreateV1] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventCustomReqTypeUpdateV1: Decoder[Event.CustomReqTypeUpdateV1] =
      Decoder.forProduct2("id", "values")(Event.CustomReqTypeUpdateV1.apply)

    implicit val encoderEventCustomReqTypeUpdateV1: Encoder[Event.CustomReqTypeUpdateV1] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventCustomReqTypeDelete: Decoder[Event.CustomReqTypeDelete] =
      Decoder[CustomReqTypeId].map(Event.CustomReqTypeDelete.apply)

    implicit val encoderEventCustomReqTypeDelete: Encoder[Event.CustomReqTypeDelete] =
      Encoder[CustomReqTypeId].contramap(_.id)

    implicit val decoderEventCustomReqTypeRestore: Decoder[Event.CustomReqTypeRestore] =
      Decoder[CustomReqTypeId].map(Event.CustomReqTypeRestore.apply)

    implicit val encoderEventCustomReqTypeRestore: Encoder[Event.CustomReqTypeRestore] =
      Encoder[CustomReqTypeId].contramap(_.id)

    implicit val decoderEventTagDelete: Decoder[Event.TagDelete] =
      Decoder[TagId].map(Event.TagDelete.apply)

    implicit val encoderEventTagDelete: Encoder[Event.TagDelete] =
      Encoder[TagId].contramap(_.id)

    implicit val decoderEventTagRestore: Decoder[Event.TagRestore] =
      Decoder[TagId].map(Event.TagRestore.apply)

    implicit val encoderEventTagRestore: Encoder[Event.TagRestore] =
      Encoder[TagId].contramap(_.id)

    implicit val decoderEventTagGroupCreate: Decoder[Event.TagGroupCreate] =
      Decoder.forProduct2("id", "values")(Event.TagGroupCreate.apply)

    implicit val encoderEventTagGroupCreate: Encoder[Event.TagGroupCreate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventTagGroupUpdate: Decoder[Event.TagGroupUpdate] =
      Decoder.forProduct2("id", "values")(Event.TagGroupUpdate.apply)

    implicit val encoderEventTagGroupUpdate: Encoder[Event.TagGroupUpdate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventApplicableTagCreateV1: Decoder[Event.ApplicableTagCreateV1] =
      Decoder.forProduct2("id", "values")(Event.ApplicableTagCreateV1.apply)

    implicit val encoderEventApplicableTagCreateV1: Encoder[Event.ApplicableTagCreateV1] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventApplicableTagUpdateV1: Decoder[Event.ApplicableTagUpdateV1] =
      Decoder.forProduct2("id", "values")(Event.ApplicableTagUpdateV1.apply)

    implicit val encoderEventApplicableTagUpdateV1: Encoder[Event.ApplicableTagUpdateV1] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomDelete: Decoder[Event.FieldCustomDelete] =
      Decoder[CustomFieldId].map(Event.FieldCustomDelete.apply)

    implicit val encoderEventFieldCustomDelete: Encoder[Event.FieldCustomDelete] =
      Encoder[CustomFieldId].contramap(_.id)

    implicit val decoderEventFieldCustomRestore: Decoder[Event.FieldCustomRestore] =
      Decoder[CustomFieldId].map(Event.FieldCustomRestore.apply)

    implicit val encoderEventFieldCustomRestore: Encoder[Event.FieldCustomRestore] =
      Encoder[CustomFieldId].contramap(_.id)

    implicit val decoderEventGenericReqTypeSet: Decoder[Event.GenericReqTypeSet] =
      Decoder.forProduct2("id", "value")(Event.GenericReqTypeSet.apply)

    implicit val encoderEventGenericReqTypeSet: Encoder[Event.GenericReqTypeSet] =
      Encoder.forProduct2("id", "value")(a => (a.id, a.value))

    implicit val decoderEventUseCaseStepCreate: Decoder[Event.UseCaseStepCreate] =
      Decoder.forProduct4("id", "ucId", "field", "at")(Event.UseCaseStepCreate.apply)

    implicit val encoderEventUseCaseStepCreate: Encoder[Event.UseCaseStepCreate] =
      Encoder.forProduct4("id", "ucId", "field", "at")(a => (a.id, a.ucId, a.field, a.at))

    implicit val decoderEventUseCaseStepShiftLeft: Decoder[Event.UseCaseStepShiftLeft] =
      Decoder[UseCaseStepId].map(Event.UseCaseStepShiftLeft.apply)

    implicit val encoderEventUseCaseStepShiftLeft: Encoder[Event.UseCaseStepShiftLeft] =
      Encoder[UseCaseStepId].contramap(_.id)

    implicit val decoderEventUseCaseStepShiftRight: Decoder[Event.UseCaseStepShiftRight] =
      Decoder[UseCaseStepId].map(Event.UseCaseStepShiftRight.apply)

    implicit val encoderEventUseCaseStepShiftRight: Encoder[Event.UseCaseStepShiftRight] =
      Encoder[UseCaseStepId].contramap(_.id)

    implicit val decoderEventUseCaseStepDelete: Decoder[Event.UseCaseStepDelete] =
      Decoder[UseCaseStepId].map(Event.UseCaseStepDelete.apply)

    implicit val encoderEventUseCaseStepDelete: Encoder[Event.UseCaseStepDelete] =
      Encoder[UseCaseStepId].contramap(_.id)

    implicit val decoderEventUseCaseStepRestore: Decoder[Event.UseCaseStepRestore] =
      Decoder[UseCaseStepId].map(Event.UseCaseStepRestore.apply)

    implicit val encoderEventUseCaseStepRestore: Encoder[Event.UseCaseStepRestore] =
      Encoder[UseCaseStepId].contramap(_.id)

    implicit val decoderEventCodeGroupsDelete: Decoder[Event.CodeGroupsDelete] =
      Decoder[NonEmptySet[ReqCodeGroupId]].map(Event.CodeGroupsDelete.apply)

    implicit val encoderEventCodeGroupsDelete: Encoder[Event.CodeGroupsDelete] =
      Encoder[NonEmptySet[ReqCodeGroupId]].contramap(_.ids)

    implicit val decoderEventReqCodesPatch: Decoder[Event.ReqCodesPatch] =
      Decoder.forProduct4("id", "remove", "restore", "add")(Event.ReqCodesPatch.apply)

    implicit val encoderEventReqCodesPatch: Encoder[Event.ReqCodesPatch] =
      Encoder.forProduct4("id", "remove", "restore", "add")(a => (a.id, a.remove, a.restore, a.add))

    implicit val decoderEventReqTagsPatch: Decoder[Event.ReqTagsPatch] =
      Decoder.forProduct2("id", "patch")(Event.ReqTagsPatch.apply)

    implicit val encoderEventReqTagsPatch: Encoder[Event.ReqTagsPatch] =
      Encoder.forProduct2("id", "patch")(a => (a.id, a.patch))

    implicit val decoderEventReqImplicationsPatch: Decoder[Event.ReqImplicationsPatch] =
      Decoder.forProduct3("id", "dir", "patch")(Event.ReqImplicationsPatch.apply)

    implicit val encoderEventReqImplicationsPatch: Encoder[Event.ReqImplicationsPatch] =
      Encoder.forProduct3("id", "dir", "patch")(a => (a.id, a.dir, a.patch))

    implicit val decoderEventContentRestore: Decoder[Event.ContentRestore] =
      Decoder.forProduct2("reqs", "codeGroups")(Event.ContentRestore.apply)

    implicit val encoderEventContentRestore: Encoder[Event.ContentRestore] =
      Encoder.forProduct2("reqs", "codeGroups")(a => (a.reqs, a.codeGroups))

    implicit val decoderEventManualIssueDelete: Decoder[Event.ManualIssueDelete] =
      Decoder[ManualIssueId].map(Event.ManualIssueDelete.apply)

    implicit val encoderEventManualIssueDelete: Encoder[Event.ManualIssueDelete] =
      Encoder[ManualIssueId].contramap(_.id)

    implicit val decoderEventSavedViewDelete: Decoder[Event.SavedViewDelete] =
      Decoder[SavedView.Id].map(Event.SavedViewDelete.apply)

    implicit val encoderEventSavedViewDelete: Encoder[Event.SavedViewDelete] =
      Encoder[SavedView.Id].contramap(_.id)

    implicit val decoderEventSavedViewDefaultSet: Decoder[Event.SavedViewDefaultSet] =
      Decoder[SavedView.Id].map(Event.SavedViewDefaultSet.apply)

    implicit val encoderEventSavedViewDefaultSet: Encoder[Event.SavedViewDefaultSet] =
      Encoder[SavedView.Id].contramap(_.id)
  }
}
