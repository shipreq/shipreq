package shipreq.webapp.base.protocol.json.v1

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.ParseInt
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable.SavedView
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol.json.JsonCodec

object Events {
  import JsonCodec.Implicits._
  import BaseData._
  import BaseMemberData1._
  import BaseMemberData1.AtomCodecs.instances._
  import BaseMemberData1.ReqTableDataCodecs._

  private implicit val codecEventNonEmptyCustomTextMap    : JsonCodec[Event.NonEmptyCustomTextMap      ] = codecNonEmptyMono
  private implicit val codecNonEmptySetApplicableTagId    : JsonCodec[NonEmptySet[ApplicableTagId]     ] = codecNES
  private implicit val codecNonEmptySetReqCodeGroupId     : JsonCodec[NonEmptySet[ReqCodeGroupId]      ] = codecNES
  private implicit val codecNonEmptySetApReqCodeIdAndValue: JsonCodec[NonEmptySet[ApReqCodeId.AndValue]] = codecNES
  private implicit val codecNonEmptySetReqId              : JsonCodec[NonEmptySet[ReqId]               ] = codecNES
  private implicit val codecSetDiffUseCaseStepId          : JsonCodec[SetDiff[UseCaseStepId]           ] = codecSetDiff
  private implicit val codecSetDiffApplicableTagId        : JsonCodec[SetDiff[ApplicableTagId]         ] = codecSetDiff
  private implicit val codecSetDiffReqId                  : JsonCodec[SetDiff[ReqId]                   ] = codecSetDiff
  private implicit val codecSetDiffNEUseCaseStepId        : JsonCodec[SetDiff.NE[UseCaseStepId]        ] = codecNonEmptyMono
  private implicit val codecSetDiffNEApplicableTagId      : JsonCodec[SetDiff.NE[ApplicableTagId]      ] = codecNonEmptyMono
  private implicit val codecSetDiffNEReqId                : JsonCodec[SetDiff.NE[ReqId]                ] = codecNonEmptyMono

  private implicit val codecProjectTemplate: JsonCodec[ProjectTemplate] =
    JsonCodec.enumAdt(AdtMacros.adtIsoSet[ProjectTemplate, Int] {
      case ProjectTemplate.V1 => 1
      case ProjectTemplate.V2 => 2
    })

  private implicit val keyDecoderTagId: KeyDecoder[TagId] =
    KeyDecoder.instance(k =>
      (k.headOption, k.drop(1)) match {
        case (Some('a'), ParseInt(i)) => Some(ApplicableTagId(i))
        case (Some('g'), ParseInt(i)) => Some(TagGroupId(i))
        case (_        , _          ) => None
      }
    )

  private implicit val keyEncoderTagId: KeyEncoder[TagId] =
    KeyEncoder.instance {
      case ApplicableTagId(i) => "a" + i
      case TagGroupId     (i) => "g" + i
    }

  // ===================================================================================================================
  // GenericData

  private implicit val codecApplicableTagGD: JsonCodec[ApplicableTagGD.NonEmptyValues] = {
    import ApplicableTagGD._

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

  private implicit val codecCodeGroupGD: JsonCodec[CodeGroupGD.NonEmptyValues] = {
    import CodeGroupGD._

    implicit val codecValueForCode  = JsonCodec.xmap(ValueForCode .apply)(_.value)
    implicit val codecValueForTitle = JsonCodec.xmap(ValueForTitle.apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("code" , c) => c.as[ValueForCode]
      case ("title", c) => c.as[ValueForTitle]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForCode  => Json.obj("code"  -> a.asJson)
      case a: ValueForTitle => Json.obj("title" -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private implicit val codecCustomImpFieldGD: JsonCodec[CustomImpFieldGD.NonEmptyValues] = {
    import CustomImpFieldGD._

    implicit val codecValueForMandatory = JsonCodec.xmap(ValueForMandatory.apply)(_.value)
    implicit val codecValueForReqTypeId = JsonCodec.xmap(ValueForReqTypeId.apply)(_.value)
    implicit val codecValueForReqTypes  = JsonCodec.xmap(ValueForReqTypes .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("mandatory", c) => c.as[ValueForMandatory]
      case ("reqTypeId", c) => c.as[ValueForReqTypeId]
      case ("reqTypes" , c) => c.as[ValueForReqTypes]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForMandatory => Json.obj("mandatory" -> a.asJson)
      case a: ValueForReqTypeId => Json.obj("reqTypeId" -> a.asJson)
      case a: ValueForReqTypes  => Json.obj("reqTypes"  -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private implicit val codecCustomIssueTypeGD: JsonCodec[CustomIssueTypeGD.NonEmptyValues] = {
    import CustomIssueTypeGD._

    implicit val codecValueForDesc = JsonCodec.xmap(ValueForDesc.apply)(_.value)
    implicit val codecValueForKey  = JsonCodec.xmap(ValueForKey .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("desc", c) => c.as[ValueForDesc]
      case ("key" , c) => c.as[ValueForKey]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForDesc => Json.obj("desc" -> a.asJson)
      case a: ValueForKey  => Json.obj("key"  -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private implicit val codecCustomReqTypeGD: JsonCodec[CustomReqTypeGD.NonEmptyValues] = {
    import CustomReqTypeGD._

    implicit val codecValueForImp      = JsonCodec.xmap(ValueForImp     .apply)(_.value)
    implicit val codecValueForMnemonic = JsonCodec.xmap(ValueForMnemonic.apply)(_.value)
    implicit val codecValueForName     = JsonCodec.xmap(ValueForName    .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("imp"     , c) => c.as[ValueForImp]
      case ("mnemonic", c) => c.as[ValueForMnemonic]
      case ("name"    , c) => c.as[ValueForName]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForImp      => Json.obj("imp"      -> a.asJson)
      case a: ValueForMnemonic => Json.obj("mnemonic" -> a.asJson)
      case a: ValueForName     => Json.obj("name"     -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private implicit val codecCustomTagFieldGD: JsonCodec[CustomTagFieldGD.NonEmptyValues] = {
    import CustomTagFieldGD._

    implicit val codecValueForMandatory = JsonCodec.xmap(ValueForMandatory.apply)(_.value)
    implicit val codecValueForReqTypes  = JsonCodec.xmap(ValueForReqTypes .apply)(_.value)
    implicit val codecValueForTagId     = JsonCodec.xmap(ValueForTagId    .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("mandatory", c) => c.as[ValueForMandatory]
      case ("reqTypes" , c) => c.as[ValueForReqTypes]
      case ("tagId"    , c) => c.as[ValueForTagId]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForMandatory => Json.obj("mandatory" -> a.asJson)
      case a: ValueForReqTypes  => Json.obj("reqTypes"  -> a.asJson)
      case a: ValueForTagId     => Json.obj("tagId"     -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private implicit val codecCustomTextFieldGD: JsonCodec[CustomTextFieldGD.NonEmptyValues] = {
    import CustomTextFieldGD._

    implicit val codecValueForKey       = JsonCodec.xmap(ValueForKey      .apply)(_.value)
    implicit val codecValueForMandatory = JsonCodec.xmap(ValueForMandatory.apply)(_.value)
    implicit val codecValueForName      = JsonCodec.xmap(ValueForName     .apply)(_.value)
    implicit val codecValueForReqTypes  = JsonCodec.xmap(ValueForReqTypes .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("key"      , c) => c.as[ValueForKey]
      case ("mandatory", c) => c.as[ValueForMandatory]
      case ("name"     , c) => c.as[ValueForName]
      case ("reqTypes" , c) => c.as[ValueForReqTypes]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForKey       => Json.obj("key"       -> a.asJson)
      case a: ValueForMandatory => Json.obj("mandatory" -> a.asJson)
      case a: ValueForName      => Json.obj("name"      -> a.asJson)
      case a: ValueForReqTypes  => Json.obj("reqTypes"  -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private implicit val codecGenericReqGD: JsonCodec[GenericReqGD.Values] = {
    import GenericReqGD._

    implicit val codecValueForCodes      = JsonCodec.xmap(ValueForCodes     .apply)(_.value)
    implicit val codecValueForCustomText = JsonCodec.xmap(ValueForCustomText.apply)(_.value)
    implicit val codecValueForImpSrcs    = JsonCodec.xmap(ValueForImpSrcs   .apply)(_.value)
    implicit val codecValueForImpTgts    = JsonCodec.xmap(ValueForImpTgts   .apply)(_.value)
    implicit val codecValueForTags       = JsonCodec.xmap(ValueForTags      .apply)(_.value)
    implicit val codecValueForTitle      = JsonCodec.xmap(ValueForTitle     .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("codes"     , c) => c.as[ValueForCodes]
      case ("customText", c) => c.as[ValueForCustomText]
      case ("impSrcs"   , c) => c.as[ValueForImpSrcs]
      case ("impTgts"   , c) => c.as[ValueForImpTgts]
      case ("tags"      , c) => c.as[ValueForTags]
      case ("title"     , c) => c.as[ValueForTitle]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForCodes      => Json.obj("codes"      -> a.asJson)
      case a: ValueForCustomText => Json.obj("customText" -> a.asJson)
      case a: ValueForImpSrcs    => Json.obj("impSrcs"    -> a.asJson)
      case a: ValueForImpTgts    => Json.obj("impTgts"    -> a.asJson)
      case a: ValueForTags       => Json.obj("tags"       -> a.asJson)
      case a: ValueForTitle      => Json.obj("title"      -> a.asJson)
    }

    codecIMap(emptyValues)
  }

  private implicit val codecSavedViewGD: JsonCodec[SavedViewGD.NonEmptyValues] = {
    import SavedViewGD._

    implicit val codecValueForColumns    = JsonCodec.xmap(ValueForColumns   .apply)(_.value)
    implicit val codecValueForFilter     = JsonCodec.xmap(ValueForFilter    .apply)(_.value)
    implicit val codecValueForFilterDead = JsonCodec.xmap(ValueForFilterDead.apply)(_.value)
    implicit val codecValueForName       = JsonCodec.xmap(ValueForName      .apply)(_.value)
    implicit val codecValueForOrder      = JsonCodec.xmap(ValueForOrder     .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("columns"   , c) => c.as[ValueForColumns]
      case ("filter"    , c) => c.as[ValueForFilter]
      case ("filterDead", c) => c.as[ValueForFilterDead]
      case ("name"      , c) => c.as[ValueForName]
      case ("order"     , c) => c.as[ValueForOrder]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForColumns    => Json.obj("columns"    -> a.asJson)
      case a: ValueForFilter     => Json.obj("filter"     -> a.asJson)
      case a: ValueForFilterDead => Json.obj("filterDead" -> a.asJson)
      case a: ValueForName       => Json.obj("name"       -> a.asJson)
      case a: ValueForOrder      => Json.obj("order"      -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private implicit val codecTagGroupGD: JsonCodec[TagGroupGD.NonEmptyValues] = {
    import TagGroupGD._

    implicit val codecValueForChildren      = JsonCodec.xmap(ValueForChildren     .apply)(_.value)
    implicit val codecValueForDesc          = JsonCodec.xmap(ValueForDesc         .apply)(_.value)
    implicit val codecValueForMutexChildren = JsonCodec.xmap(ValueForMutexChildren.apply)(_.value)
    implicit val codecValueForName          = JsonCodec.xmap(ValueForName         .apply)(_.value)
    implicit val codecValueForParents       = JsonCodec.xmap(ValueForParents      .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("children"     , c) => c.as[ValueForChildren]
      case ("desc"         , c) => c.as[ValueForDesc]
      case ("mutexChildren", c) => c.as[ValueForMutexChildren]
      case ("name"         , c) => c.as[ValueForName]
      case ("parents"      , c) => c.as[ValueForParents]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForChildren      => Json.obj("children"      -> a.asJson)
      case a: ValueForDesc          => Json.obj("desc"          -> a.asJson)
      case a: ValueForMutexChildren => Json.obj("mutexChildren" -> a.asJson)
      case a: ValueForName          => Json.obj("name"          -> a.asJson)
      case a: ValueForParents       => Json.obj("parents"       -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private implicit val codecUseCaseGD: JsonCodec[UseCaseGD.Values] = {
    import UseCaseGD._

    implicit val codecValueForCodes      = JsonCodec.xmap(ValueForCodes     .apply)(_.value)
    implicit val codecValueForCustomText = JsonCodec.xmap(ValueForCustomText.apply)(_.value)
    implicit val codecValueForImpSrcs    = JsonCodec.xmap(ValueForImpSrcs   .apply)(_.value)
    implicit val codecValueForImpTgts    = JsonCodec.xmap(ValueForImpTgts   .apply)(_.value)
    implicit val codecValueForTags       = JsonCodec.xmap(ValueForTags      .apply)(_.value)
    implicit val codecValueForTitle      = JsonCodec.xmap(ValueForTitle     .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("codes"     , c) => c.as[ValueForCodes]
      case ("customText", c) => c.as[ValueForCustomText]
      case ("impSrcs"   , c) => c.as[ValueForImpSrcs]
      case ("impTgts"   , c) => c.as[ValueForImpTgts]
      case ("tags"      , c) => c.as[ValueForTags]
      case ("title"     , c) => c.as[ValueForTitle]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForCodes      => Json.obj("codes"      -> a.asJson)
      case a: ValueForCustomText => Json.obj("customText" -> a.asJson)
      case a: ValueForImpSrcs    => Json.obj("impSrcs"    -> a.asJson)
      case a: ValueForImpTgts    => Json.obj("impTgts"    -> a.asJson)
      case a: ValueForTags       => Json.obj("tags"       -> a.asJson)
      case a: ValueForTitle      => Json.obj("title"      -> a.asJson)
    }

    codecIMap(emptyValues)
  }

  private implicit val codecUseCaseStepGD: JsonCodec[UseCaseStepGD.NonEmptyValues] = {
    import UseCaseStepGD._

    implicit val codecValueForFlowIn  = JsonCodec.xmap(ValueForFlowIn .apply)(_.value)
    implicit val codecValueForFlowOut = JsonCodec.xmap(ValueForFlowOut.apply)(_.value)
    implicit val codecValueForTitle   = JsonCodec.xmap(ValueForTitle  .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("flowIn" , c) => c.as[ValueForFlowIn]
      case ("flowOut", c) => c.as[ValueForFlowOut]
      case ("title"  , c) => c.as[ValueForTitle]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForFlowIn  => Json.obj("flowIn"  -> a.asJson)
      case a: ValueForFlowOut => Json.obj("flowOut" -> a.asJson)
      case a: ValueForTitle   => Json.obj("title"   -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  // ===================================================================================================================
  // Events

  private implicit val decoderEventProjectNameSet: Decoder[Event.ProjectNameSet] =
    Decoder[String].map(Event.ProjectNameSet.apply)

  private implicit val encoderEventProjectNameSet: Encoder[Event.ProjectNameSet] =
    Encoder[String].contramap(_.name)

  private implicit val decoderEventProjectTemplateApply: Decoder[Event.ProjectTemplateApply] =
    Decoder[ProjectTemplate].map(Event.ProjectTemplateApply.apply)

  private implicit val encoderEventProjectTemplateApply: Encoder[Event.ProjectTemplateApply] =
    Encoder[ProjectTemplate].contramap(_.template)

  private implicit val decoderEventCustomIssueTypeCreate: Decoder[Event.CustomIssueTypeCreate] =
    Decoder.forProduct2("id", "values")(Event.CustomIssueTypeCreate.apply)

  private implicit val encoderEventCustomIssueTypeCreate: Encoder[Event.CustomIssueTypeCreate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventCustomIssueTypeUpdate: Decoder[Event.CustomIssueTypeUpdate] =
    Decoder.forProduct2("id", "values")(Event.CustomIssueTypeUpdate.apply)

  private implicit val encoderEventCustomIssueTypeUpdate: Encoder[Event.CustomIssueTypeUpdate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventCustomIssueTypeDelete: Decoder[Event.CustomIssueTypeDelete] =
    Decoder[CustomIssueTypeId].map(Event.CustomIssueTypeDelete.apply)

  private implicit val encoderEventCustomIssueTypeDelete: Encoder[Event.CustomIssueTypeDelete] =
    Encoder[CustomIssueTypeId].contramap(_.id)

  private implicit val decoderEventCustomIssueTypeRestore: Decoder[Event.CustomIssueTypeRestore] =
    Decoder[CustomIssueTypeId].map(Event.CustomIssueTypeRestore.apply)

  private implicit val encoderEventCustomIssueTypeRestore: Encoder[Event.CustomIssueTypeRestore] =
    Encoder[CustomIssueTypeId].contramap(_.id)

  private implicit val decoderEventCustomReqTypeCreate: Decoder[Event.CustomReqTypeCreate] =
    Decoder.forProduct2("id", "values")(Event.CustomReqTypeCreate.apply)

  private implicit val encoderEventCustomReqTypeCreate: Encoder[Event.CustomReqTypeCreate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventCustomReqTypeUpdate: Decoder[Event.CustomReqTypeUpdate] =
    Decoder.forProduct2("id", "values")(Event.CustomReqTypeUpdate.apply)

  private implicit val encoderEventCustomReqTypeUpdate: Encoder[Event.CustomReqTypeUpdate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventCustomReqTypeDelete: Decoder[Event.CustomReqTypeDelete] =
    Decoder[CustomReqTypeId].map(Event.CustomReqTypeDelete.apply)

  private implicit val encoderEventCustomReqTypeDelete: Encoder[Event.CustomReqTypeDelete] =
    Encoder[CustomReqTypeId].contramap(_.id)

  private implicit val decoderEventCustomReqTypeRestore: Decoder[Event.CustomReqTypeRestore] =
    Decoder[CustomReqTypeId].map(Event.CustomReqTypeRestore.apply)

  private implicit val encoderEventCustomReqTypeRestore: Encoder[Event.CustomReqTypeRestore] =
    Encoder[CustomReqTypeId].contramap(_.id)

  private implicit val decoderEventTagDelete: Decoder[Event.TagDelete] =
    Decoder[TagId].map(Event.TagDelete.apply)

  private implicit val encoderEventTagDelete: Encoder[Event.TagDelete] =
    Encoder[TagId].contramap(_.id)

  private implicit val decoderEventTagRestore: Decoder[Event.TagRestore] =
    Decoder[TagId].map(Event.TagRestore.apply)

  private implicit val encoderEventTagRestore: Encoder[Event.TagRestore] =
    Encoder[TagId].contramap(_.id)

  private implicit val decoderEventTagGroupCreate: Decoder[Event.TagGroupCreate] =
    Decoder.forProduct2("id", "values")(Event.TagGroupCreate.apply)

  private implicit val encoderEventTagGroupCreate: Encoder[Event.TagGroupCreate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventTagGroupUpdate: Decoder[Event.TagGroupUpdate] =
    Decoder.forProduct2("id", "values")(Event.TagGroupUpdate.apply)

  private implicit val encoderEventTagGroupUpdate: Encoder[Event.TagGroupUpdate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventApplicableTagCreate: Decoder[Event.ApplicableTagCreate] =
    Decoder.forProduct2("id", "values")(Event.ApplicableTagCreate.apply)

  private implicit val encoderEventApplicableTagCreate: Encoder[Event.ApplicableTagCreate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventApplicableTagUpdate: Decoder[Event.ApplicableTagUpdate] =
    Decoder.forProduct2("id", "values")(Event.ApplicableTagUpdate.apply)

  private implicit val encoderEventApplicableTagUpdate: Encoder[Event.ApplicableTagUpdate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventFieldReposition: Decoder[Event.FieldReposition] =
    Decoder.forProduct2("id", "newPos")(Event.FieldReposition.apply)

  private implicit val encoderEventFieldReposition: Encoder[Event.FieldReposition] =
    Encoder.forProduct2("id", "newPos")(a => (a.id, a.newPos))

  private implicit val decoderEventFieldStaticAdd: Decoder[Event.FieldStaticAdd] =
    Decoder[StaticField].map(Event.FieldStaticAdd.apply)

  private implicit val encoderEventFieldStaticAdd: Encoder[Event.FieldStaticAdd] =
    Encoder[StaticField].contramap(_.f)

  private implicit val decoderEventFieldStaticRemove: Decoder[Event.FieldStaticRemove] =
    Decoder[StaticField].map(Event.FieldStaticRemove.apply)

  private implicit val encoderEventFieldStaticRemove: Encoder[Event.FieldStaticRemove] =
    Encoder[StaticField].contramap(_.f)

  private implicit val decoderEventFieldCustomDelete: Decoder[Event.FieldCustomDelete] =
    Decoder[CustomFieldId].map(Event.FieldCustomDelete.apply)

  private implicit val encoderEventFieldCustomDelete: Encoder[Event.FieldCustomDelete] =
    Encoder[CustomFieldId].contramap(_.id)

  private implicit val decoderEventFieldCustomRestore: Decoder[Event.FieldCustomRestore] =
    Decoder[CustomFieldId].map(Event.FieldCustomRestore.apply)

  private implicit val encoderEventFieldCustomRestore: Encoder[Event.FieldCustomRestore] =
    Encoder[CustomFieldId].contramap(_.id)

  private implicit val decoderEventFieldCustomTextCreate: Decoder[Event.FieldCustomTextCreate] =
    Decoder.forProduct2("id", "values")(Event.FieldCustomTextCreate.apply)

  private implicit val encoderEventFieldCustomTextCreate: Encoder[Event.FieldCustomTextCreate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventFieldCustomTextUpdate: Decoder[Event.FieldCustomTextUpdate] =
    Decoder.forProduct2("id", "values")(Event.FieldCustomTextUpdate.apply)

  private implicit val encoderEventFieldCustomTextUpdate: Encoder[Event.FieldCustomTextUpdate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventFieldCustomTagCreate: Decoder[Event.FieldCustomTagCreate] =
    Decoder.forProduct2("id", "values")(Event.FieldCustomTagCreate.apply)

  private implicit val encoderEventFieldCustomTagCreate: Encoder[Event.FieldCustomTagCreate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventFieldCustomTagUpdate: Decoder[Event.FieldCustomTagUpdate] =
    Decoder.forProduct2("id", "values")(Event.FieldCustomTagUpdate.apply)

  private implicit val encoderEventFieldCustomTagUpdate: Encoder[Event.FieldCustomTagUpdate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventFieldCustomImpCreate: Decoder[Event.FieldCustomImpCreate] =
    Decoder.forProduct2("id", "values")(Event.FieldCustomImpCreate.apply)

  private implicit val encoderEventFieldCustomImpCreate: Encoder[Event.FieldCustomImpCreate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventFieldCustomImpUpdate: Decoder[Event.FieldCustomImpUpdate] =
    Decoder.forProduct2("id", "values")(Event.FieldCustomImpUpdate.apply)

  private implicit val encoderEventFieldCustomImpUpdate: Encoder[Event.FieldCustomImpUpdate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventGenericReqCreate: Decoder[Event.GenericReqCreate] =
    Decoder.forProduct3("reqId", "reqTypeId", "values")(Event.GenericReqCreate.apply)

  private implicit val encoderEventGenericReqCreate: Encoder[Event.GenericReqCreate] =
    Encoder.forProduct3("reqId", "reqTypeId", "values")(a => (a.id, a.rt, a.vs))

  private implicit val decoderEventGenericReqTypeSet: Decoder[Event.GenericReqTypeSet] =
    Decoder.forProduct2("id", "value")(Event.GenericReqTypeSet.apply)

  private implicit val encoderEventGenericReqTypeSet: Encoder[Event.GenericReqTypeSet] =
    Encoder.forProduct2("id", "value")(a => (a.id, a.value))

  private implicit val decoderEventGenericReqTitleSet: Decoder[Event.GenericReqTitleSet] =
    Decoder.forProduct2("id", "value")(Event.GenericReqTitleSet.apply)

  private implicit val encoderEventGenericReqTitleSet: Encoder[Event.GenericReqTitleSet] =
    Encoder.forProduct2("id", "value")(a => (a.id, a.value))

  private implicit val decoderEventUseCaseCreate: Decoder[Event.UseCaseCreate] =
    Decoder.forProduct3("id", "stepId", "values")(Event.UseCaseCreate.apply)

  private implicit val encoderEventUseCaseCreate: Encoder[Event.UseCaseCreate] =
    Encoder.forProduct3("id", "stepId", "values")(a => (a.id, a.stepId, a.vs))

  private implicit val decoderEventUseCaseTitleSet: Decoder[Event.UseCaseTitleSet] =
    Decoder.forProduct2("id", "value")(Event.UseCaseTitleSet.apply)

  private implicit val encoderEventUseCaseTitleSet: Encoder[Event.UseCaseTitleSet] =
    Encoder.forProduct2("id", "value")(a => (a.id, a.value))

  private implicit val decoderEventUseCaseStepCreate: Decoder[Event.UseCaseStepCreate] =
    Decoder.forProduct4("id", "ucId", "field", "at")(Event.UseCaseStepCreate.apply)

  private implicit val encoderEventUseCaseStepCreate: Encoder[Event.UseCaseStepCreate] =
    Encoder.forProduct4("id", "ucId", "field", "at")(a => (a.id, a.ucId, a.field, a.at))

  private implicit val decoderEventUseCaseStepUpdate: Decoder[Event.UseCaseStepUpdate] =
    Decoder.forProduct2("id", "values")(Event.UseCaseStepUpdate.apply)

  private implicit val encoderEventUseCaseStepUpdate: Encoder[Event.UseCaseStepUpdate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventUseCaseStepShiftLeft: Decoder[Event.UseCaseStepShiftLeft] =
    Decoder[UseCaseStepId].map(Event.UseCaseStepShiftLeft.apply)

  private implicit val encoderEventUseCaseStepShiftLeft: Encoder[Event.UseCaseStepShiftLeft] =
    Encoder[UseCaseStepId].contramap(_.id)

  private implicit val decoderEventUseCaseStepShiftRight: Decoder[Event.UseCaseStepShiftRight] =
    Decoder[UseCaseStepId].map(Event.UseCaseStepShiftRight.apply)

  private implicit val encoderEventUseCaseStepShiftRight: Encoder[Event.UseCaseStepShiftRight] =
    Encoder[UseCaseStepId].contramap(_.id)

  private implicit val decoderEventUseCaseStepDelete: Decoder[Event.UseCaseStepDelete] =
    Decoder[UseCaseStepId].map(Event.UseCaseStepDelete.apply)

  private implicit val encoderEventUseCaseStepDelete: Encoder[Event.UseCaseStepDelete] =
    Encoder[UseCaseStepId].contramap(_.id)

  private implicit val decoderEventUseCaseStepRestore: Decoder[Event.UseCaseStepRestore] =
    Decoder[UseCaseStepId].map(Event.UseCaseStepRestore.apply)

  private implicit val encoderEventUseCaseStepRestore: Encoder[Event.UseCaseStepRestore] =
    Encoder[UseCaseStepId].contramap(_.id)

  private implicit val decoderEventCodeGroupCreate: Decoder[Event.CodeGroupCreate] =
    Decoder.forProduct2("id", "values")(Event.CodeGroupCreate.apply)

  private implicit val encoderEventCodeGroupCreate: Encoder[Event.CodeGroupCreate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventCodeGroupUpdate: Decoder[Event.CodeGroupUpdate] =
    Decoder.forProduct2("id", "values")(Event.CodeGroupUpdate.apply)

  private implicit val encoderEventCodeGroupUpdate: Encoder[Event.CodeGroupUpdate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventCodeGroupsDelete: Decoder[Event.CodeGroupsDelete] =
    Decoder[NonEmptySet[ReqCodeGroupId]].map(Event.CodeGroupsDelete.apply)

  private implicit val encoderEventCodeGroupsDelete: Encoder[Event.CodeGroupsDelete] =
    Encoder[NonEmptySet[ReqCodeGroupId]].contramap(_.ids)

  private implicit val decoderEventReqCodesPatch: Decoder[Event.ReqCodesPatch] =
    Decoder.forProduct4("id", "remove", "restore", "add")(Event.ReqCodesPatch.apply)

  private implicit val encoderEventReqCodesPatch: Encoder[Event.ReqCodesPatch] =
    Encoder.forProduct4("id", "remove", "restore", "add")(a => (a.id, a.remove, a.restore, a.add))

  private implicit val decoderEventReqTagsPatch: Decoder[Event.ReqTagsPatch] =
    Decoder.forProduct2("id", "patch")(Event.ReqTagsPatch.apply)

  private implicit val encoderEventReqTagsPatch: Encoder[Event.ReqTagsPatch] =
    Encoder.forProduct2("id", "patch")(a => (a.id, a.patch))

  private implicit val decoderEventReqImplicationsPatch: Decoder[Event.ReqImplicationsPatch] =
    Decoder.forProduct3("id", "dir", "patch")(Event.ReqImplicationsPatch.apply)

  private implicit val encoderEventReqImplicationsPatch: Encoder[Event.ReqImplicationsPatch] =
    Encoder.forProduct3("id", "dir", "patch")(a => (a.id, a.dir, a.patch))

  private implicit val decoderEventReqFieldCustomTextSet: Decoder[Event.ReqFieldCustomTextSet] =
    Decoder.forProduct3("id", "fid", "value")(Event.ReqFieldCustomTextSet.apply)

  private implicit val encoderEventReqFieldCustomTextSet: Encoder[Event.ReqFieldCustomTextSet] =
    Encoder.forProduct3("id", "fid", "value")(a => (a.id, a.fid, a.value))

  private implicit val decoderEventReqsDelete: Decoder[Event.ReqsDelete] =
    Decoder.forProduct3("reqs", "codeGroups", "reason")(Event.ReqsDelete.apply)

  private implicit val encoderEventReqsDelete: Encoder[Event.ReqsDelete] =
    Encoder.forProduct3("reqs", "codeGroups", "reason")(a => (a.reqs, a.codeGroups, a.reason))

  private implicit val decoderEventContentRestore: Decoder[Event.ContentRestore] =
    Decoder.forProduct2("reqs", "codeGroups")(Event.ContentRestore.apply)

  private implicit val encoderEventContentRestore: Encoder[Event.ContentRestore] =
    Encoder.forProduct2("reqs", "codeGroups")(a => (a.reqs, a.codeGroups))

  private implicit val decoderEventManualIssueCreate: Decoder[Event.ManualIssueCreate] =
    Decoder.forProduct2("id", "text")(Event.ManualIssueCreate.apply)

  private implicit val encoderEventManualIssueCreate: Encoder[Event.ManualIssueCreate] =
    Encoder.forProduct2("id", "text")(a => (a.id, a.text))

  private implicit val decoderEventManualIssueUpdate: Decoder[Event.ManualIssueUpdate] =
    Decoder.forProduct2("id", "text")(Event.ManualIssueUpdate.apply)

  private implicit val encoderEventManualIssueUpdate: Encoder[Event.ManualIssueUpdate] =
    Encoder.forProduct2("id", "text")(a => (a.id, a.text))

  private implicit val decoderEventManualIssueDelete: Decoder[Event.ManualIssueDelete] =
    Decoder[ManualIssueId].map(Event.ManualIssueDelete.apply)

  private implicit val encoderEventManualIssueDelete: Encoder[Event.ManualIssueDelete] =
    Encoder[ManualIssueId].contramap(_.id)

  private implicit val decoderEventSavedViewCreate: Decoder[Event.SavedViewCreate] =
    Decoder.forProduct6("id", "name", "columns", "order", "filterDead", "filter")(Event.SavedViewCreate.apply)

  private implicit val encoderEventSavedViewCreate: Encoder[Event.SavedViewCreate] =
    Encoder.forProduct6("id", "name", "columns", "order", "filterDead", "filter")(a => (a.id, a.name, a.columns, a.order, a.filterDead, a.filter))

  private implicit val decoderEventSavedViewUpdate: Decoder[Event.SavedViewUpdate] =
    Decoder.forProduct2("id", "values")(Event.SavedViewUpdate.apply)

  private implicit val encoderEventSavedViewUpdate: Encoder[Event.SavedViewUpdate] =
    Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

  private implicit val decoderEventSavedViewDelete: Decoder[Event.SavedViewDelete] =
    Decoder[SavedView.Id].map(Event.SavedViewDelete.apply)

  private implicit val encoderEventSavedViewDelete: Encoder[Event.SavedViewDelete] =
    Encoder[SavedView.Id].contramap(_.id)

  private implicit val decoderEventSavedViewDefaultSet: Decoder[Event.SavedViewDefaultSet] =
    Decoder[SavedView.Id].map(Event.SavedViewDefaultSet.apply)

  private implicit val encoderEventSavedViewDefaultSet: Encoder[Event.SavedViewDefaultSet] =
    Encoder[SavedView.Id].contramap(_.id)

  // ===================================================================================================================

  implicit val decoderEvent: Decoder[Event] = decodeSumBySoleKey {
    case ("ApplicableTagCreate"   , c) => c.as[Event.ApplicableTagCreate]
    case ("ApplicableTagUpdate"   , c) => c.as[Event.ApplicableTagUpdate]
    case ("CodeGroupCreate"       , c) => c.as[Event.CodeGroupCreate]
    case ("CodeGroupUpdate"       , c) => c.as[Event.CodeGroupUpdate]
    case ("CodeGroupsDelete"      , c) => c.as[Event.CodeGroupsDelete]
    case ("ContentRestore"        , c) => c.as[Event.ContentRestore]
    case ("CustomIssueTypeCreate" , c) => c.as[Event.CustomIssueTypeCreate]
    case ("CustomIssueTypeDelete" , c) => c.as[Event.CustomIssueTypeDelete]
    case ("CustomIssueTypeRestore", c) => c.as[Event.CustomIssueTypeRestore]
    case ("CustomIssueTypeUpdate" , c) => c.as[Event.CustomIssueTypeUpdate]
    case ("CustomReqTypeCreate"   , c) => c.as[Event.CustomReqTypeCreate]
    case ("CustomReqTypeDelete"   , c) => c.as[Event.CustomReqTypeDelete]
    case ("CustomReqTypeRestore"  , c) => c.as[Event.CustomReqTypeRestore]
    case ("CustomReqTypeUpdate"   , c) => c.as[Event.CustomReqTypeUpdate]
    case ("FieldCustomDelete"     , c) => c.as[Event.FieldCustomDelete]
    case ("FieldCustomImpCreate"  , c) => c.as[Event.FieldCustomImpCreate]
    case ("FieldCustomImpUpdate"  , c) => c.as[Event.FieldCustomImpUpdate]
    case ("FieldCustomRestore"    , c) => c.as[Event.FieldCustomRestore]
    case ("FieldCustomTagCreate"  , c) => c.as[Event.FieldCustomTagCreate]
    case ("FieldCustomTagUpdate"  , c) => c.as[Event.FieldCustomTagUpdate]
    case ("FieldCustomTextCreate" , c) => c.as[Event.FieldCustomTextCreate]
    case ("FieldCustomTextUpdate" , c) => c.as[Event.FieldCustomTextUpdate]
    case ("FieldReposition"       , c) => c.as[Event.FieldReposition]
    case ("FieldStaticAdd"        , c) => c.as[Event.FieldStaticAdd]
    case ("FieldStaticRemove"     , c) => c.as[Event.FieldStaticRemove]
    case ("GenericReqCreate"      , c) => c.as[Event.GenericReqCreate]
    case ("GenericReqTitleSet"    , c) => c.as[Event.GenericReqTitleSet]
    case ("GenericReqTypeSet"     , c) => c.as[Event.GenericReqTypeSet]
    case ("ManualIssueCreate"     , c) => c.as[Event.ManualIssueCreate]
    case ("ManualIssueDelete"     , c) => c.as[Event.ManualIssueDelete]
    case ("ManualIssueUpdate"     , c) => c.as[Event.ManualIssueUpdate]
    case ("ProjectNameSet"        , c) => c.as[Event.ProjectNameSet]
    case ("ProjectTemplateApply"  , c) => c.as[Event.ProjectTemplateApply]
    case ("ReqCodesPatch"         , c) => c.as[Event.ReqCodesPatch]
    case ("ReqFieldCustomTextSet" , c) => c.as[Event.ReqFieldCustomTextSet]
    case ("ReqImplicationsPatch"  , c) => c.as[Event.ReqImplicationsPatch]
    case ("ReqTagsPatch"          , c) => c.as[Event.ReqTagsPatch]
    case ("ReqsDelete"            , c) => c.as[Event.ReqsDelete]
    case ("SavedViewCreate"       , c) => c.as[Event.SavedViewCreate]
    case ("SavedViewDefaultSet"   , c) => c.as[Event.SavedViewDefaultSet]
    case ("SavedViewDelete"       , c) => c.as[Event.SavedViewDelete]
    case ("SavedViewUpdate"       , c) => c.as[Event.SavedViewUpdate]
    case ("TagDelete"             , c) => c.as[Event.TagDelete]
    case ("TagGroupCreate"        , c) => c.as[Event.TagGroupCreate]
    case ("TagGroupUpdate"        , c) => c.as[Event.TagGroupUpdate]
    case ("TagRestore"            , c) => c.as[Event.TagRestore]
    case ("UseCaseCreate"         , c) => c.as[Event.UseCaseCreate]
    case ("UseCaseStepCreate"     , c) => c.as[Event.UseCaseStepCreate]
    case ("UseCaseStepDelete"     , c) => c.as[Event.UseCaseStepDelete]
    case ("UseCaseStepRestore"    , c) => c.as[Event.UseCaseStepRestore]
    case ("UseCaseStepShiftLeft"  , c) => c.as[Event.UseCaseStepShiftLeft]
    case ("UseCaseStepShiftRight" , c) => c.as[Event.UseCaseStepShiftRight]
    case ("UseCaseStepUpdate"     , c) => c.as[Event.UseCaseStepUpdate]
    case ("UseCaseTitleSet"       , c) => c.as[Event.UseCaseTitleSet]
  }

  implicit val encoderEvent: Encoder[Event] = Encoder.instance {
    case a: Event.ApplicableTagCreate    => Json.obj("ApplicableTagCreate"    -> a.asJson)
    case a: Event.ApplicableTagUpdate    => Json.obj("ApplicableTagUpdate"    -> a.asJson)
    case a: Event.CodeGroupCreate        => Json.obj("CodeGroupCreate"        -> a.asJson)
    case a: Event.CodeGroupUpdate        => Json.obj("CodeGroupUpdate"        -> a.asJson)
    case a: Event.CodeGroupsDelete       => Json.obj("CodeGroupsDelete"       -> a.asJson)
    case a: Event.ContentRestore         => Json.obj("ContentRestore"         -> a.asJson)
    case a: Event.CustomIssueTypeCreate  => Json.obj("CustomIssueTypeCreate"  -> a.asJson)
    case a: Event.CustomIssueTypeDelete  => Json.obj("CustomIssueTypeDelete"  -> a.asJson)
    case a: Event.CustomIssueTypeRestore => Json.obj("CustomIssueTypeRestore" -> a.asJson)
    case a: Event.CustomIssueTypeUpdate  => Json.obj("CustomIssueTypeUpdate"  -> a.asJson)
    case a: Event.CustomReqTypeCreate    => Json.obj("CustomReqTypeCreate"    -> a.asJson)
    case a: Event.CustomReqTypeDelete    => Json.obj("CustomReqTypeDelete"    -> a.asJson)
    case a: Event.CustomReqTypeRestore   => Json.obj("CustomReqTypeRestore"   -> a.asJson)
    case a: Event.CustomReqTypeUpdate    => Json.obj("CustomReqTypeUpdate"    -> a.asJson)
    case a: Event.FieldCustomDelete      => Json.obj("FieldCustomDelete"      -> a.asJson)
    case a: Event.FieldCustomImpCreate   => Json.obj("FieldCustomImpCreate"   -> a.asJson)
    case a: Event.FieldCustomImpUpdate   => Json.obj("FieldCustomImpUpdate"   -> a.asJson)
    case a: Event.FieldCustomRestore     => Json.obj("FieldCustomRestore"     -> a.asJson)
    case a: Event.FieldCustomTagCreate   => Json.obj("FieldCustomTagCreate"   -> a.asJson)
    case a: Event.FieldCustomTagUpdate   => Json.obj("FieldCustomTagUpdate"   -> a.asJson)
    case a: Event.FieldCustomTextCreate  => Json.obj("FieldCustomTextCreate"  -> a.asJson)
    case a: Event.FieldCustomTextUpdate  => Json.obj("FieldCustomTextUpdate"  -> a.asJson)
    case a: Event.FieldReposition        => Json.obj("FieldReposition"        -> a.asJson)
    case a: Event.FieldStaticAdd         => Json.obj("FieldStaticAdd"         -> a.asJson)
    case a: Event.FieldStaticRemove      => Json.obj("FieldStaticRemove"      -> a.asJson)
    case a: Event.GenericReqCreate       => Json.obj("GenericReqCreate"       -> a.asJson)
    case a: Event.GenericReqTitleSet     => Json.obj("GenericReqTitleSet"     -> a.asJson)
    case a: Event.GenericReqTypeSet      => Json.obj("GenericReqTypeSet"      -> a.asJson)
    case a: Event.ManualIssueCreate      => Json.obj("ManualIssueCreate"      -> a.asJson)
    case a: Event.ManualIssueDelete      => Json.obj("ManualIssueDelete"      -> a.asJson)
    case a: Event.ManualIssueUpdate      => Json.obj("ManualIssueUpdate"      -> a.asJson)
    case a: Event.ProjectNameSet         => Json.obj("ProjectNameSet"         -> a.asJson)
    case a: Event.ProjectTemplateApply   => Json.obj("ProjectTemplateApply"   -> a.asJson)
    case a: Event.ReqCodesPatch          => Json.obj("ReqCodesPatch"          -> a.asJson)
    case a: Event.ReqFieldCustomTextSet  => Json.obj("ReqFieldCustomTextSet"  -> a.asJson)
    case a: Event.ReqImplicationsPatch   => Json.obj("ReqImplicationsPatch"   -> a.asJson)
    case a: Event.ReqTagsPatch           => Json.obj("ReqTagsPatch"           -> a.asJson)
    case a: Event.ReqsDelete             => Json.obj("ReqsDelete"             -> a.asJson)
    case a: Event.SavedViewCreate        => Json.obj("SavedViewCreate"        -> a.asJson)
    case a: Event.SavedViewDefaultSet    => Json.obj("SavedViewDefaultSet"    -> a.asJson)
    case a: Event.SavedViewDelete        => Json.obj("SavedViewDelete"        -> a.asJson)
    case a: Event.SavedViewUpdate        => Json.obj("SavedViewUpdate"        -> a.asJson)
    case a: Event.TagDelete              => Json.obj("TagDelete"              -> a.asJson)
    case a: Event.TagGroupCreate         => Json.obj("TagGroupCreate"         -> a.asJson)
    case a: Event.TagGroupUpdate         => Json.obj("TagGroupUpdate"         -> a.asJson)
    case a: Event.TagRestore             => Json.obj("TagRestore"             -> a.asJson)
    case a: Event.UseCaseCreate          => Json.obj("UseCaseCreate"          -> a.asJson)
    case a: Event.UseCaseStepCreate      => Json.obj("UseCaseStepCreate"      -> a.asJson)
    case a: Event.UseCaseStepDelete      => Json.obj("UseCaseStepDelete"      -> a.asJson)
    case a: Event.UseCaseStepRestore     => Json.obj("UseCaseStepRestore"     -> a.asJson)
    case a: Event.UseCaseStepShiftLeft   => Json.obj("UseCaseStepShiftLeft"   -> a.asJson)
    case a: Event.UseCaseStepShiftRight  => Json.obj("UseCaseStepShiftRight"  -> a.asJson)
    case a: Event.UseCaseStepUpdate      => Json.obj("UseCaseStepUpdate"      -> a.asJson)
    case a: Event.UseCaseTitleSet        => Json.obj("UseCaseTitleSet"        -> a.asJson)
  }
}
