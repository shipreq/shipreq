package shipreq.webapp.member.protocol.json.v1

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.adt_macros.AdtMacros
import nyaya.util.Multimap
import shipreq.base.util.JsonUtil._
import shipreq.base.util._
import shipreq.webapp.base.util._
import shipreq.webapp.member.data._
import shipreq.webapp.member.issue.IssueCategory
import shipreq.webapp.member.protocol.json.JsonCodec
import shipreq.webapp.member.sort.SortMethod

/** This is the minimum set of codecs required for event codecs.
  *
  * Events (and their dependencies) are expected to be extremely stable and only change very, very rarely if ever.
  */
private[v1] object BaseMemberData1 {
  import JsonCodec.Implicits._
  import BaseData._

  // ===================================================================================================================
  // Polymorphic definitions
  // (non-implicit, "decode/decode/codec" prefix)

  def codecIMapD[K: UnivEq, V: Decoder : Encoder](implicit d: DataIdAux[V, K]): JsonCodec[IMap[K, V]] =
    codecIMap(d.emptyIMap)

  // ===================================================================================================================
  // Concrete decoders for base data type
  // (implicit lazy vals, "decoder" prefix)

  object SavedViewCodecs {
    import shipreq.webapp.member.data.savedview._

    implicit val codecColumnImplications: JsonCodec[Column.Implications] =
      JsonCodec.xmap(Column.Implications.apply)(_.dir)

    implicit val codecColumnCustomField: JsonCodec[Column.CustomField] =
      JsonCodec.xmap(Column.CustomField.apply)(_.id)

    private[this] final val KeyPubid = "pubid"

    implicit val decoderColumnImplications: Decoder[Column.Implications] =
      Decoder[Direction].map(Column.Implications.apply)

    implicit val encoderColumnImplications: Encoder[Column.Implications] =
      Encoder[Direction].contramap(_.dir)

    implicit val decoderColumnCustomField: Decoder[Column.CustomField] =
      Decoder[CustomFieldId].map(Column.CustomField.apply)

    implicit val encoderColumnCustomField: Encoder[Column.CustomField] =
      Encoder[CustomFieldId].contramap(_.id)

    implicit val codecColumnSortConclusive: JsonCodec[Column.SortConclusive] =
      JsonCodec.enumAdt(AdtMacros.adtIsoSet[Column.SortConclusive, String] {
        case Column.Pubid => KeyPubid
      })

    implicit val codecSortMethod: JsonCodec[SortMethod] =
      JsonCodec.enumAdt(AdtMacros.adtIsoSet[SortMethod, String] {
        case SortMethod.Asc            => "asc"
        case SortMethod.AscThenBlanks  => "asc_"
        case SortMethod.BlanksThenAsc  => "_asc"
        case SortMethod.BlanksThenDesc => "_desc"
        case SortMethod.Desc           => "desc"
        case SortMethod.DescThenBlanks => "desc_"
      })

    implicit val codecSortMethodIgnoreBlanks: JsonCodec[SortMethod.IgnoreBlanks] =
      JsonCodec.enumAdt(AdtMacros.adtIsoSet[SortMethod.IgnoreBlanks, String] {
        case SortMethod.Asc  => "asc"
        case SortMethod.Desc => "desc"
      })

    implicit val codecSortMethodConsiderBlanks: JsonCodec[SortMethod.ConsiderBlanks] =
      JsonCodec.enumAdt(AdtMacros.adtIsoSet[SortMethod.ConsiderBlanks, String] {
        case SortMethod.AscThenBlanks  => "asc_"
        case SortMethod.BlanksThenAsc  => "_asc"
        case SortMethod.BlanksThenDesc => "_desc"
        case SortMethod.DescThenBlanks => "desc_"
      })

    implicit val decoderSortCriterionConclusive: Decoder[SortCriterion.Conclusive] =
      Decoder.forProduct2("column", "method")(SortCriterion.Conclusive.apply)

    implicit val encoderSortCriterionConclusive: Encoder[SortCriterion.Conclusive] =
      Encoder.forProduct2("column", "method")(a => (a.column, a.method))

    implicit val codecSavedViewId: JsonCodec[SavedView.Id] =
      JsonCodec.xmap(SavedView.Id.apply)(_.value)

    implicit val codecSavedViewName: JsonCodec[SavedView.Name] =
      JsonCodec.xmap(SavedView.Name.apply)(_.value)
  }

  implicit lazy val codecApplicableTagId: JsonCodec[ApplicableTagId] =
    codecTaggedI(ApplicableTagId.apply)

  implicit lazy val codecApReqCodeId: JsonCodec[ApReqCodeId] =
    codecTaggedI(ApReqCodeId.apply)

  implicit lazy val decoderApReqCodeIdAndValue: Decoder[ApReqCodeId.AndValue] =
    Decoder.forProduct2("id", "value")(ApReqCodeId.AndValue.apply)

  implicit lazy val encoderApReqCodeIdAndValue: Encoder[ApReqCodeId.AndValue] =
    Encoder.forProduct2("id", "value")(a => (a.id, a.value))

  implicit lazy val decoderCustomFieldId: Decoder[CustomFieldId] = decodeSumBySoleKey {
    case ("imp" , c) => c.as[CustomField.Implication.Id]
    case ("tag" , c) => c.as[CustomField.Tag.Id]
    case ("text", c) => c.as[CustomField.Text.Id]
  }

  implicit lazy val encoderCustomFieldId: Encoder[CustomFieldId] = Encoder.instance {
    case a: CustomField.Implication.Id => Json.obj("imp"  -> a.asJson)
    case a: CustomField.Tag.Id         => Json.obj("tag"  -> a.asJson)
    case a: CustomField.Text.Id        => Json.obj("text" -> a.asJson)
  }

  implicit lazy val codecCustomFieldImplicationId: JsonCodec[CustomField.Implication.Id] =
    codecTaggedI(CustomField.Implication.Id)

  implicit lazy val codecCustomFieldTagId: JsonCodec[CustomField.Tag.Id] =
    codecTaggedI(CustomField.Tag.Id)

  implicit lazy val keyDecoderCustomFieldTextId: KeyDecoder[CustomField.Text.Id] =
    KeyDecoder.decodeKeyInt.map(CustomField.Text.Id.apply)

  implicit lazy val keyEncoderCustomFieldTextId: KeyEncoder[CustomField.Text.Id] =
    KeyEncoder.encodeKeyInt.contramap(_.value)

  implicit lazy val codecCustomFieldTextId: JsonCodec[CustomField.Text.Id] =
    codecTaggedI(CustomField.Text.Id)

  implicit lazy val codecCustomIssueTypeId: JsonCodec[CustomIssueTypeId] =
    codecTaggedI(CustomIssueTypeId)

  implicit lazy val codecCustomReqTypeId: JsonCodec[CustomReqTypeId] =
    codecTaggedI(CustomReqTypeId)

  implicit lazy val codecFilterDead: JsonCodec[FilterDead] =
    codecBoolVia(FilterDead) {
      case ShowDead => "show"
      case HideDead => "hide"
    }

  implicit lazy val codecGenericReqId: JsonCodec[GenericReqId] =
    codecTaggedI(GenericReqId)

  implicit lazy val codecHashRefKey: JsonCodec[HashRefKey] =
    codecTaggedS(HashRefKey)

  implicit lazy val codecIssueCategory: JsonCodec[IssueCategory] =
    JsonCodec.enumAdt(AdtMacros.adtIsoSet[IssueCategory, String] {
      case IssueCategory.BadData     => "badData"
      case IssueCategory.Futility    => "futility"
      case IssueCategory.MissingData => "missingData"
      case IssueCategory.UserDefined => "userDefined"
    })

  implicit lazy val codecLive: JsonCodec[Live] =
    codecBoolVia(Live) {
      case Live => "live"
      case Dead => "dead"
    }

  implicit lazy val codecMandatory: JsonCodec[Mandatory] =
    codecBool(Mandatory)

  implicit lazy val codecManualIssueId: JsonCodec[ManualIssueId] =
    codecTaggedI(ManualIssueId)

  implicit lazy val keyEncoderReqCodeValue: KeyEncoder[ReqCode.Value] =
    KeyEncoder.encodeKeyString.contramap(ReqCode.Value.toStr(_, '.'))

  implicit lazy val keyDecoderReqCodeValue: KeyDecoder[ReqCode.Value] =
    KeyDecoder.decodeKeyString.map(ReqCode.Value.unsafeFromStr(_, '.'))

  implicit lazy val codecMultimapReqCodeValueSetApReqCodeId: JsonCodec[Multimap[ReqCode.Value, Set, ApReqCodeId]] =
    codecMultimap[ReqCode.Value, Set, ApReqCodeId]

  implicit lazy val codecExclusivity: JsonCodec[Exclusivity] =
    codecBool(Exclusivity)

  implicit lazy val codecOn: JsonCodec[On] =
    codecBoolVia(On) {
      case On  => "on"
      case Off => "off"
    }

  implicit lazy val codecReqCodeGroupId: JsonCodec[ReqCodeGroupId] =
    codecTaggedI(ReqCodeGroupId)

  implicit lazy val decoderReqCodeId: Decoder[ReqCodeId] = decodeSumBySoleKey {
    case ("a", c) => c.as[ApReqCodeId]
    case ("g", c) => c.as[ReqCodeGroupId]
  }

  implicit lazy val encoderReqCodeId: Encoder[ReqCodeId] = Encoder.instance {
    case a: ApReqCodeId    => Json.obj("a" -> a.asJson)
    case a: ReqCodeGroupId => Json.obj("g" -> a.asJson)
  }

  implicit lazy val codecReqCodeNode: JsonCodec[ReqCode.Node] =
    JsonCodec.xmap(ReqCode.Node.apply)(_.value) // xmap[String] already reuses

  implicit lazy val codecReqCodeValue: JsonCodec[ReqCode.Value] =
    codecNEV

  implicit lazy val decoderReqId: Decoder[ReqId] = decodeSumBySoleKey {
    case ("gr", c) => c.as[GenericReqId]
    case ("uc", c) => c.as[UseCaseId]
  }

  implicit lazy val encoderReqId: Encoder[ReqId] = Encoder.instance {
    case a: GenericReqId => Json.obj("gr" -> a.asJson)
    case a: UseCaseId    => Json.obj("uc" -> a.asJson)
  }

  implicit lazy val codecReqTypeMnemonic: JsonCodec[ReqType.Mnemonic] =
    codecTaggedS(ReqType.Mnemonic)

  implicit lazy val codecStaticFieldUseCaseStepTree: JsonCodec[StaticField.UseCaseStepTree] =
    JsonCodec.enumAdt(AdtMacros.adtIsoSet[StaticField.UseCaseStepTree, String] {
      case StaticField.NormalAltStepTree => "stepsNA"
      case StaticField.ExceptionStepTree => "stepsE"
    })

  implicit lazy val codecTagGroupId: JsonCodec[TagGroupId] =
    codecTaggedI(TagGroupId)

  implicit lazy val decoderTagId: Decoder[TagId] = decodeSumBySoleKey {
    case ("a", c) => c.as[ApplicableTagId]
    case ("g", c) => c.as[TagGroupId]
  }

  implicit lazy val encoderTagId: Encoder[TagId] = Encoder.instance {
    case a: ApplicableTagId => Json.obj("a" -> a.asJson)
    case a: TagGroupId      => Json.obj("g" -> a.asJson)
  }

  implicit lazy val codecUseCaseId: JsonCodec[UseCaseId] =
    codecTaggedI(UseCaseId)

  implicit lazy val codecUseCaseStepId: JsonCodec[UseCaseStepId] =
    codecTaggedI(UseCaseStepId)
}
