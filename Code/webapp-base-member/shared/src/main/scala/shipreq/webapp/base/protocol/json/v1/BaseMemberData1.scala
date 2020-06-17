package shipreq.webapp.base.protocol.json.v1

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.univeq.UnivEq
import nyaya.util.Multimap
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import shipreq.base.util.JsonUtil._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.issue.IssueCategory
import shipreq.webapp.base.protocol.json.JsonCodec
import shipreq.webapp.base.sort.SortMethod
import shipreq.webapp.base.text.AtomTC

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

  object AtomCodecs extends AtomTC[JsonCodec] {
    import shipreq.webapp.base.text._
    import Atom._

    override def lazily[A](f: => JsonCodec[A]): JsonCodec[A] = codecLazily(f)

    override def arr[A](implicit a: JsonCodec[A], ct: ClassTag[A]) = codecArraySeq

    override def nea[A](as: JsonCodec[ArraySeq[A]])(implicit a: JsonCodec[A]) = codecNEA(as.decoder, as.encoder)

    private[this] final val KeyBlankLine      = "bl"
    private[this] final val KeyCodeBlock      = "cb"
    private[this] final val KeyCodeRef        = "code"
    private[this] final val KeyEmailAddress   = "email"
    private[this] final val KeyIssue          = "issue"
    private[this] final val KeyLiteral        = "lit"
    private[this] final val KeyMonospace      = "mono"
    private[this] final val KeyReqRef         = "req"
    private[this] final val KeyTagRef         = "tag"
    private[this] final val KeyTeX            = "tex"
    private[this] final val KeyUnorderedList  = "ul"
    private[this] final val KeyUseCaseStepRef = "ucs"
    private[this] final val KeyWebAddress     = "web"

    override def sum[T <: Atom.Base](t: T)(get: Atom.Type => JsonCodec[t.Atom], all: List[JsonCodec[t.Atom]]): JsonCodec[t.Atom] = {
      JsonCodec[t.Atom](
        Encoder.instance[t.Atom] { a =>
          Atom.Type.of(a) match {
            case t@ Type.Literal        => Json.obj(KeyLiteral        -> get(t).encoder(a))
            case t@ Type.BlankLine      => Json.obj(KeyBlankLine      -> get(t).encoder(a))
            case t@ Type.CodeBlock      => Json.obj(KeyCodeBlock      -> get(t).encoder(a))
            case t@ Type.CodeRef        => Json.obj(KeyCodeRef        -> get(t).encoder(a))
            case t@ Type.EmailAddress   => Json.obj(KeyEmailAddress   -> get(t).encoder(a))
            case t@ Type.Issue          => Json.obj(KeyIssue          -> get(t).encoder(a))
            case t@ Type.Monospace      => Json.obj(KeyMonospace      -> get(t).encoder(a))
            case t@ Type.ReqRef         => Json.obj(KeyReqRef         -> get(t).encoder(a))
            case t@ Type.TagRef         => Json.obj(KeyTagRef         -> get(t).encoder(a))
            case t@ Type.TeX            => Json.obj(KeyTeX            -> get(t).encoder(a))
            case t@ Type.UnorderedList  => Json.obj(KeyUnorderedList  -> get(t).encoder(a))
            case t@ Type.UseCaseStepRef => Json.obj(KeyUseCaseStepRef -> get(t).encoder(a))
            case t@ Type.WebAddress     => Json.obj(KeyWebAddress     -> get(t).encoder(a))
          }
        },
        decodeSumBySoleKey[t.Atom] {
          case (KeyLiteral       , c) => get(Type.Literal       ).decoder.tryDecode(c)
          case (KeyBlankLine     , c) => get(Type.BlankLine     ).decoder.tryDecode(c)
          case (KeyCodeBlock     , c) => get(Type.CodeBlock     ).decoder.tryDecode(c)
          case (KeyCodeRef       , c) => get(Type.CodeRef       ).decoder.tryDecode(c)
          case (KeyEmailAddress  , c) => get(Type.EmailAddress  ).decoder.tryDecode(c)
          case (KeyIssue         , c) => get(Type.Issue         ).decoder.tryDecode(c)
          case (KeyMonospace     , c) => get(Type.Monospace     ).decoder.tryDecode(c)
          case (KeyReqRef        , c) => get(Type.ReqRef        ).decoder.tryDecode(c)
          case (KeyTagRef        , c) => get(Type.TagRef        ).decoder.tryDecode(c)
          case (KeyTeX           , c) => get(Type.TeX           ).decoder.tryDecode(c)
          case (KeyUnorderedList , c) => get(Type.UnorderedList ).decoder.tryDecode(c)
          case (KeyUseCaseStepRef, c) => get(Type.UseCaseStepRef).decoder.tryDecode(c)
          case (KeyWebAddress    , c) => get(Type.WebAddress    ).decoder.tryDecode(c)
        }
      )
    }

    override def blankLine[T <: NewLine](t: T): JsonCodec[t.BlankLine] =
      JsonCodec.const(t.blankLine)

    override def literal[T <: Literal](t: T): JsonCodec[t.Literal] =
      JsonCodec.xmap((i: String) => t.Literal(i))(_.value)

    override def codeBlock[T <: CodeBlock](t: T): JsonCodec[t.CodeBlock] =
      JsonCodec[t.CodeBlock](
        Encoder.forProduct2[t.CodeBlock, Option[String], String]("lang", "code")(a => (a.language, a.code)),
        Decoder.forProduct2[t.CodeBlock, Option[String], String]("lang", "code")(t.CodeBlock(_, _)))

    override def webAddress[T <: PlainTextMarkup](t: T): JsonCodec[t.WebAddress] =
      JsonCodec.xmap((i: String) => t.WebAddress(i))(_.value)

    override def monospace[T <: PlainTextMarkup](t: T): JsonCodec[t.Monospace] =
      JsonCodec.xmap((i: String) => t.Monospace(i))(_.value)

    override def emailAddress[T <: PlainTextMarkup](t: T): JsonCodec[t.EmailAddress] =
      JsonCodec.xmap((i: String) => t.EmailAddress(i))(_.value)

    override def teX[T <: PlainTextMarkup](t: T): JsonCodec[t.TeX] =
      JsonCodec.xmap((i: String) => t.TeX(i))(_.value)

    override def reqRef[T <: ContentRef](t: T): JsonCodec[t.ReqRef] =
      JsonCodec.xmap((i: ReqId) => t.ReqRef(i))(_.value)

    override def codeRef[T <: ContentRef](t: T): JsonCodec[t.CodeRef] =
      JsonCodec.xmap((i: ReqCodeId) => t.CodeRef(i))(_.value)

    override def useCaseStepRef[T <: ContentRef](t: T): JsonCodec[t.UseCaseStepRef] =
      JsonCodec.xmap((i: UseCaseStepId) => t.UseCaseStepRef(i))(_.value)

    override def tagRef[T <: TagRef](t: T): JsonCodec[t.TagRef] =
      JsonCodec.xmap((i: ApplicableTagId) => t.TagRef(i))(_.value)

    override def issue[T <: Issue](t: T)(implicit h: JsonCodec[Text.InlineIssueDesc.OptionalText]): JsonCodec[t.Issue] =
      JsonCodec[t.Issue](
        Encoder.forProduct2[t.Issue, CustomIssueTypeId, Text.InlineIssueDesc.OptionalText]("type", "desc")(a => (a.typ, a.desc)),
        Decoder.forProduct2[t.Issue, CustomIssueTypeId, Text.InlineIssueDesc.OptionalText]("type", "desc")(t.Issue(_, _)))

    override def unorderedList[T <: ListMarkup](t: T)(implicit h: JsonCodec[NonEmptyArraySeq[t.ListItem]]): JsonCodec[t.UnorderedList] =
      h.xmap((i: NonEmptyArraySeq[t.ListItem]) => t.UnorderedList(i))(_.items)
  }

  object SavedViewCodecs {
    import shipreq.webapp.base.data.savedview._

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
