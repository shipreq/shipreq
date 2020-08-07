package shipreq.webapp.base.protocol.json.v1

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.adt_macros.AdtMacros
import scala.collection.immutable.TreeSet
import scala.reflect.ClassTag
import shipreq.base.util.JsonUtil._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol.json.JsonCodec
import shipreq.webapp.base.text.AtomTC

/** v1.6 */
object Rev6 {
  import JsonCodec.Implicits._
  import BaseData._
  import BaseMemberData1._
  import Rev1._
  import Events._
  import PostEvents._

  object AtomCodecs extends AtomTC[JsonCodec] {
    import shipreq.webapp.base.text._
    import Atom._

    override def lazily[A](f: => JsonCodec[A]): JsonCodec[A] = codecLazily(f)

    override def xmap[A, B](fa: JsonCodec[A])(f: A => B)(g: B => A): JsonCodec[B] = fa.xmap(f)(g)

    override def arr[A](implicit a: JsonCodec[A], ct: ClassTag[A]) = codecArraySeq

    override def nea[A](as: JsonCodec[ArraySeq[A]])(implicit a: JsonCodec[A]) = codecNEA(as.decoder, as.encoder)

    override def str = JsonCodec.str

    private[this] final val KeyBlankLine      = "bl"
    private[this] final val KeyBold           = "**"
    private[this] final val KeyCodeBlock      = "cb"
    private[this] final val KeyCodeRef        = "code"
    private[this] final val KeyEmailAddress   = "email"
    private[this] final val KeyHeading1       = "h1"
    private[this] final val KeyHeading2       = "h2"
    private[this] final val KeyHeading3       = "h3"
    private[this] final val KeyHeading4       = "h4"
    private[this] final val KeyHeading5       = "h5"
    private[this] final val KeyHeading6       = "h6"
    private[this] final val KeyIssue          = "issue"
    private[this] final val KeyItalic         = "//"
    private[this] final val KeyLiteral        = "lit"
    private[this] final val KeyMonospace      = "mono"
    private[this] final val KeyOrderedList    = "ol"
    private[this] final val KeyReqRef         = "req"
    private[this] final val KeyStrikethrough  = "~~"
    private[this] final val KeyTagRef         = "tag"
    private[this] final val KeyTeX            = "tex"
    private[this] final val KeyUnderline      = "__"
    private[this] final val KeyUnorderedList  = "ul"
    private[this] final val KeyUseCaseStepRef = "ucs"
    private[this] final val KeyWebAddress     = "web"

    override def sum[T <: Atom.Base](t: T)(get: Atom.Type => JsonCodec[t.Atom], all: List[JsonCodec[t.Atom]]): JsonCodec[t.Atom] = {
      JsonCodec[t.Atom](
        Encoder.instance[t.Atom] { a =>
          Atom.Type.of(a) match {
            case t@ Type.Literal        => Json.obj(KeyLiteral        -> get(t).encoder(a))
            case t@ Type.BlankLine      => Json.obj(KeyBlankLine      -> get(t).encoder(a))
            case t@ Type.Bold           => Json.obj(KeyBold           -> get(t).encoder(a))
            case t@ Type.CodeBlock      => Json.obj(KeyCodeBlock      -> get(t).encoder(a))
            case t@ Type.CodeRef        => Json.obj(KeyCodeRef        -> get(t).encoder(a))
            case t@ Type.EmailAddress   => Json.obj(KeyEmailAddress   -> get(t).encoder(a))
            case t@ Type.Heading1       => Json.obj(KeyHeading1       -> get(t).encoder(a))
            case t@ Type.Heading2       => Json.obj(KeyHeading2       -> get(t).encoder(a))
            case t@ Type.Heading3       => Json.obj(KeyHeading3       -> get(t).encoder(a))
            case t@ Type.Heading4       => Json.obj(KeyHeading4       -> get(t).encoder(a))
            case t@ Type.Heading5       => Json.obj(KeyHeading5       -> get(t).encoder(a))
            case t@ Type.Heading6       => Json.obj(KeyHeading6       -> get(t).encoder(a))
            case t@ Type.Issue          => Json.obj(KeyIssue          -> get(t).encoder(a))
            case t@ Type.Italic         => Json.obj(KeyItalic         -> get(t).encoder(a))
            case t@ Type.Monospace      => Json.obj(KeyMonospace      -> get(t).encoder(a))
            case t@ Type.OrderedList    => Json.obj(KeyOrderedList    -> get(t).encoder(a))
            case t@ Type.ReqRef         => Json.obj(KeyReqRef         -> get(t).encoder(a))
            case t@ Type.Strikethrough  => Json.obj(KeyStrikethrough  -> get(t).encoder(a))
            case t@ Type.TagRef         => Json.obj(KeyTagRef         -> get(t).encoder(a))
            case t@ Type.TeX            => Json.obj(KeyTeX            -> get(t).encoder(a))
            case t@ Type.Underline      => Json.obj(KeyUnderline      -> get(t).encoder(a))
            case t@ Type.UnorderedList  => Json.obj(KeyUnorderedList  -> get(t).encoder(a))
            case t@ Type.UseCaseStepRef => Json.obj(KeyUseCaseStepRef -> get(t).encoder(a))
            case t@ Type.WebAddress     => Json.obj(KeyWebAddress     -> get(t).encoder(a))
          }
        },
        decodeSumBySoleKey[t.Atom] {
          case (KeyLiteral       , c) => get(Type.Literal       ).decoder.tryDecode(c)
          case (KeyBlankLine     , c) => get(Type.BlankLine     ).decoder.tryDecode(c)
          case (KeyBold          , c) => get(Type.Bold          ).decoder.tryDecode(c)
          case (KeyCodeBlock     , c) => get(Type.CodeBlock     ).decoder.tryDecode(c)
          case (KeyCodeRef       , c) => get(Type.CodeRef       ).decoder.tryDecode(c)
          case (KeyEmailAddress  , c) => get(Type.EmailAddress  ).decoder.tryDecode(c)
          case (KeyHeading1      , c) => get(Type.Heading1      ).decoder.tryDecode(c)
          case (KeyHeading2      , c) => get(Type.Heading2      ).decoder.tryDecode(c)
          case (KeyHeading3      , c) => get(Type.Heading3      ).decoder.tryDecode(c)
          case (KeyHeading4      , c) => get(Type.Heading4      ).decoder.tryDecode(c)
          case (KeyHeading5      , c) => get(Type.Heading5      ).decoder.tryDecode(c)
          case (KeyHeading6      , c) => get(Type.Heading6      ).decoder.tryDecode(c)
          case (KeyIssue         , c) => get(Type.Issue         ).decoder.tryDecode(c)
          case (KeyItalic        , c) => get(Type.Italic        ).decoder.tryDecode(c)
          case (KeyMonospace     , c) => get(Type.Monospace     ).decoder.tryDecode(c)
          case (KeyOrderedList   , c) => get(Type.OrderedList   ).decoder.tryDecode(c)
          case (KeyReqRef        , c) => get(Type.ReqRef        ).decoder.tryDecode(c)
          case (KeyStrikethrough , c) => get(Type.Strikethrough ).decoder.tryDecode(c)
          case (KeyTagRef        , c) => get(Type.TagRef        ).decoder.tryDecode(c)
          case (KeyTeX           , c) => get(Type.TeX           ).decoder.tryDecode(c)
          case (KeyUnderline     , c) => get(Type.Underline     ).decoder.tryDecode(c)
          case (KeyUnorderedList , c) => get(Type.UnorderedList ).decoder.tryDecode(c)
          case (KeyUseCaseStepRef, c) => get(Type.UseCaseStepRef).decoder.tryDecode(c)
          case (KeyWebAddress    , c) => get(Type.WebAddress    ).decoder.tryDecode(c)
        }
      )
    }

    override def blankLine[T <: NewLine](t: T): JsonCodec[t.BlankLine] =
      JsonCodec.const(t.blankLine)

    override def codeBlock[T <: CodeBlock](t: T): JsonCodec[t.CodeBlock] = {
      val encoder =
        Encoder.instance[t.CodeBlock](a =>
          Json.obj(
            "code" -> a.code.asJson,
            "lang" -> a.detail.map(_.language).asJson,
            "attr" -> a.detail.map(_.attributes).asJson,
          ).dropNullValues
        )

      val decoder =
        Decoder.instance[t.CodeBlock] { c =>
          for {
            code <- c.get[String]("code")
            lang <- c.get[Option[String]]("lang")
            attr <- c.get[Option[TreeSet[String]]]("attr")
          } yield {
            val detail = lang.map(CodeBlockDetail(_, attr.getOrElse(TreeSet.empty)))
            t.CodeBlock(detail, code)
          }
        }

      JsonCodec[t.CodeBlock](encoder, decoder)
    }

    private implicit val reqRefOptions: JsonCodec[DisplayReqRef] =
      JsonCodec.enumAdt(AdtMacros.adtIsoSet[DisplayReqRef, String] {
        case DisplayReqRef.AsId         => "id"
        case DisplayReqRef.AsIdAndTitle => "id: title"
      })

    private def abstractReqRef[A, I](make: (I, DisplayReqRef) => A)
                                    (getI: A => I,
                                     getD: A => DisplayReqRef)
                                    (implicit di: Decoder[I],
                                     ei: Encoder[I]): JsonCodec[A] = {

      val encoder = Encoder.instance[A] { a =>
        val id = ei(getI(a))
        getD(a) match {
          case DisplayReqRef.AsId => id
          case d =>
            Json.obj(
              "id"      -> id,
              "display" -> d.asJson
            )
        }
      }

      val decoder = Decoder.instance[A] { c =>
        di(c) match {
          case Right(i) => Right(make(i, DisplayReqRef.AsId))
          case Left(_) =>
            for {
              i <- c.get[I]("id")
              d <- c.get[DisplayReqRef]("display")
            } yield make(i, d)
        }
      }

      JsonCodec(encoder, decoder)
    }

    override def reqRef[T <: ContentRef](t: T): JsonCodec[t.ReqRef] =
      abstractReqRef[t.ReqRef, ReqId](t.ReqRef(_, _))(_.id, _.display)

    override def codeRef[T <: ContentRef](t: T): JsonCodec[t.CodeRef] =
      abstractReqRef[t.CodeRef, ReqCodeId](t.CodeRef(_, _))(_.id, _.display)

    override def useCaseStepRef[T <: ContentRef](t: T): JsonCodec[t.UseCaseStepRef] =
      JsonCodec.xmap((i: UseCaseStepId) => t.UseCaseStepRef(i))(_.value)

    override def tagRef[T <: TagRef](t: T): JsonCodec[t.TagRef] =
      JsonCodec.xmap((i: ApplicableTagId) => t.TagRef(i))(_.value)

    override def issue[T <: Issue](t: T)(implicit h: JsonCodec[Text.InlineIssueDesc.OptionalText]): JsonCodec[t.Issue] =
      JsonCodec[t.Issue](
        Encoder.forProduct2[t.Issue, CustomIssueTypeId, Text.InlineIssueDesc.OptionalText]("type", "desc")(a => (a.typ, a.desc)),
        Decoder.forProduct2[t.Issue, CustomIssueTypeId, Text.InlineIssueDesc.OptionalText]("type", "desc")(t.Issue(_, _)))

    override def orderedList[T <: ListMarkup](t: T)(implicit h: JsonCodec[NonEmptyArraySeq[t.ListItem]]): JsonCodec[t.OrderedList] =
      h.xmap((i: NonEmptyArraySeq[t.ListItem]) => t.OrderedList(i))(_.items)

    override def unorderedList[T <: ListMarkup](t: T)(implicit h: JsonCodec[NonEmptyArraySeq[t.ListItem]]): JsonCodec[t.UnorderedList] =
      h.xmap((i: NonEmptyArraySeq[t.ListItem]) => t.UnorderedList(i))(_.items)
  }

  import AtomCodecs.instances._

  // ===================================================================================================================

  private[v1] implicit lazy val codecEnabled: JsonCodec[Enabled] =
    codecBool(Enabled)

  private[v1] implicit lazy val codecDerivativeTagsTagPair: JsonCodec[DerivativeTags.TagPair] =
    JsonCodec.xemap((p: DerivativeTags.TagPair) => Array(p.lo, p.hi))(
      ids => ids.length match {
        case 2 => Right(DerivativeTags.TagPair(ids(0), ids(1)))
        case _ => Left(DecodingFailure("Expected array of two tag ids", Nil))
      }
    )

  private[v1] implicit lazy val codecDerivativeTagsRules: JsonCodec[DerivativeTags.Rules] = {
    import DerivativeTags.TagPair

    implicit val keyEnc: KeyEncoder[TagPair] =
      KeyEncoder.instance(p => s"${p.lo.value}+${p.hi.value}")

    val regex = """^(\d+?)\+(\d+)$""".r

    implicit val keyDec: KeyDecoder[TagPair] =
      KeyDecoder.instance {
        case regex(lo, hi) => Some(TagPair(ApplicableTagId(lo.toInt), ApplicableTagId(hi.toInt)))
        case _             => None
      }

    JsonCodec.map
  }

  private[v1] implicit lazy val codecDerivativeTags: JsonCodec[DerivativeTags] =
    JsonCodec(
      Encoder.forProduct2("enabled", "rules")(a => (a.enabled, a.rules)),
      Decoder.forProduct2("enabled", "rules")(DerivativeTags.apply))

  private[v1] implicit lazy val codecCustomTagFieldGD: JsonCodec[CustomTagFieldGD.NonEmptyValues] = {
    import CustomTagFieldGD._

    implicit val codecValueForFieldReqTypeRules = JsonCodec.xmap(ValueForFieldReqTypeRules.apply)(_.value)
    implicit val codecValueForDerivativeTags    = JsonCodec.xmap(ValueForDerivativeTags   .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("reqTypes",   c) => c.as[ValueForFieldReqTypeRules]
      case ("derivation", c) => c.as[ValueForDerivativeTags]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForFieldReqTypeRules => Json.obj("reqTypes"   -> a.asJson)
      case a: ValueForDerivativeTags    => Json.obj("derivation" -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit val codecEventNonEmptyCustomTextMap: JsonCodec[Event.NonEmptyCustomTextMap] =
    codecNonEmptyMono

  private[v1] implicit val codecCodeGroupGD: JsonCodec[CodeGroupGD.NonEmptyValues] = {
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

  private[v1] implicit val codecGenericReqGD: JsonCodec[GenericReqGD.Values] = {
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

  private[v1] implicit val codecUseCaseGD: JsonCodec[UseCaseGD.Values] = {
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

  private[v1] implicit val codecUseCaseStepGD: JsonCodec[UseCaseStepGD.NonEmptyValues] = {
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

  object EventData {

    implicit val decoderEventFieldCustomTagCreate: Decoder[Event.FieldCustomTagCreate] =
      Decoder.forProduct3("id", "tagId", "values")(Event.FieldCustomTagCreate.apply)

    implicit val encoderEventFieldCustomTagCreate: Encoder[Event.FieldCustomTagCreate] =
      Encoder.forProduct3("id", "tagId", "values")(a => (a.id, a.tagId, a.vs))

    implicit val decoderEventFieldCustomTagUpdate: Decoder[Event.FieldCustomTagUpdate] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomTagUpdate.apply)

    implicit val encoderEventFieldCustomTagUpdate: Encoder[Event.FieldCustomTagUpdate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventGenericReqCreate: Decoder[Event.GenericReqCreate] =
      Decoder.forProduct3("reqId", "reqTypeId", "values")(Event.GenericReqCreate.apply)

    implicit val encoderEventGenericReqCreate: Encoder[Event.GenericReqCreate] =
      Encoder.forProduct3("reqId", "reqTypeId", "values")(a => (a.id, a.rt, a.vs))

    implicit val decoderEventCodeGroupCreate: Decoder[Event.CodeGroupCreate] =
      Decoder.forProduct2("id", "values")(Event.CodeGroupCreate.apply)

    implicit val encoderEventCodeGroupCreate: Encoder[Event.CodeGroupCreate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventCodeGroupUpdate: Decoder[Event.CodeGroupUpdate] =
      Decoder.forProduct2("id", "values")(Event.CodeGroupUpdate.apply)

    implicit val encoderEventCodeGroupUpdate: Encoder[Event.CodeGroupUpdate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventReqsDelete: Decoder[Event.ReqsDelete] =
      Decoder.forProduct3("reqs", "codeGroups", "reason")(Event.ReqsDelete.apply)

    implicit val encoderEventReqsDelete: Encoder[Event.ReqsDelete] =
      Encoder.forProduct3("reqs", "codeGroups", "reason")(a => (a.reqs, a.codeGroups, a.reason))

    implicit val decoderEventReqFieldCustomTextSet: Decoder[Event.ReqFieldCustomTextSet] =
      Decoder.forProduct3("id", "fid", "value")(Event.ReqFieldCustomTextSet.apply)

    implicit val encoderEventReqFieldCustomTextSet: Encoder[Event.ReqFieldCustomTextSet] =
      Encoder.forProduct3("id", "fid", "value")(a => (a.id, a.fid, a.value))

    implicit val decoderEventManualIssueCreate: Decoder[Event.ManualIssueCreate] =
      Decoder.forProduct2("id", "text")(Event.ManualIssueCreate.apply)

    implicit val encoderEventManualIssueCreate: Encoder[Event.ManualIssueCreate] =
      Encoder.forProduct2("id", "text")(a => (a.id, a.text))

    implicit val decoderEventManualIssueUpdate: Decoder[Event.ManualIssueUpdate] =
      Decoder.forProduct2("id", "text")(Event.ManualIssueUpdate.apply)

    implicit val encoderEventManualIssueUpdate: Encoder[Event.ManualIssueUpdate] =
      Encoder.forProduct2("id", "text")(a => (a.id, a.text))

    implicit val decoderEventGenericReqTitleSet: Decoder[Event.GenericReqTitleSet] =
      Decoder.forProduct2("id", "value")(Event.GenericReqTitleSet.apply)

    implicit val encoderEventGenericReqTitleSet: Encoder[Event.GenericReqTitleSet] =
      Encoder.forProduct2("id", "value")(a => (a.id, a.value))

    implicit val decoderEventUseCaseCreate: Decoder[Event.UseCaseCreate] =
      Decoder.forProduct3("id", "stepId", "values")(Event.UseCaseCreate.apply)

    implicit val encoderEventUseCaseCreate: Encoder[Event.UseCaseCreate] =
      Encoder.forProduct3("id", "stepId", "values")(a => (a.id, a.stepId, a.vs))

    implicit val decoderEventUseCaseStepUpdate: Decoder[Event.UseCaseStepUpdate] =
      Decoder.forProduct2("id", "values")(Event.UseCaseStepUpdate.apply)

    implicit val encoderEventUseCaseStepUpdate: Encoder[Event.UseCaseStepUpdate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventUseCaseTitleSet: Decoder[Event.UseCaseTitleSet] =
      Decoder.forProduct2("id", "value")(Event.UseCaseTitleSet.apply)

    implicit val encoderEventUseCaseTitleSet: Encoder[Event.UseCaseTitleSet] =
      Encoder.forProduct2("id", "value")(a => (a.id, a.value))
  }

  // ===================================================================================================================

  import Events.EventData._
  import Rev1.EventData._
  import Rev5.EventData._
  import EventData._

  implicit lazy val decoderEvent: Decoder[Event] = decodeSumBySoleKey {
    case ("ApplicableTagCreate"    , c) => c.as[Event.ApplicableTagCreateV1]
    case ("ApplicableTagCreate:2"  , c) => c.as[Event.ApplicableTagCreate]
    case ("ApplicableTagUpdate"    , c) => c.as[Event.ApplicableTagUpdateV1]
    case ("ApplicableTagUpdate:2"  , c) => c.as[Event.ApplicableTagUpdate]
    case ("CodeGroupCreate"        , c) => c.as[Event.CodeGroupCreate]
    case ("CodeGroupUpdate"        , c) => c.as[Event.CodeGroupUpdate]
    case ("CodeGroupsDelete"       , c) => c.as[Event.CodeGroupsDelete]
    case ("ContentRestore"         , c) => c.as[Event.ContentRestore]
    case ("CustomIssueTypeCreate"  , c) => c.as[Event.CustomIssueTypeCreate]
    case ("CustomIssueTypeDelete"  , c) => c.as[Event.CustomIssueTypeDelete]
    case ("CustomIssueTypeRestore" , c) => c.as[Event.CustomIssueTypeRestore]
    case ("CustomIssueTypeUpdate"  , c) => c.as[Event.CustomIssueTypeUpdate]
    case ("CustomReqTypeCreate"    , c) => c.as[Event.CustomReqTypeCreateV1]
    case ("CustomReqTypeCreate:2"  , c) => c.as[Event.CustomReqTypeCreate]
    case ("CustomReqTypeDelete"    , c) => c.as[Event.CustomReqTypeDelete]
    case ("CustomReqTypeDeleteHard", c) => c.as[Event.CustomReqTypeDeleteHard]
    case ("CustomReqTypeDeleteSoft", c) => c.as[Event.CustomReqTypeDeleteSoft]
    case ("CustomReqTypeRestore"   , c) => c.as[Event.CustomReqTypeRestore]
    case ("CustomReqTypeUpdate"    , c) => c.as[Event.CustomReqTypeUpdateV1]
    case ("CustomReqTypeUpdate:2"  , c) => c.as[Event.CustomReqTypeUpdate]
    case ("FieldCustomDelete"      , c) => c.as[Event.FieldCustomDelete]
    case ("FieldCustomImpCreate"   , c) => c.as[Event.FieldCustomImpCreateV1]
    case ("FieldCustomImpCreate:2" , c) => c.as[Event.FieldCustomImpCreate]
    case ("FieldCustomImpUpdate"   , c) => c.as[Event.FieldCustomImpUpdateV1]
    case ("FieldCustomImpUpdate:2" , c) => c.as[Event.FieldCustomImpUpdate]
    case ("FieldCustomRestore"     , c) => c.as[Event.FieldCustomRestore]
    case ("FieldCustomTagCreate"   , c) => c.as[Event.FieldCustomTagCreateV1]
    case ("FieldCustomTagCreate:2" , c) => c.as[Event.FieldCustomTagCreate]
    case ("FieldCustomTagUpdate"   , c) => c.as[Event.FieldCustomTagUpdateV1]
    case ("FieldCustomTagUpdate:2" , c) => c.as[Event.FieldCustomTagUpdate]
    case ("FieldCustomTextCreate"  , c) => c.as[Event.FieldCustomTextCreateV1]
    case ("FieldCustomTextCreate:2", c) => c.as[Event.FieldCustomTextCreate]
    case ("FieldCustomTextUpdate"  , c) => c.as[Event.FieldCustomTextUpdateV1]
    case ("FieldCustomTextUpdate:2", c) => c.as[Event.FieldCustomTextUpdate]
    case ("FieldReposition"        , c) => c.as[Event.FieldReposition]
    case ("FieldStaticAdd"         , c) => c.as[Event.FieldStaticAdd]
    case ("FieldStaticRemove"      , c) => c.as[Event.FieldStaticRemove]
    case ("GenericReqCreate"       , c) => c.as[Event.GenericReqCreate]
    case ("GenericReqTitleSet"     , c) => c.as[Event.GenericReqTitleSet]
    case ("GenericReqTypeSet"      , c) => c.as[Event.GenericReqTypeSet]
    case ("ManualIssueCreate"      , c) => c.as[Event.ManualIssueCreate]
    case ("ManualIssueDelete"      , c) => c.as[Event.ManualIssueDelete]
    case ("ManualIssueUpdate"      , c) => c.as[Event.ManualIssueUpdate]
    case ("ProjectNameSet"         , c) => c.as[Event.ProjectNameSet]
    case ("ProjectTemplateApply"   , c) => c.as[Event.ProjectTemplateApply]
    case ("ReqCodesPatch"          , c) => c.as[Event.ReqCodesPatch]
    case ("ReqFieldCustomTextSet"  , c) => c.as[Event.ReqFieldCustomTextSet]
    case ("ReqImplicationsPatch"   , c) => c.as[Event.ReqImplicationsPatch]
    case ("ReqTagsPatch"           , c) => c.as[Event.ReqTagsPatch]
    case ("ReqsDelete"             , c) => c.as[Event.ReqsDelete]
    case ("SavedViewCreate"        , c) => c.as[Event.SavedViewCreateV1]
    case ("SavedViewCreate:2"      , c) => c.as[Event.SavedViewCreate]
    case ("SavedViewDefaultSet"    , c) => c.as[Event.SavedViewDefaultSet]
    case ("SavedViewDelete"        , c) => c.as[Event.SavedViewDelete]
    case ("SavedViewUpdate"        , c) => c.as[Event.SavedViewUpdateV1]
    case ("SavedViewUpdate:2"      , c) => c.as[Event.SavedViewUpdate]
    case ("TagDelete"              , c) => c.as[Event.TagDelete]
    case ("TagGroupCreate"         , c) => c.as[Event.TagGroupCreate]
    case ("TagGroupUpdate"         , c) => c.as[Event.TagGroupUpdate]
    case ("TagRestore"             , c) => c.as[Event.TagRestore]
    case ("UseCaseCreate"          , c) => c.as[Event.UseCaseCreate]
    case ("UseCaseStepCreate"      , c) => c.as[Event.UseCaseStepCreate]
    case ("UseCaseStepDelete"      , c) => c.as[Event.UseCaseStepDelete]
    case ("UseCaseStepRestore"     , c) => c.as[Event.UseCaseStepRestore]
    case ("UseCaseStepShiftLeft"   , c) => c.as[Event.UseCaseStepShiftLeft]
    case ("UseCaseStepShiftRight"  , c) => c.as[Event.UseCaseStepShiftRight]
    case ("UseCaseStepUpdate"      , c) => c.as[Event.UseCaseStepUpdate]
    case ("UseCaseTitleSet"        , c) => c.as[Event.UseCaseTitleSet]
  }

  implicit lazy val encoderEvent: Encoder[Event] = Encoder.instance {
    case a: Event.ApplicableTagCreateV1   => Json.obj("ApplicableTagCreate"     -> a.asJson)
    case a: Event.ApplicableTagCreate     => Json.obj("ApplicableTagCreate:2"   -> a.asJson)
    case a: Event.ApplicableTagUpdateV1   => Json.obj("ApplicableTagUpdate"     -> a.asJson)
    case a: Event.ApplicableTagUpdate     => Json.obj("ApplicableTagUpdate:2"   -> a.asJson)
    case a: Event.CodeGroupCreate         => Json.obj("CodeGroupCreate"         -> a.asJson)
    case a: Event.CodeGroupUpdate         => Json.obj("CodeGroupUpdate"         -> a.asJson)
    case a: Event.CodeGroupsDelete        => Json.obj("CodeGroupsDelete"        -> a.asJson)
    case a: Event.ContentRestore          => Json.obj("ContentRestore"          -> a.asJson)
    case a: Event.CustomIssueTypeCreate   => Json.obj("CustomIssueTypeCreate"   -> a.asJson)
    case a: Event.CustomIssueTypeDelete   => Json.obj("CustomIssueTypeDelete"   -> a.asJson)
    case a: Event.CustomIssueTypeRestore  => Json.obj("CustomIssueTypeRestore"  -> a.asJson)
    case a: Event.CustomIssueTypeUpdate   => Json.obj("CustomIssueTypeUpdate"   -> a.asJson)
    case a: Event.CustomReqTypeCreateV1   => Json.obj("CustomReqTypeCreate"     -> a.asJson)
    case a: Event.CustomReqTypeCreate     => Json.obj("CustomReqTypeCreate:2"   -> a.asJson)
    case a: Event.CustomReqTypeDelete     => Json.obj("CustomReqTypeDelete"     -> a.asJson)
    case a: Event.CustomReqTypeDeleteHard => Json.obj("CustomReqTypeDeleteHard" -> a.asJson)
    case a: Event.CustomReqTypeDeleteSoft => Json.obj("CustomReqTypeDeleteSoft" -> a.asJson)
    case a: Event.CustomReqTypeRestore    => Json.obj("CustomReqTypeRestore"    -> a.asJson)
    case a: Event.CustomReqTypeUpdateV1   => Json.obj("CustomReqTypeUpdate"     -> a.asJson)
    case a: Event.CustomReqTypeUpdate     => Json.obj("CustomReqTypeUpdate:2"   -> a.asJson)
    case a: Event.FieldCustomDelete       => Json.obj("FieldCustomDelete"       -> a.asJson)
    case a: Event.FieldCustomImpCreateV1  => Json.obj("FieldCustomImpCreate"    -> a.asJson)
    case a: Event.FieldCustomImpCreate    => Json.obj("FieldCustomImpCreate:2"  -> a.asJson)
    case a: Event.FieldCustomImpUpdateV1  => Json.obj("FieldCustomImpUpdate"    -> a.asJson)
    case a: Event.FieldCustomImpUpdate    => Json.obj("FieldCustomImpUpdate:2"  -> a.asJson)
    case a: Event.FieldCustomRestore      => Json.obj("FieldCustomRestore"      -> a.asJson)
    case a: Event.FieldCustomTagCreateV1  => Json.obj("FieldCustomTagCreate"    -> a.asJson)
    case a: Event.FieldCustomTagCreate    => Json.obj("FieldCustomTagCreate:2"  -> a.asJson)
    case a: Event.FieldCustomTagUpdateV1  => Json.obj("FieldCustomTagUpdate"    -> a.asJson)
    case a: Event.FieldCustomTagUpdate    => Json.obj("FieldCustomTagUpdate:2"  -> a.asJson)
    case a: Event.FieldCustomTextCreateV1 => Json.obj("FieldCustomTextCreate"   -> a.asJson)
    case a: Event.FieldCustomTextCreate   => Json.obj("FieldCustomTextCreate:2" -> a.asJson)
    case a: Event.FieldCustomTextUpdateV1 => Json.obj("FieldCustomTextUpdate"   -> a.asJson)
    case a: Event.FieldCustomTextUpdate   => Json.obj("FieldCustomTextUpdate:2" -> a.asJson)
    case a: Event.FieldReposition         => Json.obj("FieldReposition"         -> a.asJson)
    case a: Event.FieldStaticAdd          => Json.obj("FieldStaticAdd"          -> a.asJson)
    case a: Event.FieldStaticRemove       => Json.obj("FieldStaticRemove"       -> a.asJson)
    case a: Event.GenericReqCreate        => Json.obj("GenericReqCreate"        -> a.asJson)
    case a: Event.GenericReqTitleSet      => Json.obj("GenericReqTitleSet"      -> a.asJson)
    case a: Event.GenericReqTypeSet       => Json.obj("GenericReqTypeSet"       -> a.asJson)
    case a: Event.ManualIssueCreate       => Json.obj("ManualIssueCreate"       -> a.asJson)
    case a: Event.ManualIssueDelete       => Json.obj("ManualIssueDelete"       -> a.asJson)
    case a: Event.ManualIssueUpdate       => Json.obj("ManualIssueUpdate"       -> a.asJson)
    case a: Event.ProjectNameSet          => Json.obj("ProjectNameSet"          -> a.asJson)
    case a: Event.ProjectTemplateApply    => Json.obj("ProjectTemplateApply"    -> a.asJson)
    case a: Event.ReqCodesPatch           => Json.obj("ReqCodesPatch"           -> a.asJson)
    case a: Event.ReqFieldCustomTextSet   => Json.obj("ReqFieldCustomTextSet"   -> a.asJson)
    case a: Event.ReqImplicationsPatch    => Json.obj("ReqImplicationsPatch"    -> a.asJson)
    case a: Event.ReqTagsPatch            => Json.obj("ReqTagsPatch"            -> a.asJson)
    case a: Event.ReqsDelete              => Json.obj("ReqsDelete"              -> a.asJson)
    case a: Event.SavedViewCreateV1       => Json.obj("SavedViewCreate"         -> a.asJson)
    case a: Event.SavedViewCreate         => Json.obj("SavedViewCreate:2"       -> a.asJson)
    case a: Event.SavedViewDefaultSet     => Json.obj("SavedViewDefaultSet"     -> a.asJson)
    case a: Event.SavedViewDelete         => Json.obj("SavedViewDelete"         -> a.asJson)
    case a: Event.SavedViewUpdateV1       => Json.obj("SavedViewUpdate"         -> a.asJson)
    case a: Event.SavedViewUpdate         => Json.obj("SavedViewUpdate:2"       -> a.asJson)
    case a: Event.TagDelete               => Json.obj("TagDelete"               -> a.asJson)
    case a: Event.TagGroupCreate          => Json.obj("TagGroupCreate"          -> a.asJson)
    case a: Event.TagGroupUpdate          => Json.obj("TagGroupUpdate"          -> a.asJson)
    case a: Event.TagRestore              => Json.obj("TagRestore"              -> a.asJson)
    case a: Event.UseCaseCreate           => Json.obj("UseCaseCreate"           -> a.asJson)
    case a: Event.UseCaseStepCreate       => Json.obj("UseCaseStepCreate"       -> a.asJson)
    case a: Event.UseCaseStepDelete       => Json.obj("UseCaseStepDelete"       -> a.asJson)
    case a: Event.UseCaseStepRestore      => Json.obj("UseCaseStepRestore"      -> a.asJson)
    case a: Event.UseCaseStepShiftLeft    => Json.obj("UseCaseStepShiftLeft"    -> a.asJson)
    case a: Event.UseCaseStepShiftRight   => Json.obj("UseCaseStepShiftRight"   -> a.asJson)
    case a: Event.UseCaseStepUpdate       => Json.obj("UseCaseStepUpdate"       -> a.asJson)
    case a: Event.UseCaseTitleSet         => Json.obj("UseCaseTitleSet"         -> a.asJson)
  }

  implicit lazy val decoderVerifiedEvent: Decoder[VerifiedEvent] =
    Decoder.forProduct3("#", "event", "createdAt")(VerifiedEvent.apply)

  implicit lazy val encoderVerifiedEvent: Encoder[VerifiedEvent] =
    Encoder.forProduct3("#", "event", "createdAt")(a => (a.ord, a.event, a.createdAt))

}
