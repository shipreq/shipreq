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
}
