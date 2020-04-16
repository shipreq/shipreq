package shipreq.webapp.base.protocol.json.v1

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.univeq.UnivEq
import nyaya.util.Multimap
import shipreq.base.util._
import shipreq.base.util.JsonUtil._
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

    override def vec[A](implicit a: JsonCodec[A]) = JsonCodec.summon

    override def nev[A](as: JsonCodec[Vector[A]])(implicit a: JsonCodec[A]) = codecNEV

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

    override def unorderedList[T <: ListMarkup](t: T)(implicit h: JsonCodec[NonEmptyVector[t.ListItem]]): JsonCodec[t.UnorderedList] =
      h.xmap((i: NonEmptyVector[t.ListItem]) => t.UnorderedList(i))(_.items)
  }

  object ReqTableDataCodecs {
    import reqtable._

    implicit val codecColumnImplications: JsonCodec[Column.Implications] =
      JsonCodec.xmap(Column.Implications.apply)(_.dir)

    implicit val codecColumnCustomField: JsonCodec[Column.CustomField] =
      JsonCodec.xmap(Column.CustomField.apply)(_.id)

    private[this] final val KeyPubid          = "pubid"

    // Replaced by v1.1
    // private[this] final val KeyCustomField    = "custom"
    // private[this] final val KeyImplications   = "imps"
    // private[this] final val KeyCode           = "code"
    // private[this] final val KeyDeletionReason = "delReason"
    // private[this] final val KeyReqType        = "reqType"
    // private[this] final val KeyTags           = "tags"
    // private[this] final val KeyTitle          = "title"
    //
    // implicit val decoderColumn: Decoder[Column] = decodeSumBySoleKeyOrConst[Column](
    //   KeyCode           -> Column.Code,
    //   KeyDeletionReason -> Column.DeletionReason,
    //   KeyPubid          -> Column.Pubid,
    //   KeyReqType        -> Column.ReqType,
    //   KeyTags           -> Column.Tags,
    //   KeyTitle          -> Column.Title,
    // ) {
    //   case (KeyCustomField   , c) => c.as[Column.CustomField]
    //   case (KeyImplications  , c) => c.as[Column.Implications]
    // }
    //
    // implicit val encoderColumn: Encoder[Column] = Encoder.instance {
    //   case Column.Code            => Json.fromString(KeyCode)
    //   case Column.DeletionReason  => Json.fromString(KeyDeletionReason)
    //   case Column.Pubid           => Json.fromString(KeyPubid)
    //   case Column.ReqType         => Json.fromString(KeyReqType)
    //   case Column.Tags            => Json.fromString(KeyTags)
    //   case Column.Title           => Json.fromString(KeyTitle)
    //   case a: Column.CustomField  => Json.obj(KeyCustomField  -> a.asJson)
    //   case a: Column.Implications => Json.obj(KeyImplications -> a.asJson)
    // }

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

    // Replaced by v1.1
    // implicit val decoderColumnSortInconclusive: Decoder[Column.SortInconclusive] =
    //   decodeSumBySoleKeyOrConst[Column.SortInconclusive](
    //     KeyCode           -> Column.Code,
    //     KeyDeletionReason -> Column.DeletionReason,
    //     KeyReqType        -> Column.ReqType,
    //     KeyTags           -> Column.OtherTags,
    //     KeyTitle          -> Column.Title,
    //   ) {
    //     case (KeyCustomField , c) => c.as[Column.CustomField]
    //     case (KeyImplications, c) => c.as[Column.Implications]
    //   }
    //
    // implicit val encoderColumnSortInconclusive: Encoder[Column.SortInconclusive] = Encoder.instance {
    //   case Column.Code            => Json.fromString(KeyCode)
    //   case Column.DeletionReason  => Json.fromString(KeyDeletionReason)
    //   case Column.ReqType         => Json.fromString(KeyReqType)
    //   case Column.OtherTags            => Json.fromString(KeyTags)
    //   case Column.Title           => Json.fromString(KeyTitle)
    //   case a: Column.CustomField  => Json.obj(KeyCustomField  -> a.asJson)
    //   case a: Column.Implications => Json.obj(KeyImplications -> a.asJson)
    // }
    //
    // implicit val decoderColumnSortInconclusiveHasBlanks: Decoder[Column.SortInconclusiveHasBlanks] =
    //   decodeSumBySoleKeyOrConst[Column.SortInconclusiveHasBlanks](
    //     KeyCode           -> Column.Code,
    //     KeyDeletionReason -> Column.DeletionReason,
    //     KeyTags           -> Column.OtherTags,
    //     KeyTitle          -> Column.Title,
    //   ) {
    //   case (KeyCustomField , c) => c.as[Column.CustomField]
    //   case (KeyImplications, c) => c.as[Column.Implications]
    // }
    //
    // implicit val encoderColumnSortInconclusiveHasBlanks: Encoder[Column.SortInconclusiveHasBlanks] = Encoder.instance {
    //   case Column.Code            => Json.fromString(KeyCode)
    //   case Column.DeletionReason  => Json.fromString(KeyDeletionReason)
    //   case Column.OtherTags            => Json.fromString(KeyTags)
    //   case Column.Title           => Json.fromString(KeyTitle)
    //   case a: Column.CustomField  => Json.obj(KeyCustomField  -> a.asJson)
    //   case a: Column.Implications => Json.obj(KeyImplications -> a.asJson)
    // }
    //
    // implicit val codecColumnSortInconclusiveNoBlanks: JsonCodec[Column.SortInconclusiveNoBlanks] =
    //   JsonCodec.enumAdt(AdtMacros.adtIsoSet[Column.SortInconclusiveNoBlanks, String] {
    //     case Column.ReqType => KeyReqType
    //   })
    //
    // implicit val codecColumnNEV: JsonCodec[NonEmptyVector[Column]] =
    //   codecNEV

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

    // Replaced by v1.1
    // implicit val decoderSortCriterionInconclusiveCB: Decoder[SortCriterion.InconclusiveCB] =
    //   Decoder.forProduct2("column", "method")(SortCriterion.InconclusiveCB.apply)
    //
    // implicit val encoderSortCriterionInconclusiveCB: Encoder[SortCriterion.InconclusiveCB] =
    //   Encoder.forProduct2("column", "method")(a => (a.column, a.method))
    //
    // implicit val decoderSortCriterionInconclusiveIB: Decoder[SortCriterion.InconclusiveIB] =
    //   Decoder.forProduct2("column", "method")(SortCriterion.InconclusiveIB.apply)
    //
    // implicit val encoderSortCriterionInconclusiveIB: Encoder[SortCriterion.InconclusiveIB] =
    //   Encoder.forProduct2("column", "method")(a => (a.column, a.method))

    implicit val decoderSortCriterionConclusive: Decoder[SortCriterion.Conclusive] =
      Decoder.forProduct2("column", "method")(SortCriterion.Conclusive.apply)

    implicit val encoderSortCriterionConclusive: Encoder[SortCriterion.Conclusive] =
      Encoder.forProduct2("column", "method")(a => (a.column, a.method))

    // Replaced by v1.1
    // implicit val decoderSortCriterionI: Decoder[SortCriterion.Inconclusive] = decodeSumBySoleKey {
    //   case ("CB", c) => c.as[SortCriterion.InconclusiveCB]
    //   case ("IB", c) => c.as[SortCriterion.InconclusiveIB]
    // }
    //
    // implicit val encoderSortCriterionI: Encoder[SortCriterion.Inconclusive] = Encoder.instance {
    //   case a: SortCriterion.InconclusiveCB => Json.obj("CB" -> a.asJson)
    //   case a: SortCriterion.InconclusiveIB => Json.obj("IB" -> a.asJson)
    // }
    //
    // implicit val decoderSortCriterion: Decoder[SortCriterion] = decodeSumBySoleKey {
    //   case ("c" , c) => c.as[SortCriterion.Conclusive]
    //   case ("CB", c) => c.as[SortCriterion.InconclusiveCB]
    //   case ("IB", c) => c.as[SortCriterion.InconclusiveIB]
    // }
    //
    // implicit val encoderSortCriterion: Encoder[SortCriterion] = Encoder.instance {
    //   case a: SortCriterion.Conclusive     => Json.obj("c"  -> a.asJson)
    //   case a: SortCriterion.InconclusiveCB => Json.obj("CB" -> a.asJson)
    //   case a: SortCriterion.InconclusiveIB => Json.obj("IB" -> a.asJson)
    // }
    //
    // implicit val decoderSortCriteria: Decoder[SortCriteria] =
    //   Decoder.forProduct2("init", "last")(SortCriteria.apply)
    //
    // implicit val encoderSortCriteria: Encoder[SortCriteria] =
    //   Encoder.forProduct2("init", "last")(a => (a.init, a.last))
    //
    // implicit val decoderView: Decoder[View] =
    //   Decoder.forProduct4("columns", "order", "filterDead", "filter")(View.apply)
    //
    // implicit val encoderView: Encoder[View] =
    //   Encoder.forProduct4("columns", "order", "filterDead", "filter")(a => (a.columns, a.order, a.filterDead, a.filter))

    implicit val codecSavedViewId: JsonCodec[SavedView.Id] =
      JsonCodec.xmap(SavedView.Id.apply)(_.value)

    implicit val codecSavedViewName: JsonCodec[SavedView.Name] =
      JsonCodec.xmap(SavedView.Name.apply)(_.value)

    // Replaced by v1.1
    // implicit val decoderSavedView: Decoder[SavedView] =
    //   Decoder.forProduct3("id", "name", "view")(SavedView.apply)
    //
    // implicit val encoderSavedView: Encoder[SavedView] =
    //   Encoder.forProduct3("id", "name", "view")(a => (a.id, a.name, a.view))
    //
    // implicit val codecSavedViewsND: JsonCodec[SavedViews.NonDefault] =
    //   codecIMap(SavedViews.emptyNonDefault)
    //
    // implicit val decoderSavedViews: Decoder[SavedViews.NonEmpty] =
    //   Decoder.forProduct2("default", "nonDefault")(SavedViews.NonEmpty.apply)
    //
    // implicit val encoderSavedViews: Encoder[SavedViews.NonEmpty] =
    //   Encoder.forProduct2("default", "nonDefault")(a => (a.default, a.nonDefault))
  }

  // Note: This has been designed to be identical to ISubset[ReqTypeId] which is what it's meant to replace.
  // implicit lazy val codecApplicableReqTypes: JsonCodec[ApplicableReqTypes] = {
  //   val unit = ().asJson
  //
  //   implicit val encoder: Encoder[ApplicableReqTypes] =
  //     Encoder.instance { a =>
  //       if (a.isEmpty)
  //         Json.obj("all" -> unit)
  //       else {
  //         val key = if (a.applicability is Applicable) "only" else "not"
  //         Json.obj(key -> a.reqTypes.asJson)
  //       }
  //     }
  //
  //   implicit val decoder: Decoder[ApplicableReqTypes] =
  //     decodeSumBySoleKey {
  //       case ("all" , _) => Right(ApplicableReqTypes.empty)
  //       case ("only", c) => c.as[Set[ReqTypeId]].map(ApplicableReqTypes(Applicable, _))
  //       case ("not" , c) => c.as[Set[ReqTypeId]].map(ApplicableReqTypes(NotApplicable, _))
  //     }
  //
  //   JsonCodec.summon
  // }

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

  // Replaced by v1.1
  // implicit lazy val decoderCustomReqType: Decoder[CustomReqType] =
  //   Decoder.forProduct6("id", "mnemonic", "oldMnemonics", "name", "imp", "live")(CustomReqType.apply)
  //
  // implicit lazy val encoderCustomReqType: Encoder[CustomReqType] =
  //   Encoder.forProduct6("id", "mnemonic", "oldMnemonics", "name", "imp", "live")(a => (a.id, a.mnemonic, a.oldMnemonics, a.name, a.implication, a.live))

  implicit lazy val codecCustomReqTypeId: JsonCodec[CustomReqTypeId] =
    codecTaggedI(CustomReqTypeId)

  // Replaced by v1.1
  // implicit lazy val decoderFieldId: Decoder[FieldId] = decodeSumBySoleKeyOrConst[FieldId](
  //   "stepsNA"   -> StaticField.NormalAltStepTree,
  //   "stepsE"    -> StaticField.ExceptionStepTree,
  //   "impGraph"  -> StaticField.ImplicationGraph,
  //   "stepGraph" -> StaticField.StepGraph,
  // ) {
  //   case ("imp" , c) => c.as[CustomField.Implication.Id]
  //   case ("tag" , c) => c.as[CustomField.Tag.Id]
  //   case ("text", c) => c.as[CustomField.Text.Id]
  // }
  //
  // implicit lazy val encoderFieldId: Encoder[FieldId] = Encoder.instance {
  //   case StaticField.NormalAltStepTree => Json.fromString("stepsNA")
  //   case StaticField.ExceptionStepTree => Json.fromString("stepsE")
  //   case StaticField.ImplicationGraph  => Json.fromString("impGraph")
  //   case StaticField.StepGraph         => Json.fromString("stepGraph")
  //   case a: CustomField.Implication.Id => Json.obj("imp"  -> a.asJson)
  //   case a: CustomField.Tag.Id         => Json.obj("tag"  -> a.asJson)
  //   case a: CustomField.Text.Id        => Json.obj("text" -> a.asJson)
  // }

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

  // Replaced by v1.1
  // implicit lazy val decoderReqTypeId: Decoder[ReqTypeId] = decodeSumBySoleKeyOrConst[ReqTypeId](
  //   "uc" -> StaticReqType.UseCase,
  // ) {
  //   case ("c", c) => c.as[CustomReqTypeId]
  // }
  //
  // implicit lazy val encoderReqTypeId: Encoder[ReqTypeId] = Encoder.instance {
  //   case a: CustomReqTypeId       => Json.obj("c"  -> a.asJson)
  //   case _: StaticReqType.UseCase => Json.fromString("uc")
  // }

  implicit lazy val codecReqTypeMnemonic: JsonCodec[ReqType.Mnemonic] =
    codecTaggedS(ReqType.Mnemonic)

  // Replaced by v1.1
  // implicit lazy val codecReqTypes: JsonCodec[ReqTypes] =
  //   JsonCodec.xmap(ReqTypes.apply)(_.custom)
  //
  //  implicit lazy val codecReqTypesCustom: JsonCodec[ReqTypes.Custom] =
  //    codecIMapD
  //
  // implicit lazy val codecStaticField: JsonCodec[StaticField] =
  //   JsonCodec.enumAdt(AdtMacros.adtIsoSet[StaticField, String] {
  //     case StaticField.ImplicationGraph  => "impGraph"
  //     case StaticField.NormalAltStepTree => "stepsNA"
  //     case StaticField.ExceptionStepTree => "stepsE"
  //     case StaticField.StepGraph         => "stepGraph"
  //   })
  //
  // implicit lazy val codecStaticFieldOptional: JsonCodec[StaticField.Optional] =
  //   codecStaticField.narrow

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

  // ===================================================================================================================
  // Replaced by v1.1
  //
  // private[this] object FilterAstKeys {
  //   final val KeyAstAllOf          = "all"
  //   final val KeyAstAnyOf          = "any"
  //   final val KeyAstHasIssue       = "issue"
  //   final val KeyAstHashRef        = "hash"
  //   final val KeyAstImpliedByAnyOf = "impBy"
  //   final val KeyAstImpliesAnyOf   = "imp"
  //   final val KeyAstNot            = "not"
  //   final val KeyAstPresence       = "has"
  //   final val KeyAstRegex          = "regex"
  //   final val KeyAstReqType        = "reqType"
  //   final val KeyAstReqs           = "reqs"
  //   final val KeyAstText           = "text"
  // }
  //
  // implicit lazy val codecValidFilter: JsonCodec[Filter.Valid] = {
  //   import shipreq.webapp.base.filter.{IntensionalReqSet, FilterAst}
  //   import Filter._
  //   import Filter.Implicits._
  //   import FilterAstKeys._
  //
  //   implicit val codecNonEmptySetInt: JsonCodec[NonEmptySet[Int]] =
  //     codecNES
  //
  //   implicit def decoderIRSetWhole[RT: Decoder]: Decoder[IntensionalReqSet.WholeType[RT]] =
  //     Decoder[RT].map(IntensionalReqSet.WholeType.apply[RT])
  //
  //   implicit def encoderIRSetWhole[RT: Encoder]: Encoder[IntensionalReqSet.WholeType[RT]] =
  //     Encoder[RT].contramap(_.reqType)
  //
  //   implicit def decoderIRSetSome[RT: Decoder]: Decoder[IntensionalReqSet.SomeOfType[RT]] =
  //     Decoder.forProduct2("reqType", "numbers")(IntensionalReqSet.SomeOfType.apply[RT])
  //
  //   implicit def encoderIRSetSome[RT: Encoder]: Encoder[IntensionalReqSet.SomeOfType[RT]] =
  //     Encoder.forProduct2("reqType", "numbers")(a => (a.reqType, a.numbers))
  //
  //   def decoderIRSet[RT](implicit d1: Decoder[IntensionalReqSet.SomeOfType[RT]], d2: Decoder[IntensionalReqSet.WholeType[RT]]): Decoder[IntensionalReqSet[RT]] = decodeSumBySoleKey {
  //     case ("some" , c) => c.as[IntensionalReqSet.SomeOfType[RT]]
  //     case ("whole", c) => c.as[IntensionalReqSet.WholeType[RT]]
  //   }
  //
  //   def encoderIRSet[RT](implicit e1: Encoder[IntensionalReqSet.SomeOfType[RT]], e2: Encoder[IntensionalReqSet.WholeType[RT]]): Encoder[IntensionalReqSet[RT]] = Encoder.instance {
  //     case a: IntensionalReqSet.SomeOfType[RT] => Json.obj("some"  -> a.asJson)
  //     case a: IntensionalReqSet.WholeType[RT]  => Json.obj("whole" -> a.asJson)
  //   }
  //
  //   implicit lazy val codecValidHashTag: JsonCodec[Valid.HashTag] =
  //     codecDisj[CustomIssueTypeId, ApplicableTagId]
  //
  //   implicit val codecValidIssueCatNEV: JsonCodec[NonEmptyVector[Valid.IssueCat]] =
  //     codecNEV
  //
  //   implicit val codecValidReqSubset: JsonCodec[Valid.ReqSubset] =
  //     JsonCodec(encoderIRSet, decoderIRSet)
  //
  //   implicit val codecValidReqSet: JsonCodec[Valid.ReqSet] =
  //     codecNEV
  //
  //   implicit lazy val codecFilterAstAttr: JsonCodec[FilterAst.Attr] =
  //     JsonCodec.enumAdt(AdtMacros.adtIsoSet[FilterAst.Attr, String] {
  //       case FilterAst.Attr.AnyIssue => "issue"
  //       case FilterAst.Attr.AnyTag   => "tag"
  //     })
  //
  //   implicit val decoderFilterAstText: Decoder[FilterAst.Text] =
  //     Decoder.forProduct2("text", "quote")(FilterAst.Text.apply)
  //
  //   implicit val encoderFilterAstText: Encoder[FilterAst.Text] =
  //     Encoder.forProduct2("text", "quote")(a => (a.text, a.quoteChar))
  //
  //   implicit val codecFilterAstRegex: JsonCodec[FilterAst.Regex] =
  //     JsonCodec.xmap(FilterAst.Regex.apply)(_.text)
  //
  //   implicit val codecFilterAstPresence: JsonCodec[FilterAst.Presence[Valid.Attr]] =
  //     JsonCodec.xmap(FilterAst.Presence.apply[Valid.Attr])(_.attr)
  //
  //   implicit val decoderFilterAstHasIssue: Decoder[FilterAst.HasIssue[Valid.IssueCat]] =
  //     Decoder.forProduct2("on", "criteria")(FilterAst.HasIssue.apply)
  //
  //   implicit val encoderFilterAstHasIssue: Encoder[FilterAst.HasIssue[Valid.IssueCat]] =
  //     Encoder.forProduct2("on", "criteria")(a => (a.on, a.criteria))
  //
  //   implicit val decoderFilterAstRegex: Decoder[FilterAst.Regex] =
  //     Decoder[String].map(FilterAst.Regex.apply)
  //
  //   implicit val encoderFilterAstRegex: Encoder[FilterAst.Regex] =
  //     Encoder[String].contramap(_.text)
  //
  //   implicit val decoderFilterAstHashRef: Decoder[FilterAst.HashRef[Valid.HashTag]] =
  //     Decoder[Valid.HashTag].map(FilterAst.HashRef.apply)
  //
  //   implicit val encoderFilterAstHashRef: Encoder[FilterAst.HashRef[Valid.HashTag]] =
  //     Encoder[Valid.HashTag].contramap(_.value)
  //
  //   implicit val decoderFilterAstImpliesAnyOf: Decoder[FilterAst.ImpliesAnyOf[Valid.ReqSet]] =
  //     Decoder[Valid.ReqSet].map(FilterAst.ImpliesAnyOf.apply)
  //
  //   implicit val encoderFilterAstImpliesAnyOf: Encoder[FilterAst.ImpliesAnyOf[Valid.ReqSet]] =
  //     Encoder[Valid.ReqSet].contramap(_.reqs)
  //
  //   implicit val decoderFilterAstImpliedByAnyOf: Decoder[FilterAst.ImpliedByAnyOf[Valid.ReqSet]] =
  //     Decoder[Valid.ReqSet].map(FilterAst.ImpliedByAnyOf.apply)
  //
  //   implicit val encoderFilterAstImpliedByAnyOf: Encoder[FilterAst.ImpliedByAnyOf[Valid.ReqSet]] =
  //     Encoder[Valid.ReqSet].contramap(_.reqs)
  //
  //   implicit val decoderFilterAstReqs: Decoder[FilterAst.Reqs[Valid.ReqSet]] =
  //     Decoder[Valid.ReqSet].map(FilterAst.Reqs.apply)
  //
  //   implicit val encoderFilterAstReqs: Encoder[FilterAst.Reqs[Valid.ReqSet]] =
  //     Encoder[Valid.ReqSet].contramap(_.reqs)
  //
  //   implicit val decoderFilterAstReqType: Decoder[FilterAst.ReqType[Valid.ReqType]] =
  //     Decoder[Valid.ReqType].map(FilterAst.ReqType.apply)
  //
  //   implicit val encoderFilterAstReqType: Encoder[FilterAst.ReqType[Valid.ReqType]] =
  //     Encoder[Valid.ReqType].contramap(_.reqType)
  //
  //   JsonCodec.fix[ValidF]({
  //     case a: FilterAst.Text                           => Json.obj(KeyAstText           -> a.asJson)
  //     case a: FilterAst.Regex                          => Json.obj(KeyAstRegex          -> a.asJson)
  //     case a: FilterAst.Presence      [Valid.Attr]     => Json.obj(KeyAstPresence       -> a.asJson)
  //     case a: FilterAst.HasIssue      [Valid.IssueCat] => Json.obj(KeyAstHasIssue       -> a.asJson)
  //     case a: FilterAst.HashRef       [Valid.HashTag]  => Json.obj(KeyAstHashRef        -> a.asJson)
  //     case a: FilterAst.ImpliesAnyOf  [Valid.ReqSet]   => Json.obj(KeyAstImpliesAnyOf   -> a.asJson)
  //     case a: FilterAst.ImpliedByAnyOf[Valid.ReqSet]   => Json.obj(KeyAstImpliedByAnyOf -> a.asJson)
  //     case a: FilterAst.Reqs          [Valid.ReqSet]   => Json.obj(KeyAstReqs           -> a.asJson)
  //     case a: FilterAst.ReqType       [Valid.ReqType]  => Json.obj(KeyAstReqType        -> a.asJson)
  //     case FilterAst.Not              (clause)         => Json.obj(KeyAstNot            -> clause)
  //     case FilterAst.AllOf            (clauses)        => Json.obj(KeyAstAllOf          -> Json.arr(clauses.whole: _*))
  //     case FilterAst.AnyOf            (head, tail)     => Json.obj(KeyAstAnyOf          -> Json.arr(head +: tail.whole: _*))
  //   }, decoderFnSumBySoleKey {
  //     case (KeyAstText          , c) => c.as[FilterAst.Text]
  //     case (KeyAstRegex         , c) => c.as[FilterAst.Regex]
  //     case (KeyAstPresence      , c) => c.as[FilterAst.Presence      [Valid.Attr]]
  //     case (KeyAstHasIssue      , c) => c.as[FilterAst.HasIssue      [Valid.IssueCat]]
  //     case (KeyAstHashRef       , c) => c.as[FilterAst.HashRef       [Valid.HashTag]]
  //     case (KeyAstImpliesAnyOf  , c) => c.as[FilterAst.ImpliesAnyOf  [Valid.ReqSet]]
  //     case (KeyAstImpliedByAnyOf, c) => c.as[FilterAst.ImpliedByAnyOf[Valid.ReqSet]]
  //     case (KeyAstReqs          , c) => c.as[FilterAst.Reqs          [Valid.ReqSet]]
  //     case (KeyAstReqType       , c) => c.as[FilterAst.ReqType       [Valid.ReqType]]
  //     case (KeyAstNot           , c) => Right(FilterAst.Not(c))
  //
  //     case (KeyAstAllOf, c) =>
  //       val c1 = c.downArray
  //       val cn = Iterator.iterate(c1)(_.right).takeWhile(_.succeeded).toVector
  //       Right(FilterAst.AllOf(NonEmptyVector(c1, cn)))
  //
  //     case (KeyAstAnyOf, c) =>
  //       val c1 = c.downArray
  //       val c2 = c1.right
  //       val cn = Iterator.iterate(c2)(_.right).takeWhile(_.succeeded).toVector
  //       Right(FilterAst.AnyOf(c1, NonEmptyVector(c2, cn)))
  //   })
  // }

}
