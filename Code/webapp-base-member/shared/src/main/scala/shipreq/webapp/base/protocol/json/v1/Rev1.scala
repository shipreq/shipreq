package shipreq.webapp.base.protocol.json.v1

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.microlibs.stdlib_ext.ParseInt
import scalaz.{-\/, \/-}
import shipreq.base.util.JsonUtil._
import shipreq.base.util._
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.RetiredGenericData._
import shipreq.webapp.base.event._
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.protocol.json.JsonCodec

/** v1.1 */
object Rev1 {
  import JsonCodec.Implicits._
  import BaseData._
  import BaseMemberData1._
  import Events._
  import Events.EventData._
  import PostEvents._

  // ===================================================================================================================

  import BaseMemberData1.SavedViewCodecs._

  object SavedViewCodecs {
    import shipreq.webapp.base.data.savedview._

    private[this] final val KeyCustomField    = "custom"
    private[this] final val KeyImplications   = "imps"
    private[this] final val KeyCode           = "code"
    private[this] final val KeyDeletionReason = "delReason"
    private[this] final val KeyPubid          = "pubid"
    private[this] final val KeyReqType        = "reqType"
    private[this] final val KeyTags           = "tags"
    private[this] final val KeyOtherTags      = "otherTags"
    private[this] final val KeyAllTags        = "allTags"
    private[this] final val KeyTitle          = "title"

    implicit val decoderColumn: Decoder[Column] = decodeSumBySoleKeyOrConst[Column](
      KeyCode           -> Column.Code,
      KeyDeletionReason -> Column.DeletionReason,
      KeyPubid          -> Column.Pubid,
      KeyReqType        -> Column.ReqType,
      KeyTags           -> Column.OtherTags,
      KeyOtherTags      -> Column.OtherTags,
      KeyAllTags        -> Column.AllTags,
      KeyTitle          -> Column.Title,
    ) {
      case (KeyCustomField   , c) => c.as[Column.CustomField]
      case (KeyImplications  , c) => c.as[Column.Implications]
    }

    implicit val encoderColumn: Encoder[Column] = Encoder.instance {
      case Column.Code            => Json.fromString(KeyCode)
      case Column.DeletionReason  => Json.fromString(KeyDeletionReason)
      case Column.Pubid           => Json.fromString(KeyPubid)
      case Column.ReqType         => Json.fromString(KeyReqType)
      case Column.OtherTags       => Json.fromString(KeyOtherTags)
      case Column.AllTags         => Json.fromString(KeyAllTags)
      case Column.Title           => Json.fromString(KeyTitle)
      case a: Column.CustomField  => Json.obj(KeyCustomField  -> a.asJson)
      case a: Column.Implications => Json.obj(KeyImplications -> a.asJson)
    }

    implicit val decoderColumnSortInconclusive: Decoder[Column.SortInconclusive] =
      decodeSumBySoleKeyOrConst[Column.SortInconclusive](
        KeyCode           -> Column.Code,
        KeyDeletionReason -> Column.DeletionReason,
        KeyReqType        -> Column.ReqType,
        KeyTags           -> Column.OtherTags,
        KeyOtherTags      -> Column.OtherTags,
        KeyAllTags        -> Column.AllTags,
        KeyTitle          -> Column.Title,
      ) {
        case (KeyCustomField , c) => c.as[Column.CustomField]
        case (KeyImplications, c) => c.as[Column.Implications]
      }

    implicit val encoderColumnSortInconclusive: Encoder[Column.SortInconclusive] = Encoder.instance {
      case Column.Code            => Json.fromString(KeyCode)
      case Column.DeletionReason  => Json.fromString(KeyDeletionReason)
      case Column.ReqType         => Json.fromString(KeyReqType)
      case Column.OtherTags       => Json.fromString(KeyOtherTags)
      case Column.AllTags         => Json.fromString(KeyAllTags)
      case Column.Title           => Json.fromString(KeyTitle)
      case a: Column.CustomField  => Json.obj(KeyCustomField  -> a.asJson)
      case a: Column.Implications => Json.obj(KeyImplications -> a.asJson)
    }

    implicit val decoderColumnSortInconclusiveHasBlanks: Decoder[Column.SortInconclusiveHasBlanks] =
      decodeSumBySoleKeyOrConst[Column.SortInconclusiveHasBlanks](
        KeyCode           -> Column.Code,
        KeyDeletionReason -> Column.DeletionReason,
        KeyTags           -> Column.OtherTags,
        KeyOtherTags      -> Column.OtherTags,
        KeyAllTags        -> Column.AllTags,
        KeyTitle          -> Column.Title,
      ) {
      case (KeyCustomField , c) => c.as[Column.CustomField]
      case (KeyImplications, c) => c.as[Column.Implications]
    }

    implicit val encoderColumnSortInconclusiveHasBlanks: Encoder[Column.SortInconclusiveHasBlanks] = Encoder.instance {
      case Column.Code            => Json.fromString(KeyCode)
      case Column.DeletionReason  => Json.fromString(KeyDeletionReason)
      case Column.OtherTags       => Json.fromString(KeyOtherTags)
      case Column.AllTags         => Json.fromString(KeyAllTags)
      case Column.Title           => Json.fromString(KeyTitle)
      case a: Column.CustomField  => Json.obj(KeyCustomField  -> a.asJson)
      case a: Column.Implications => Json.obj(KeyImplications -> a.asJson)
    }

    implicit val codecColumnSortInconclusiveNoBlanks: JsonCodec[Column.SortInconclusiveNoBlanks] =
      JsonCodec.enumAdt(AdtMacros.adtIsoSet[Column.SortInconclusiveNoBlanks, String] {
        case Column.ReqType => KeyReqType
      })

    implicit val codecColumnNEV: JsonCodec[NonEmptyVector[Column]] =
      codecNEV

    implicit val decoderSortCriterionInconclusiveCB: Decoder[SortCriterion.InconclusiveCB] =
      Decoder.forProduct2("column", "method")(SortCriterion.InconclusiveCB.apply)

    implicit val encoderSortCriterionInconclusiveCB: Encoder[SortCriterion.InconclusiveCB] =
      Encoder.forProduct2("column", "method")(a => (a.column, a.method))

    implicit val decoderSortCriterionInconclusiveIB: Decoder[SortCriterion.InconclusiveIB] =
      Decoder.forProduct2("column", "method")(SortCriterion.InconclusiveIB.apply)

    implicit val encoderSortCriterionInconclusiveIB: Encoder[SortCriterion.InconclusiveIB] =
      Encoder.forProduct2("column", "method")(a => (a.column, a.method))

    implicit val decoderSortCriterionI: Decoder[SortCriterion.Inconclusive] = decodeSumBySoleKey {
      case ("CB", c) => c.as[SortCriterion.InconclusiveCB]
      case ("IB", c) => c.as[SortCriterion.InconclusiveIB]
    }

    implicit val encoderSortCriterionI: Encoder[SortCriterion.Inconclusive] = Encoder.instance {
      case a: SortCriterion.InconclusiveCB => Json.obj("CB" -> a.asJson)
      case a: SortCriterion.InconclusiveIB => Json.obj("IB" -> a.asJson)
    }

    implicit val decoderSortCriterion: Decoder[SortCriterion] = decodeSumBySoleKey {
      case ("c" , c) => c.as[SortCriterion.Conclusive]
      case ("CB", c) => c.as[SortCriterion.InconclusiveCB]
      case ("IB", c) => c.as[SortCriterion.InconclusiveIB]
    }

    implicit val encoderSortCriterion: Encoder[SortCriterion] = Encoder.instance {
      case a: SortCriterion.Conclusive     => Json.obj("c"  -> a.asJson)
      case a: SortCriterion.InconclusiveCB => Json.obj("CB" -> a.asJson)
      case a: SortCriterion.InconclusiveIB => Json.obj("IB" -> a.asJson)
    }

    implicit val decoderSortCriteria: Decoder[SortCriteria] =
      Decoder.forProduct2("init", "last")(SortCriteria.apply)

    implicit val encoderSortCriteria: Encoder[SortCriteria] =
      Encoder.forProduct2("init", "last")(a => (a.init, a.last))

    implicit val decoderImpGraphConfigGraphDir: Decoder[ImpGraphConfig.GraphDir] = decodeSumBySoleKey {
      case ("bottomToTop", _) => Right(ImpGraphConfig.GraphDir.BottomToTop)
      case ("leftToRight", _) => Right(ImpGraphConfig.GraphDir.LeftToRight)
      case ("rightToLeft", _) => Right(ImpGraphConfig.GraphDir.RightToLeft)
      case ("topToBottom", _) => Right(ImpGraphConfig.GraphDir.TopToBottom)
    }

    implicit val encoderImpGraphConfigGraphDir: Encoder[ImpGraphConfig.GraphDir] = Encoder.instance {
      case ImpGraphConfig.GraphDir.BottomToTop => Json.obj("bottomToTop" -> ().asJson)
      case ImpGraphConfig.GraphDir.LeftToRight => Json.obj("leftToRight" -> ().asJson)
      case ImpGraphConfig.GraphDir.RightToLeft => Json.obj("rightToLeft" -> ().asJson)
      case ImpGraphConfig.GraphDir.TopToBottom => Json.obj("topToBottom" -> ().asJson)
    }

    implicit val decoderImpGraphConfigColoursByTag: Decoder[ImpGraphConfig.Colours.ByTag] =
      Decoder.forProduct1("tagGroupId")(ImpGraphConfig.Colours.ByTag.apply)

    implicit val encoderImpGraphConfigColoursByTag: Encoder[ImpGraphConfig.Colours.ByTag] =
      Encoder.forProduct1("tagGroupId")(_.tagGroupId)

    implicit val decoderImpGraphConfigColours: Decoder[ImpGraphConfig.Colours] = decodeSumBySoleKey {
      case ("byReqType", _) => Right(ImpGraphConfig.Colours.ByReqType)
      case ("byTag"    , c) => c.as[ImpGraphConfig.Colours.ByTag]
    }

    implicit val encoderImpGraphConfigColours: Encoder[ImpGraphConfig.Colours] = Encoder.instance {
      case ImpGraphConfig.Colours.ByReqType => Json.obj("byReqType" -> ().asJson)
      case a: ImpGraphConfig.Colours.ByTag  => Json.obj("byTag"     -> a.asJson)
    }

    implicit val decoderImpGraphConfigLabelFormat: Decoder[ImpGraphConfig.LabelFormat] = decodeSumBySoleKey {
      case ("id"        , _) => Right(ImpGraphConfig.LabelFormat.Pubid)
      case ("idAndTitle", _) => Right(ImpGraphConfig.LabelFormat.PubidAndTitle)
    }

    implicit val encoderImpGraphConfigLabelFormat: Encoder[ImpGraphConfig.LabelFormat] = Encoder.instance {
      case ImpGraphConfig.LabelFormat.Pubid         => Json.obj("id"         -> ().asJson)
      case ImpGraphConfig.LabelFormat.PubidAndTitle => Json.obj("idAndTitle" -> ().asJson)
    }

    implicit val decoderImpGraphConfigImpGraphConfig: Decoder[ImpGraphConfig] =
      Decoder.forProduct3("graphDir", "labelFormat", "colours")(ImpGraphConfig.apply)

    implicit val encoderImpGraphConfig: Encoder[ImpGraphConfig] =
      Encoder.forProduct3("graphDir", "labelFormat", "colours")(a => (a.graphDir, a.labelFormat, a.colours))

    implicit val decoderView: Decoder[View] =
      Decoder.instance { c =>
        for {
          columns        <- c.get[NonEmptyVector[Column]]("columns")
          order          <- c.get[SortCriteria          ]("order")
          filterDead     <- c.get[FilterDead            ]("filterDead")
          filter         <- c.get[Option[Filter.Valid]  ]("filter")
          impGraphConfig <- c.get[Option[ImpGraphConfig]]("impGraphConfig")
        } yield View(columns, order, filterDead, filter, impGraphConfig)
      }

    implicit val encoderView: Encoder[View] =
      Encoder.instance(value => Json.obj(
        "columns"        -> value.columns       .asJson,
        "order"          -> value.order         .asJson,
        "filterDead"     -> value.filterDead    .asJson,
        "filter"         -> value.filter        .asJson,
        "impGraphConfig" -> value.impGraphConfig.asJson,
      ).dropNullValues)

    implicit val decoderSavedView: Decoder[SavedView] =
      Decoder.forProduct3("id", "name", "view")(SavedView.apply)

    implicit val encoderSavedView: Encoder[SavedView] =
      Encoder.forProduct3("id", "name", "view")(a => (a.id, a.name, a.view))

    implicit val codecSavedViewsND: JsonCodec[SavedViews.NonDefault] =
      codecIMap(SavedViews.emptyNonDefault)

    implicit val decoderSavedViews: Decoder[SavedViews.NonEmpty] =
      Decoder.forProduct2("default", "nonDefault")(SavedViews.NonEmpty.apply)

    implicit val encoderSavedViews: Encoder[SavedViews.NonEmpty] =
      Encoder.forProduct2("default", "nonDefault")(a => (a.default, a.nonDefault))
  }

  import SavedViewCodecs._

  // ===================================================================================================================

  private[this] object FilterAstKeys {
    final val KeyAstAllOf          = "all"
    final val KeyAstAnyOf          = "any"
    final val KeyAstHasIssue       = "issue"
    final val KeyAstHashRef        = "hash"
    final val KeyAstFieldProp      = "field"
    final val KeyAstImpliedByAnyOf = "impBy"
    final val KeyAstImpliesAnyOf   = "imp"
    final val KeyAstNot            = "not"
    final val KeyAstPresence       = "has"
    final val KeyAstRegex          = "regex"
    final val KeyAstReqType        = "reqType"
    final val KeyAstReqs           = "reqs"
    final val KeyAstText           = "text"
  }

  implicit lazy val codecValidFilter: JsonCodec[Filter.Valid] = {
    import shipreq.webapp.base.filter.{IntensionalReqSet, FilterAst}
    import Filter._
    import Filter.Implicits._
    import FilterAstKeys._

    implicit val codecNonEmptySetInt: JsonCodec[NonEmptySet[Int]] =
      codecNES

    implicit def decoderIRSetWhole[RT: Decoder]: Decoder[IntensionalReqSet.WholeType[RT]] =
      Decoder[RT].map(IntensionalReqSet.WholeType.apply[RT])

    implicit def encoderIRSetWhole[RT: Encoder]: Encoder[IntensionalReqSet.WholeType[RT]] =
      Encoder[RT].contramap(_.reqType)

    implicit def decoderIRSetSome[RT: Decoder]: Decoder[IntensionalReqSet.SomeOfType[RT]] =
      Decoder.forProduct2("reqType", "numbers")(IntensionalReqSet.SomeOfType.apply[RT])

    implicit def encoderIRSetSome[RT: Encoder]: Encoder[IntensionalReqSet.SomeOfType[RT]] =
      Encoder.forProduct2("reqType", "numbers")(a => (a.reqType, a.numbers))

    def decoderIRSet[RT](implicit d1: Decoder[IntensionalReqSet.SomeOfType[RT]], d2: Decoder[IntensionalReqSet.WholeType[RT]]): Decoder[IntensionalReqSet[RT]] = decodeSumBySoleKey {
      case ("some" , c) => c.as[IntensionalReqSet.SomeOfType[RT]]
      case ("whole", c) => c.as[IntensionalReqSet.WholeType[RT]]
    }

    def encoderIRSet[RT](implicit e1: Encoder[IntensionalReqSet.SomeOfType[RT]], e2: Encoder[IntensionalReqSet.WholeType[RT]]): Encoder[IntensionalReqSet[RT]] = Encoder.instance {
      case a: IntensionalReqSet.SomeOfType[RT] => Json.obj("some"  -> a.asJson)
      case a: IntensionalReqSet.WholeType[RT]  => Json.obj("whole" -> a.asJson)
    }

    implicit lazy val codecValidHashTag: JsonCodec[Valid.HashTag] =
      codecDisj[CustomIssueTypeId, ApplicableTagId]

    implicit lazy val codecValidField: JsonCodec[Valid.Field] = {
      val encoder =
        Encoder.instance[Valid.Field] {
          case \/-(f)                         => f.asJson
          case -\/(SpecialBuiltInField.Title) => Json.fromString("title")
        }

      val decFieldId = decoderFieldId.map[Valid.Field](\/-(_))

      val decBuiltIn = Decoder[String].emap[Valid.Field] {
        case "title" => Right(-\/(SpecialBuiltInField.Title))
        case x       => Left("Unknown field: " + x)
      }

      JsonCodec(encoder, decFieldId or decBuiltIn)
    }

    implicit val codecValidIssueCatNEV: JsonCodec[NonEmptyVector[Valid.IssueCat]] =
      codecNEV

    implicit val codecValidReqSubset: JsonCodec[Valid.ReqSubset] =
      JsonCodec(encoderIRSet, decoderIRSet)

    implicit val codecValidReqSet: JsonCodec[Valid.ReqSet] =
      codecNEV

    implicit lazy val codecFilterAstAttr: JsonCodec[FilterAst.Attr] =
      JsonCodec.enumAdt(AdtMacros.adtIsoSet[FilterAst.Attr, String] {
        case FilterAst.Attr.AnyIssue => "issue"
        case FilterAst.Attr.AnyTag   => "tag"
      })

    implicit lazy val codecFilterAstFieldAttr: JsonCodec[FilterAst.FieldAttr] =
      JsonCodec.enumAdt(AdtMacros.adtIsoSet[FilterAst.FieldAttr, String] {
        case FilterAst.FieldAttr.Blank         => "blank"
        case FilterAst.FieldAttr.NotApplicable => "n/a"
        case FilterAst.FieldAttr.DefaultInUse  => "default"
      })

    implicit val decoderFilterAstText: Decoder[FilterAst.Text] =
      Decoder.forProduct2("text", "quote")(FilterAst.Text.apply)

    implicit val encoderFilterAstText: Encoder[FilterAst.Text] =
      Encoder.forProduct2("text", "quote")(a => (a.text, a.quoteChar))

    implicit val codecFilterAstRegex: JsonCodec[FilterAst.Regex] =
      JsonCodec.xmap(FilterAst.Regex.apply)(_.text)

    implicit val codecFilterAstPresence: JsonCodec[FilterAst.Presence[Valid.Attr]] =
      JsonCodec.xmap(FilterAst.Presence.apply[Valid.Attr])(_.attr)

    implicit val decoderFilterAstHasIssue: Decoder[FilterAst.HasIssue[Valid.IssueCat]] =
      Decoder.forProduct2("on", "criteria")(FilterAst.HasIssue.apply)

    implicit val encoderFilterAstHasIssue: Encoder[FilterAst.HasIssue[Valid.IssueCat]] =
      Encoder.forProduct2("on", "criteria")(a => (a.on, a.criteria))

    implicit val decoderFilterAstFieldProp: Decoder[FilterAst.FieldProp[Valid.Field, Valid.FieldAttr]] =
      Decoder.forProduct2("field", "attr")(FilterAst.FieldProp.apply)

    implicit val encoderFilterAstFieldProp: Encoder[FilterAst.FieldProp[Valid.Field, Valid.FieldAttr]] =
      Encoder.forProduct2("field", "attr")(a => (a.field, a.attr))

    implicit val decoderFilterAstHashRef: Decoder[FilterAst.HashRef[Valid.HashTag]] =
      Decoder[Valid.HashTag].map(FilterAst.HashRef.apply)

    implicit val encoderFilterAstHashRef: Encoder[FilterAst.HashRef[Valid.HashTag]] =
      Encoder[Valid.HashTag].contramap(_.value)

    implicit val decoderFilterAstImpliesAnyOf: Decoder[FilterAst.ImpliesAnyOf[Valid.ReqSet]] =
      Decoder[Valid.ReqSet].map(FilterAst.ImpliesAnyOf.apply)

    implicit val encoderFilterAstImpliesAnyOf: Encoder[FilterAst.ImpliesAnyOf[Valid.ReqSet]] =
      Encoder[Valid.ReqSet].contramap(_.reqs)

    implicit val decoderFilterAstImpliedByAnyOf: Decoder[FilterAst.ImpliedByAnyOf[Valid.ReqSet]] =
      Decoder[Valid.ReqSet].map(FilterAst.ImpliedByAnyOf.apply)

    implicit val encoderFilterAstImpliedByAnyOf: Encoder[FilterAst.ImpliedByAnyOf[Valid.ReqSet]] =
      Encoder[Valid.ReqSet].contramap(_.reqs)

    implicit val decoderFilterAstReqs: Decoder[FilterAst.Reqs[Valid.ReqSet]] =
      Decoder[Valid.ReqSet].map(FilterAst.Reqs.apply)

    implicit val encoderFilterAstReqs: Encoder[FilterAst.Reqs[Valid.ReqSet]] =
      Encoder[Valid.ReqSet].contramap(_.reqs)

    implicit val decoderFilterAstReqType: Decoder[FilterAst.ReqType[Valid.ReqType]] =
      Decoder[Valid.ReqType].map(FilterAst.ReqType.apply)

    implicit val encoderFilterAstReqType: Encoder[FilterAst.ReqType[Valid.ReqType]] =
      Encoder[Valid.ReqType].contramap(_.reqType)

    JsonCodec.fix[ValidF]({
      case a: FilterAst.Text                                         => Json.obj(KeyAstText           -> a.asJson)
      case a: FilterAst.Regex                                        => Json.obj(KeyAstRegex          -> a.asJson)
      case a: FilterAst.Presence      [Valid.Attr]                   => Json.obj(KeyAstPresence       -> a.asJson)
      case a: FilterAst.FieldProp     [Valid.Field, Valid.FieldAttr] => Json.obj(KeyAstFieldProp      -> a.asJson)
      case a: FilterAst.HasIssue      [Valid.IssueCat]               => Json.obj(KeyAstHasIssue       -> a.asJson)
      case a: FilterAst.HashRef       [Valid.HashTag]                => Json.obj(KeyAstHashRef        -> a.asJson)
      case a: FilterAst.ImpliesAnyOf  [Valid.ReqSet]                 => Json.obj(KeyAstImpliesAnyOf   -> a.asJson)
      case a: FilterAst.ImpliedByAnyOf[Valid.ReqSet]                 => Json.obj(KeyAstImpliedByAnyOf -> a.asJson)
      case a: FilterAst.Reqs          [Valid.ReqSet]                 => Json.obj(KeyAstReqs           -> a.asJson)
      case a: FilterAst.ReqType       [Valid.ReqType]                => Json.obj(KeyAstReqType        -> a.asJson)
      case FilterAst.Not              (clause)                       => Json.obj(KeyAstNot            -> clause)
      case FilterAst.AllOf            (clauses)                      => Json.obj(KeyAstAllOf          -> Json.arr(clauses.whole: _*))
      case FilterAst.AnyOf            (head, tail)                   => Json.obj(KeyAstAnyOf          -> Json.arr(head +: tail.whole: _*))
    }, decoderFnSumBySoleKey {
      case (KeyAstText          , c) => c.as[FilterAst.Text]
      case (KeyAstRegex         , c) => c.as[FilterAst.Regex]
      case (KeyAstPresence      , c) => c.as[FilterAst.Presence      [Valid.Attr]]
      case (KeyAstFieldProp     , c) => c.as[FilterAst.FieldProp     [Valid.Field, Valid.FieldAttr]]
      case (KeyAstHasIssue      , c) => c.as[FilterAst.HasIssue      [Valid.IssueCat]]
      case (KeyAstHashRef       , c) => c.as[FilterAst.HashRef       [Valid.HashTag]]
      case (KeyAstImpliesAnyOf  , c) => c.as[FilterAst.ImpliesAnyOf  [Valid.ReqSet]]
      case (KeyAstImpliedByAnyOf, c) => c.as[FilterAst.ImpliedByAnyOf[Valid.ReqSet]]
      case (KeyAstReqs          , c) => c.as[FilterAst.Reqs          [Valid.ReqSet]]
      case (KeyAstReqType       , c) => c.as[FilterAst.ReqType       [Valid.ReqType]]
      case (KeyAstNot           , c) => Right(FilterAst.Not(c))

      case (KeyAstAllOf, c) =>
        val c1 = c.downArray
        val cn = Iterator.iterate(c1)(_.right).takeWhile(_.succeeded).toVector
        Right(FilterAst.AllOf(NonEmptyVector(c1, cn)))

      case (KeyAstAnyOf, c) =>
        val c1 = c.downArray
        val c2 = c1.right
        val cn = Iterator.iterate(c2)(_.right).takeWhile(_.succeeded).toVector
        Right(FilterAst.AnyOf(c1, NonEmptyVector(c2, cn)))
    })
  }

  // ===================================================================================================================

  implicit lazy val codecColour: JsonCodec[Colour] =
    JsonCodec.xmap(Colour.force)(_.value)

  implicit lazy val decoderReqTypeId: Decoder[ReqTypeId] = {
    val old = decoderFnSumBySoleKey { case ("c", c) => c.as[CustomReqTypeId] }
    decodeSumBySoleKeyOr[ReqTypeId](
      "uc" -> StaticReqType.UseCase,
    ) { c =>
      val o = old(c)
      if (o.isRight) o else c.as[Int].map(CustomReqTypeId.apply)
    }
  }

  implicit lazy val encoderReqTypeId: Encoder[ReqTypeId] = Encoder.instance {
    case a: CustomReqTypeId       => a.value.asJson
    case _: StaticReqType.UseCase => Json.fromString("uc")
  }

  implicit lazy val decoderCustomReqType: Decoder[CustomReqType] =
    Decoder.forProduct7("id", "mnemonic", "oldMnemonics", "name", "desc", "imp", "live")(CustomReqType.apply)

  implicit lazy val encoderCustomReqType: Encoder[CustomReqType] =
    Encoder.forProduct7("id", "mnemonic", "oldMnemonics", "name", "desc", "imp", "live")(a =>
      (a.id, a.mnemonic, a.oldMnemonics, a.name, a.description,  a.implication, a.live))

  implicit lazy val codecReqTypes: JsonCodec[ReqTypes] =
    JsonCodec.xmap(ReqTypes.apply)(_.custom)

  implicit lazy val codecReqTypesCustom: JsonCodec[ReqTypes.Custom] =
    codecIMapD[CustomReqTypeId, CustomReqType]

  implicit lazy val decoderFieldId: Decoder[FieldId] = decodeSumBySoleKeyOrConst[FieldId](
    "stepsNA"   -> StaticField.NormalAltStepTree,
    "stepsE"    -> StaticField.ExceptionStepTree,
    "impGraph"  -> StaticField.ImplicationGraph,
    "stepGraph" -> StaticField.StepGraph,
    "otherTags" -> StaticField.OtherTags,
    "allTags"   -> StaticField.AllTags,
  ) {
    case ("imp" , c) => c.as[CustomField.Implication.Id]
    case ("tag" , c) => c.as[CustomField.Tag.Id]
    case ("text", c) => c.as[CustomField.Text.Id]
  }

  implicit lazy val encoderFieldId: Encoder[FieldId] = Encoder.instance {
    case StaticField.NormalAltStepTree => Json.fromString("stepsNA")
    case StaticField.ExceptionStepTree => Json.fromString("stepsE")
    case StaticField.ImplicationGraph  => Json.fromString("impGraph")
    case StaticField.StepGraph         => Json.fromString("stepGraph")
    case StaticField.OtherTags         => Json.fromString("otherTags")
    case StaticField.AllTags           => Json.fromString("allTags")
    case a: CustomField.Implication.Id => Json.obj("imp"  -> a.asJson)
    case a: CustomField.Tag.Id         => Json.obj("tag"  -> a.asJson)
    case a: CustomField.Text.Id        => Json.obj("text" -> a.asJson)
  }

  implicit lazy val codecStaticField: JsonCodec[StaticField] =
    JsonCodec.enumAdt(AdtMacros.adtIsoSet[StaticField, String] {
      case StaticField.ImplicationGraph  => "impGraph"
      case StaticField.NormalAltStepTree => "stepsNA"
      case StaticField.ExceptionStepTree => "stepsE"
      case StaticField.StepGraph         => "stepGraph"
      case StaticField.OtherTags         => "otherTags"
      case StaticField.AllTags           => "allTags"
    })

  implicit lazy val codecStaticFieldOptional: JsonCodec[StaticField.Optional] =
    codecStaticField.narrow

  implicit lazy val codecApplicableReqTypes: JsonCodec[ApplicableReqTypes] = {
    val unit = ().asJson

    implicit val encoder: Encoder[ApplicableReqTypes] =
      Encoder.instance { a =>
        if (a.isEmpty)
          Json.obj("all" -> unit)
        else {
          val key = if (a.applicability is Applicable) "only" else "not"
          Json.obj(key -> a.reqTypes.asJson)
        }
      }

    implicit val decoder: Decoder[ApplicableReqTypes] =
      decodeSumBySoleKey {
        case ("all" , _) => Right(ApplicableReqTypes.empty)
        case ("only", c) => c.as[Set[ReqTypeId]].map(ApplicableReqTypes(Applicable, _))
        case ("not" , c) => c.as[Set[ReqTypeId]].map(ApplicableReqTypes(NotApplicable, _))
      }

    JsonCodec.summon
  }

  implicit lazy val encoderImpossible: Encoder[Impossible] =
    Encoder.instance(_ => null)

  implicit lazy val decoderImpossible: Decoder[Impossible] =
    Decoder.instance(c => Left(DecodingFailure("Impossible case", c.history)))

  implicit lazy val keyDecoderReqTypeId: KeyDecoder[ReqTypeId] =
    KeyDecoder.instance {
      case "UC"         => Some(StaticReqType.UseCase)
      case ParseInt(id) => Some(CustomReqTypeId(id))
      case _            => None
    }

  implicit lazy val keyEncoderReqTypeId: KeyEncoder[ReqTypeId] =
    KeyEncoder.instance {
      case StaticReqType.UseCase => "UC"
      case CustomReqTypeId(id)   => (id: Int).toString
    }

  private implicit def decoderFieldReqTypeRulesResolutionDefaultTo[D: Decoder]: Decoder[FieldReqTypeRules.Resolution.DefaultTo[D]] =
    Decoder[D].map(FieldReqTypeRules.Resolution.DefaultTo.apply[D])

  private implicit def encoderFieldReqTypeRulesResolutionDefaultTo[D: Encoder]: Encoder[FieldReqTypeRules.Resolution.DefaultTo[D]] =
    Encoder[D].contramap(_.default)

  private implicit def decoderFieldReqTypeRulesResolution[D](implicit d1: Decoder[FieldReqTypeRules.Resolution.DefaultTo[D]]): Decoder[FieldReqTypeRules.Resolution[D]] =
    decodeSumBySoleKeyOrConst[FieldReqTypeRules.Resolution[D]](
      "mandatory"     -> FieldReqTypeRules.Resolution.Mandatory,
      "notApplicable" -> FieldReqTypeRules.Resolution.NotApplicable,
      "optional"      -> FieldReqTypeRules.Resolution.Optional,
    ) {
      case ("defaultTo", c) => c.as[FieldReqTypeRules.Resolution.DefaultTo[D]]
    }

  private implicit def encoderFieldReqTypeRulesResolution[D](implicit e1: Encoder[FieldReqTypeRules.Resolution.DefaultTo[D]]): Encoder[FieldReqTypeRules.Resolution[D]] = Encoder.instance {
    case a: FieldReqTypeRules.Resolution.DefaultTo[D] => Json.obj("defaultTo" -> a.asJson)
    case FieldReqTypeRules.Resolution.Mandatory       => Json.fromString("mandatory")
    case FieldReqTypeRules.Resolution.NotApplicable   => Json.fromString("notApplicable")
    case FieldReqTypeRules.Resolution.Optional        => Json.fromString("optional")
  }

  implicit def decoderFieldReqTypeRules[D: Decoder]: Decoder[FieldReqTypeRules[D]] =
    Decoder.forProduct2("perReqType", "otherwise")(FieldReqTypeRules.apply[D])

  implicit def encoderFieldReqTypeRules[D: Encoder]: Encoder[FieldReqTypeRules[D]] =
    Encoder.forProduct2("perReqType", "otherwise")(a => (a.perReqType, a.otherwise))

  private[v1] implicit lazy val codecApplicableTagGD: JsonCodec[ApplicableTagGD.NonEmptyValues] = {
    import ApplicableTagGD._

    implicit val codecValueForApplicableReqTypes = JsonCodec.xmap(ValueForApplicableReqTypes.apply)(_.value)
    implicit val codecValueForChildren           = JsonCodec.xmap(ValueForChildren          .apply)(_.value)
    implicit val codecValueForColour             = JsonCodec.xmap(ValueForColour            .apply)(_.value)
    implicit val codecValueForDesc               = JsonCodec.xmap(ValueForDesc              .apply)(_.value)
    implicit val codecValueForKey                = JsonCodec.xmap(ValueForKey               .apply)(_.value)
    implicit val codecValueForParents            = JsonCodec.xmap(ValueForParents           .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("reqTypes", c) => c.as[ValueForApplicableReqTypes]
      case ("children", c) => c.as[ValueForChildren]
      case ("colour"  , c) => c.as[ValueForColour]
      case ("desc"    , c) => c.as[ValueForDesc]
      case ("key"     , c) => c.as[ValueForKey]
      case ("parents" , c) => c.as[ValueForParents]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForApplicableReqTypes => Json.obj("reqTypes" -> a.asJson)
      case a: ValueForChildren           => Json.obj("children" -> a.asJson)
      case a: ValueForColour             => Json.obj("colour"   -> a.asJson)
      case a: ValueForDesc               => Json.obj("desc"     -> a.asJson)
      case a: ValueForKey                => Json.obj("key"      -> a.asJson)
      case a: ValueForParents            => Json.obj("parents"  -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit lazy val codecCustomImpFieldGDv1: JsonCodec[CustomImpFieldGDv1.NonEmptyValues] = {
    import CustomImpFieldGDv1._

    implicit val codecValueForApplicableReqTypes = JsonCodec.xmap(ValueForApplicableReqTypes.apply)(_.value)
    implicit val codecValueForMandatory          = JsonCodec.xmap(ValueForMandatory         .apply)(_.value)
    implicit val codecValueForReqTypeId          = JsonCodec.xmap(ValueForReqTypeId         .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("mandatory", c) => c.as[ValueForMandatory]
      case ("reqTypeId", c) => c.as[ValueForReqTypeId]
      case ("reqTypes" , c) => c.as[ValueForApplicableReqTypes]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForMandatory          => Json.obj("mandatory" -> a.asJson)
      case a: ValueForReqTypeId          => Json.obj("reqTypeId" -> a.asJson)
      case a: ValueForApplicableReqTypes => Json.obj("reqTypes"  -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit lazy val codecCustomImpFieldGD: JsonCodec[CustomImpFieldGD.NonEmptyValues] = {
    import CustomImpFieldGD._

    implicit val codecValueForFieldReqTypeRules = JsonCodec.xmap(ValueForFieldReqTypeRules.apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("reqTypes", c) => c.as[ValueForFieldReqTypeRules]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForFieldReqTypeRules => Json.obj("reqTypes" -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit lazy val codecCustomIssueTypeGD: JsonCodec[CustomIssueTypeGD.NonEmptyValues] = {
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

  private[v1] implicit lazy val codecCustomReqTypeGD: JsonCodec[CustomReqTypeGD.NonEmptyValues] = {
    import CustomReqTypeGD._

    implicit val codecValueForImplication = JsonCodec.xmap(ValueForImplication.apply)(_.value)
    implicit val codecValueForMnemonic    = JsonCodec.xmap(ValueForMnemonic   .apply)(_.value)
    implicit val codecValueForName        = JsonCodec.xmap(ValueForName       .apply)(_.value)
    implicit val codecValueForDescription = JsonCodec.xmap(ValueForDescription.apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("imp"     , c) => c.as[ValueForImplication]
      case ("mnemonic", c) => c.as[ValueForMnemonic]
      case ("name"    , c) => c.as[ValueForName]
      case ("desc"    , c) => c.as[ValueForDescription]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForImplication => Json.obj("imp"      -> a.asJson)
      case a: ValueForMnemonic    => Json.obj("mnemonic" -> a.asJson)
      case a: ValueForName        => Json.obj("name"     -> a.asJson)
      case a: ValueForDescription => Json.obj("desc"     -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit lazy val codecCustomTagFieldGDv1: JsonCodec[CustomTagFieldGDv1.NonEmptyValues] = {
    import CustomTagFieldGDv1._

    implicit val codecValueForMandatory          = JsonCodec.xmap(ValueForMandatory         .apply)(_.value)
    implicit val codecValueForApplicableReqTypes = JsonCodec.xmap(ValueForApplicableReqTypes.apply)(_.value)
    implicit val codecValueForTagId              = JsonCodec.xmap(ValueForTagId             .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("mandatory", c) => c.as[ValueForMandatory]
      case ("reqTypes" , c) => c.as[ValueForApplicableReqTypes]
      case ("tagId"    , c) => c.as[ValueForTagId]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForMandatory          => Json.obj("mandatory" -> a.asJson)
      case a: ValueForApplicableReqTypes => Json.obj("reqTypes"  -> a.asJson)
      case a: ValueForTagId              => Json.obj("tagId"     -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit lazy val codecCustomTagFieldGD: JsonCodec[CustomTagFieldGD.NonEmptyValues] = {
    import CustomTagFieldGD._

    implicit val codecValueForFieldReqTypeRules = JsonCodec.xmap(ValueForFieldReqTypeRules.apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("reqTypes", c) => c.as[ValueForFieldReqTypeRules]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForFieldReqTypeRules => Json.obj("reqTypes" -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit lazy val codecCustomTextFieldGDv1: JsonCodec[CustomTextFieldGDv1.NonEmptyValues] = {
    import CustomTextFieldGDv1._

    implicit val codecValueForKey                = JsonCodec.xmap(ValueForKey               .apply)(_.value)
    implicit val codecValueForMandatory          = JsonCodec.xmap(ValueForMandatory         .apply)(_.value)
    implicit val codecValueForName               = JsonCodec.xmap(ValueForName              .apply)(_.value)
    implicit val codecValueForApplicableReqTypes = JsonCodec.xmap(ValueForApplicableReqTypes.apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("key"      , c) => c.as[ValueForKey]
      case ("mandatory", c) => c.as[ValueForMandatory]
      case ("name"     , c) => c.as[ValueForName]
      case ("reqTypes" , c) => c.as[ValueForApplicableReqTypes]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForKey                => Json.obj("key"       -> a.asJson)
      case a: ValueForMandatory          => Json.obj("mandatory" -> a.asJson)
      case a: ValueForName               => Json.obj("name"      -> a.asJson)
      case a: ValueForApplicableReqTypes => Json.obj("reqTypes"  -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit lazy val codecCustomTextFieldGD: JsonCodec[CustomTextFieldGD.NonEmptyValues] = {
    import CustomTextFieldGD._

    implicit val codecValueForName              = JsonCodec.xmap(ValueForName             .apply)(_.value)
    implicit val codecValueForFieldReqTypeRules = JsonCodec.xmap(ValueForFieldReqTypeRules.apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("name"    , c) => c.as[ValueForName]
      case ("reqTypes", c) => c.as[ValueForFieldReqTypeRules]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForName              => Json.obj("name"     -> a.asJson)
      case a: ValueForFieldReqTypeRules => Json.obj("reqTypes" -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit lazy val codecSavedViewGDv1: JsonCodec[SavedViewGDv1.NonEmptyValues] = {
    import SavedViewGDv1._

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

  private[v1] implicit lazy val codecSavedViewGD: JsonCodec[SavedViewGD.NonEmptyValues] = {
    import SavedViewGD._

    implicit val codecValueForColumns        = JsonCodec.xmap(ValueForColumns       .apply)(_.value)
    implicit val codecValueForFilter         = JsonCodec.xmap(ValueForFilter        .apply)(_.value)
    implicit val codecValueForFilterDead     = JsonCodec.xmap(ValueForFilterDead    .apply)(_.value)
    implicit val codecValueForName           = JsonCodec.xmap(ValueForName          .apply)(_.value)
    implicit val codecValueForOrder          = JsonCodec.xmap(ValueForOrder         .apply)(_.value)
    implicit val codecValueForImpGraphConfig = JsonCodec.xmap(ValueForImpGraphConfig.apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("columns"       , c) => c.as[ValueForColumns]
      case ("filter"        , c) => c.as[ValueForFilter]
      case ("filterDead"    , c) => c.as[ValueForFilterDead]
      case ("name"          , c) => c.as[ValueForName]
      case ("order"         , c) => c.as[ValueForOrder]
      case ("impGraphConfig", c) => c.as[ValueForImpGraphConfig]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForColumns        => Json.obj("columns"        -> a.asJson)
      case a: ValueForFilter         => Json.obj("filter"         -> a.asJson)
      case a: ValueForFilterDead     => Json.obj("filterDead"     -> a.asJson)
      case a: ValueForName           => Json.obj("name"           -> a.asJson)
      case a: ValueForOrder          => Json.obj("order"          -> a.asJson)
      case a: ValueForImpGraphConfig => Json.obj("impGraphConfig" -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  object EventData {
    implicit val decoderEventApplicableTagCreate: Decoder[Event.ApplicableTagCreate] =
      Decoder.forProduct2("id", "values")(Event.ApplicableTagCreate.apply)

    implicit val encoderEventApplicableTagCreate: Encoder[Event.ApplicableTagCreate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventApplicableTagUpdate: Decoder[Event.ApplicableTagUpdate] =
      Decoder.forProduct2("id", "values")(Event.ApplicableTagUpdate.apply)

    implicit val encoderEventApplicableTagUpdate: Encoder[Event.ApplicableTagUpdate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventCustomIssueTypeCreate: Decoder[Event.CustomIssueTypeCreate] =
      Decoder.forProduct2("id", "values")(Event.CustomIssueTypeCreate.apply)

    implicit val encoderEventCustomIssueTypeCreate: Encoder[Event.CustomIssueTypeCreate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventCustomIssueTypeUpdate: Decoder[Event.CustomIssueTypeUpdate] =
      Decoder.forProduct2("id", "values")(Event.CustomIssueTypeUpdate.apply)

    implicit val encoderEventCustomIssueTypeUpdate: Encoder[Event.CustomIssueTypeUpdate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventSavedViewCreateV1: Decoder[Event.SavedViewCreateV1] =
      Decoder.forProduct6("id", "name", "columns", "order", "filterDead", "filter")(Event.SavedViewCreateV1.apply)

    implicit val encoderEventSavedViewCreateV1: Encoder[Event.SavedViewCreateV1] =
      Encoder.forProduct6("id", "name", "columns", "order", "filterDead", "filter")(a => (a.id, a.name, a.columns, a.order, a.filterDead, a.filter))

    implicit val decoderEventSavedViewUpdateV1: Decoder[Event.SavedViewUpdateV1] =
      Decoder.forProduct2("id", "values")(Event.SavedViewUpdateV1.apply)

    implicit val encoderEventSavedViewUpdateV1: Encoder[Event.SavedViewUpdateV1] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventSavedViewCreate: Decoder[Event.SavedViewCreate] =
      Decoder.forProduct7("id", "name", "columns", "order", "filterDead", "filter", "impGraphConfig")(
        Event.SavedViewCreate.apply)

    implicit val encoderEventSavedViewCreate: Encoder[Event.SavedViewCreate] =
      Encoder.forProduct7("id", "name", "columns", "order", "filterDead", "filter", "impGraphConfig")(
        a => (a.id, a.name, a.columns, a.order, a.filterDead, a.filter, a.impGraphConfig))

    implicit val decoderEventSavedViewUpdate: Decoder[Event.SavedViewUpdate] =
      Decoder.forProduct2("id", "values")(Event.SavedViewUpdate.apply)

    implicit val encoderEventSavedViewUpdate: Encoder[Event.SavedViewUpdate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomTextCreateV1: Decoder[Event.FieldCustomTextCreateV1] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomTextCreateV1.apply)

    implicit val encoderEventFieldCustomTextCreateV1: Encoder[Event.FieldCustomTextCreateV1] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomTextUpdateV1: Decoder[Event.FieldCustomTextUpdateV1] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomTextUpdateV1.apply)

    implicit val encoderEventFieldCustomTextUpdateV1: Encoder[Event.FieldCustomTextUpdateV1] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomTagCreateV1: Decoder[Event.FieldCustomTagCreateV1] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomTagCreateV1.apply)

    implicit val encoderEventFieldCustomTagCreateV1: Encoder[Event.FieldCustomTagCreateV1] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomTagUpdateV1: Decoder[Event.FieldCustomTagUpdateV1] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomTagUpdateV1.apply)

    implicit val encoderEventFieldCustomTagUpdateV1: Encoder[Event.FieldCustomTagUpdateV1] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomImpCreateV1: Decoder[Event.FieldCustomImpCreateV1] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomImpCreateV1.apply)

    implicit val encoderEventFieldCustomImpCreateV1: Encoder[Event.FieldCustomImpCreateV1] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomImpUpdateV1: Decoder[Event.FieldCustomImpUpdateV1] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomImpUpdateV1.apply)

    implicit val encoderEventFieldCustomImpUpdateV1: Encoder[Event.FieldCustomImpUpdateV1] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomTextCreate: Decoder[Event.FieldCustomTextCreate] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomTextCreate.apply)

    implicit val encoderEventFieldCustomTextCreate: Encoder[Event.FieldCustomTextCreate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomTextUpdate: Decoder[Event.FieldCustomTextUpdate] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomTextUpdate.apply)

    implicit val encoderEventFieldCustomTextUpdate: Encoder[Event.FieldCustomTextUpdate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomTagCreate: Decoder[Event.FieldCustomTagCreate] =
      Decoder.forProduct3("id", "tagId", "values")(Event.FieldCustomTagCreate.apply)

    implicit val encoderEventFieldCustomTagCreate: Encoder[Event.FieldCustomTagCreate] =
      Encoder.forProduct3("id", "tagId", "values")(a => (a.id, a.tagId, a.vs))

    implicit val decoderEventFieldCustomTagUpdate: Decoder[Event.FieldCustomTagUpdate] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomTagUpdate.apply)

    implicit val encoderEventFieldCustomTagUpdate: Encoder[Event.FieldCustomTagUpdate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomImpCreate: Decoder[Event.FieldCustomImpCreate] =
      Decoder.forProduct3("id", "reqTypeId", "values")(Event.FieldCustomImpCreate.apply)

    implicit val encoderEventFieldCustomImpCreate: Encoder[Event.FieldCustomImpCreate] =
      Encoder.forProduct3("id", "reqTypeId", "values")(a => (a.id, a.reqTypeId, a.vs))

    implicit val decoderEventFieldCustomImpUpdate: Decoder[Event.FieldCustomImpUpdate] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomImpUpdate.apply)

    implicit val encoderEventFieldCustomImpUpdate: Encoder[Event.FieldCustomImpUpdate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventCustomReqTypeDeleteHard: Decoder[Event.CustomReqTypeDeleteHard] =
      Decoder[CustomReqTypeId].map(Event.CustomReqTypeDeleteHard.apply)

    implicit val encoderEventCustomReqTypeDeleteHard: Encoder[Event.CustomReqTypeDeleteHard] =
      Encoder[CustomReqTypeId].contramap(_.id)

    implicit val decoderEventCustomReqTypeDeleteSoft: Decoder[Event.CustomReqTypeDeleteSoft] =
      Decoder[CustomReqTypeId].map(Event.CustomReqTypeDeleteSoft.apply)

    implicit val encoderEventCustomReqTypeDeleteSoft: Encoder[Event.CustomReqTypeDeleteSoft] =
      Encoder[CustomReqTypeId].contramap(_.id)

    implicit val decoderEventCustomReqTypeCreate: Decoder[Event.CustomReqTypeCreate] =
      Decoder.forProduct2("id", "values")(Event.CustomReqTypeCreate.apply)

    implicit val encoderEventCustomReqTypeCreate: Encoder[Event.CustomReqTypeCreate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventCustomReqTypeUpdate: Decoder[Event.CustomReqTypeUpdate] =
      Decoder.forProduct2("id", "values")(Event.CustomReqTypeUpdate.apply)

    implicit val encoderEventCustomReqTypeUpdate: Encoder[Event.CustomReqTypeUpdate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldReposition: Decoder[Event.FieldReposition] =
      Decoder.forProduct2("id", "newPos")(Event.FieldReposition.apply)

    implicit val encoderEventFieldReposition: Encoder[Event.FieldReposition] =
      Encoder.forProduct2("id", "newPos")(a => (a.id, a.newPos))

    implicit val decoderEventFieldStaticAdd: Decoder[Event.FieldStaticAdd] =
      Decoder[StaticField.Optional].map(Event.FieldStaticAdd.apply)

    implicit val encoderEventFieldStaticAdd: Encoder[Event.FieldStaticAdd] =
      Encoder[StaticField.Optional].contramap(_.f)

    implicit val decoderEventFieldStaticRemove: Decoder[Event.FieldStaticRemove] =
      Decoder[StaticField.Optional].map(Event.FieldStaticRemove.apply)

    implicit val encoderEventFieldStaticRemove: Encoder[Event.FieldStaticRemove] =
      Encoder[StaticField.Optional].contramap(_.f)
  }
}
