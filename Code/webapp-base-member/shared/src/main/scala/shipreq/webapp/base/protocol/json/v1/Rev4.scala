package shipreq.webapp.base.protocol.json.v1

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import shipreq.base.util.JsonUtil._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.RetiredGenericData._
import shipreq.webapp.base.event._
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.protocol.json.JsonCodec

/** v1.4 */
object Rev4 {
  import JsonCodec.Implicits._
  import BaseData._
  import BaseMemberData1._
  import PostEvents._
  import Rev1._

  implicit lazy val codecReqTypePos: JsonCodec[ReqTypePos] =
    JsonCodec.xmap(ReqTypePos)(_.value)

  implicit lazy val codecReqTypePosNES: JsonCodec[NonEmptySet[ReqTypePos]] =
    codecNES

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
    import Filter.Valid.FieldCriteria
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

    implicit val decoderFieldCriteriaAttr: Decoder[FieldCriteria.Attr] =
      Decoder[FilterAst.FieldAttr].map(FieldCriteria.Attr.apply)

    implicit val encoderFieldCriteriaAttr: Encoder[FieldCriteria.Attr] =
      Encoder[FilterAst.FieldAttr].contramap(_.attr)

    implicit val decoderFieldCriteriaReqTypePosSet: Decoder[FieldCriteria.ReqTypePosSet] =
      Decoder[NonEmptySet[ReqTypePos]].map(FieldCriteria.ReqTypePosSet.apply)

    implicit val encoderFieldCriteriaReqTypePosSet: Encoder[FieldCriteria.ReqTypePosSet] =
      Encoder[NonEmptySet[ReqTypePos]].contramap(_.values)

    implicit val decoderFieldCriteria: Decoder[FieldCriteria] = decodeSumBySoleKey {
      case ("attr" , c) => c.as[FieldCriteria.Attr]
      case ("rtpos", c) => c.as[FieldCriteria.ReqTypePosSet]
    }

    implicit val encoderFieldCriteria: Encoder[FieldCriteria] = Encoder.instance {
      case a: FieldCriteria.Attr          => Json.obj("attr"  -> a.asJson)
      case a: FieldCriteria.ReqTypePosSet => Json.obj("rtpos" -> a.asJson)
    }

    implicit val decoderFilterAstFieldProp: Decoder[FilterAst.FieldProp[Valid.Field, Valid.FieldCriteria]] =
      Decoder.instance { c =>
        for {
          field    <- c.get[Valid.Field]("field")
          criteria <- c.get[Valid.FieldCriteria]("criteria") orElse c.get[FilterAst.FieldAttr]("attr").map(Valid.FieldCriteria.Attr)
        } yield FilterAst.FieldProp(field, criteria)
      }

    implicit val encoderFilterAstFieldProp: Encoder[FilterAst.FieldProp[Valid.Field, Valid.FieldCriteria]] =
      Encoder.forProduct2("field", "criteria")(a => (a.field, a.criteria))

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
      case a: FilterAst.FieldProp     [Valid.Field, Valid.FieldCriteria] => Json.obj(KeyAstFieldProp      -> a.asJson)
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
      case (KeyAstFieldProp     , c) => c.as[FilterAst.FieldProp     [Valid.Field, Valid.FieldCriteria]]
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

  import BaseMemberData1.SavedViewCodecs._
  import Rev1.SavedViewCodecs._

  object SavedViewCodecs {
    import shipreq.webapp.base.data.savedview._

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


  // ===================================================================================================================

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
  }

  // ===================================================================================================================

  import Events.EventData._
  import Rev1.EventData._
  import Rev3.EventData._
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

  // ===================================================================================================================
}
