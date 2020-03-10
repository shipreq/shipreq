package shipreq.webapp.base.protocol.json.v1

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import shipreq.base.util.JsonUtil._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.protocol.json.JsonCodec

/** v1.1
  *
  * Created because [[ApplicableTag]] lost and gained fields.
  */
object Rev1 {
  import JsonCodec.Implicits._
  import BaseData._
  import BaseMemberData1._
  import Events._
  import Events.EventData._
  import PostEvents._

  // ===================================================================================================================

  import BaseMemberData1.ReqTableDataCodecs._

  object ReqTableDataCodecs {
    import reqtable._

    implicit val decoderView: Decoder[View] =
      Decoder.forProduct4("columns", "order", "filterDead", "filter")(View.apply)

    implicit val encoderView: Encoder[View] =
      Encoder.forProduct4("columns", "order", "filterDead", "filter")(a => (a.columns, a.order, a.filterDead, a.filter))

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

  import ReqTableDataCodecs._

  // ===================================================================================================================

  private[this] object FilterAstKeys {
    final val KeyAstAllOf          = "all"
    final val KeyAstAnyOf          = "any"
    final val KeyAstHasIssue       = "issue"
    final val KeyAstHashRef        = "hash"
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

    implicit val decoderFilterAstRegex: Decoder[FilterAst.Regex] =
      Decoder[String].map(FilterAst.Regex.apply)

    implicit val encoderFilterAstRegex: Encoder[FilterAst.Regex] =
      Encoder[String].contramap(_.text)

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
      case a: FilterAst.Text                           => Json.obj(KeyAstText           -> a.asJson)
      case a: FilterAst.Regex                          => Json.obj(KeyAstRegex          -> a.asJson)
      case a: FilterAst.Presence      [Valid.Attr]     => Json.obj(KeyAstPresence       -> a.asJson)
      case a: FilterAst.HasIssue      [Valid.IssueCat] => Json.obj(KeyAstHasIssue       -> a.asJson)
      case a: FilterAst.HashRef       [Valid.HashTag]  => Json.obj(KeyAstHashRef        -> a.asJson)
      case a: FilterAst.ImpliesAnyOf  [Valid.ReqSet]   => Json.obj(KeyAstImpliesAnyOf   -> a.asJson)
      case a: FilterAst.ImpliedByAnyOf[Valid.ReqSet]   => Json.obj(KeyAstImpliedByAnyOf -> a.asJson)
      case a: FilterAst.Reqs          [Valid.ReqSet]   => Json.obj(KeyAstReqs           -> a.asJson)
      case a: FilterAst.ReqType       [Valid.ReqType]  => Json.obj(KeyAstReqType        -> a.asJson)
      case FilterAst.Not              (clause)         => Json.obj(KeyAstNot            -> clause)
      case FilterAst.AllOf            (clauses)        => Json.obj(KeyAstAllOf          -> Json.arr(clauses.whole: _*))
      case FilterAst.AnyOf            (head, tail)     => Json.obj(KeyAstAnyOf          -> Json.arr(head +: tail.whole: _*))
    }, decoderFnSumBySoleKey {
      case (KeyAstText          , c) => c.as[FilterAst.Text]
      case (KeyAstRegex         , c) => c.as[FilterAst.Regex]
      case (KeyAstPresence      , c) => c.as[FilterAst.Presence      [Valid.Attr]]
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

  private[v1] implicit lazy val codecCustomImpFieldGD: JsonCodec[CustomImpFieldGD.NonEmptyValues] = {
    import CustomImpFieldGD._

    implicit val codecValueForMandatory          = JsonCodec.xmap(ValueForMandatory.apply)(_.value)
    implicit val codecValueForReqTypeId          = JsonCodec.xmap(ValueForReqTypeId.apply)(_.value)
    implicit val codecValueForApplicableReqTypes = JsonCodec.xmap(ValueForApplicableReqTypes .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("mandatory", c) => c.as[ValueForMandatory]
      case ("reqTypeId", c) => c.as[ValueForReqTypeId]
      case ("reqTypes" , c) => c.as[ValueForApplicableReqTypes]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForMandatory => Json.obj("mandatory" -> a.asJson)
      case a: ValueForReqTypeId => Json.obj("reqTypeId" -> a.asJson)
      case a: ValueForApplicableReqTypes  => Json.obj("reqTypes"  -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit val codecCustomIssueTypeGD: JsonCodec[CustomIssueTypeGD.NonEmptyValues] = {
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

  private[v1] implicit lazy val codecCustomTagFieldGD: JsonCodec[CustomTagFieldGD.NonEmptyValues] = {
    import CustomTagFieldGD._

    implicit val codecValueForMandatory          = JsonCodec.xmap(ValueForMandatory.apply)(_.value)
    implicit val codecValueForApplicableReqTypes = JsonCodec.xmap(ValueForApplicableReqTypes .apply)(_.value)
    implicit val codecValueForTagId              = JsonCodec.xmap(ValueForTagId    .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("mandatory", c) => c.as[ValueForMandatory]
      case ("reqTypes" , c) => c.as[ValueForApplicableReqTypes]
      case ("tagId"    , c) => c.as[ValueForTagId]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForMandatory => Json.obj("mandatory" -> a.asJson)
      case a: ValueForApplicableReqTypes  => Json.obj("reqTypes"  -> a.asJson)
      case a: ValueForTagId     => Json.obj("tagId"     -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit lazy val codecCustomTextFieldGD: JsonCodec[CustomTextFieldGD.NonEmptyValues] = {
    import CustomTextFieldGD._

    implicit val codecValueForKey                = JsonCodec.xmap(ValueForKey      .apply)(_.value)
    implicit val codecValueForMandatory          = JsonCodec.xmap(ValueForMandatory.apply)(_.value)
    implicit val codecValueForName               = JsonCodec.xmap(ValueForName     .apply)(_.value)
    implicit val codecValueForApplicableReqTypes = JsonCodec.xmap(ValueForApplicableReqTypes .apply)(_.value)

    implicit val decoderValue: Decoder[Value] = decodeSumBySoleKey {
      case ("key"      , c) => c.as[ValueForKey]
      case ("mandatory", c) => c.as[ValueForMandatory]
      case ("name"     , c) => c.as[ValueForName]
      case ("reqTypes" , c) => c.as[ValueForApplicableReqTypes]
    }

    implicit val encoderValue: Encoder[Value] = Encoder.instance {
      case a: ValueForKey       => Json.obj("key"       -> a.asJson)
      case a: ValueForMandatory => Json.obj("mandatory" -> a.asJson)
      case a: ValueForName      => Json.obj("name"      -> a.asJson)
      case a: ValueForApplicableReqTypes  => Json.obj("reqTypes"  -> a.asJson)
    }

    implicit val values: JsonCodec[Values] = codecIMap(emptyValues)
    codecNonEmptyMono[Values]
  }

  private[v1] implicit lazy val codecSavedViewGD: JsonCodec[SavedViewGD.NonEmptyValues] = {
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

    implicit val decoderEventSavedViewCreate: Decoder[Event.SavedViewCreate] =
      Decoder.forProduct6("id", "name", "columns", "order", "filterDead", "filter")(Event.SavedViewCreate.apply)

    implicit val encoderEventSavedViewCreate: Encoder[Event.SavedViewCreate] =
      Encoder.forProduct6("id", "name", "columns", "order", "filterDead", "filter")(a => (a.id, a.name, a.columns, a.order, a.filterDead, a.filter))

    implicit val decoderEventSavedViewUpdate: Decoder[Event.SavedViewUpdate] =
      Decoder.forProduct2("id", "values")(Event.SavedViewUpdate.apply)

    implicit val encoderEventSavedViewUpdate: Encoder[Event.SavedViewUpdate] =
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
      Decoder.forProduct2("id", "values")(Event.FieldCustomTagCreate.apply)

    implicit val encoderEventFieldCustomTagCreate: Encoder[Event.FieldCustomTagCreate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomTagUpdate: Decoder[Event.FieldCustomTagUpdate] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomTagUpdate.apply)

    implicit val encoderEventFieldCustomTagUpdate: Encoder[Event.FieldCustomTagUpdate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomImpCreate: Decoder[Event.FieldCustomImpCreate] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomImpCreate.apply)

    implicit val encoderEventFieldCustomImpCreate: Encoder[Event.FieldCustomImpCreate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))

    implicit val decoderEventFieldCustomImpUpdate: Decoder[Event.FieldCustomImpUpdate] =
      Decoder.forProduct2("id", "values")(Event.FieldCustomImpUpdate.apply)

    implicit val encoderEventFieldCustomImpUpdate: Encoder[Event.FieldCustomImpUpdate] =
      Encoder.forProduct2("id", "values")(a => (a.id, a.vs))
  }

  import EventData._

  implicit lazy val decoderEvent: Decoder[Event] = decodeSumBySoleKey {
    case ("ApplicableTagCreate"   , c) => c.as[Event.ApplicableTagCreateV1]
    case ("ApplicableTagCreate:2" , c) => c.as[Event.ApplicableTagCreate]
    case ("ApplicableTagUpdate"   , c) => c.as[Event.ApplicableTagUpdateV1]
    case ("ApplicableTagUpdate:2" , c) => c.as[Event.ApplicableTagUpdate]
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

  implicit lazy val encoderEvent: Encoder[Event] = Encoder.instance {
    case a: Event.ApplicableTagCreate    => Json.obj("ApplicableTagCreate:2"  -> a.asJson)
    case a: Event.ApplicableTagCreateV1  => Json.obj("ApplicableTagCreate"    -> a.asJson)
    case a: Event.ApplicableTagUpdate    => Json.obj("ApplicableTagUpdate:2"  -> a.asJson)
    case a: Event.ApplicableTagUpdateV1  => Json.obj("ApplicableTagUpdate"    -> a.asJson)
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

  implicit lazy val decoderVerifiedEvent: Decoder[VerifiedEvent] =
    Decoder.forProduct3("#", "event", "createdAt")(VerifiedEvent.apply)

  implicit lazy val encoderVerifiedEvent: Encoder[VerifiedEvent] =
    Encoder.forProduct3("#", "event", "createdAt")(a => (a.ord, a.event, a.createdAt))
}
