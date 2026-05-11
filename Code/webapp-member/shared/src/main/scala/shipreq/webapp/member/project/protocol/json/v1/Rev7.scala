package shipreq.webapp.member.project.protocol.json.v1

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.adt_macros.AdtMacros
import shipreq.base.util.JsonUtil._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.RetiredGenericData._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.filter.Filter
import shipreq.webapp.member.protocol.json.JsonCodec

/** v1.7
  *
  * Changes:
  *
  *   - add FilterAst.Scoped
  */
object Rev7 {
  import JsonCodec.Implicits._
  import BaseData._
  import BaseMemberData1._
  import Rev1._
  import Rev4._

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
    final val KeyAstScoped1        = "scoped1"
    final val KeyAstScoped2        = "scoped2"
    final val KeyAstRelativeTags   = "relTags"
  }

  implicit lazy val codecValidFilter: JsonCodec[Filter.Valid] = {
    import shipreq.webapp.member.project.filter.{IntensionalReqSet, FilterAst}
    import Filter._
    import Filter.Implicits._
    import Filter.Valid.FieldCriteriaF
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
        case FilterAst.FieldAttr.NotBlank      => "notBlank"
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

    implicit val decoderFieldCriteriaAttr: Decoder[FilterAst.FieldCriteria.Attr[FilterAst.FieldAttr]] =
      Decoder[FilterAst.FieldAttr].map(FilterAst.FieldCriteria.Attr.apply)

    implicit val encoderFieldCriteriaAttr: Encoder[FilterAst.FieldCriteria.Attr[FilterAst.FieldAttr]] =
      Encoder[FilterAst.FieldAttr].contramap(_.value)

    implicit val decoderFieldCriteriaReqTypePosSet: Decoder[FilterAst.FieldCriteria.ReqTypePosSet] =
      Decoder[NonEmptySet[ReqTypePos]].map(FilterAst.FieldCriteria.ReqTypePosSet.apply)

    implicit val encoderFieldCriteriaReqTypePosSet: Encoder[FilterAst.FieldCriteria.ReqTypePosSet] =
      Encoder[NonEmptySet[ReqTypePos]].contramap(_.value)

    implicit val decoderFieldCriteriaQuery: Decoder[FilterAst.FieldCriteria.Query[ACursor]] =
      Decoder.instance(c => Right(FilterAst.FieldCriteria.Query(c)))

    implicit val encoderFieldCriteriaQuery: Encoder[FilterAst.FieldCriteria.Query[Json]] =
      Encoder[Json].contramap(_.value)

    implicit val decoderFieldCriteria: Decoder[FieldCriteriaF[ACursor]] = decodeSumBySoleKey {
      case ("attr" , c) => c.as[FilterAst.FieldCriteria.Attr[FilterAst.FieldAttr]]
      case ("rtpos", c) => c.as[FilterAst.FieldCriteria.ReqTypePosSet]
      case ("query", c) => c.as[FilterAst.FieldCriteria.Query[ACursor]]
    }

    implicit val encoderFieldCriteria: Encoder[FieldCriteriaF[Json]] = Encoder.instance {
      case a: FilterAst.FieldCriteria.Attr[FilterAst.FieldAttr] => Json.obj("attr"  -> a.asJson)
      case a: FilterAst.FieldCriteria.ReqTypePosSet             => Json.obj("rtpos" -> a.asJson)
      case a: FilterAst.FieldCriteria.Query[Json]               => Json.obj("query" -> a.asJson)
    }

    implicit val decoderFilterAstFieldProp: Decoder[Valid.FieldPropF[ACursor]] =
      Decoder.instance { c =>
        for {
          field    <- c.get[Valid.Field]("field")
          criteria <- c.get[Valid.FieldCriteriaF[ACursor]]("criteria") orElse c.get[FilterAst.FieldAttr]("attr").map(FilterAst.FieldCriteria.Attr(_))
        } yield FilterAst.FieldProp(field, criteria)
      }

    implicit val encoderFilterAstFieldProp: Encoder[Valid.FieldPropF[Json]] =
      Encoder.forProduct2("field", "criteria")(a => (a.field, a.criteria))

    implicit val decoderFilterAstHashRef: Decoder[FilterAst.HashRef[Valid.HashTag]] =
      Decoder[Valid.HashTag].map(FilterAst.HashRef.apply)

    implicit val encoderFilterAstHashRef: Encoder[FilterAst.HashRef[Valid.HashTag]] =
      Encoder[Valid.HashTag].contramap(_.value)

    implicit def decoderFilterAstImpCriteriaReqs[R: Decoder]: Decoder[FilterAst.ImpCriteria.Reqs[R]] =
      Decoder[R].map(FilterAst.ImpCriteria.Reqs.apply[R])

    implicit def encoderFilterAstImpCriteriaReqs[R: Encoder]: Encoder[FilterAst.ImpCriteria.Reqs[R]] =
      Encoder[R].contramap(_.value)

    implicit val decoderFilterAstImpCriteriaQuery: Decoder[FilterAst.ImpCriteria.Query[ACursor]] =
      Decoder.instance(c => Right(FilterAst.ImpCriteria.Query(c)))

    implicit val encoderFilterAstImpCriteriaQuery: Encoder[FilterAst.ImpCriteria.Query[Json]] =
      Encoder[Json].contramap(_.value)

    implicit def decoderFilterAstImpCriteria[R, Q](implicit d1: Decoder[FilterAst.ImpCriteria.Query[Q]], d2: Decoder[FilterAst.ImpCriteria.Reqs[R]]): Decoder[FilterAst.ImpCriteria[R, Q]] = decodeSumBySoleKey {
      case ("query", c) => c.as[FilterAst.ImpCriteria.Query[Q]]
      case ("reqs" , c) => c.as[FilterAst.ImpCriteria.Reqs[R]]
    }

    implicit def encoderFilterAstImpCriteria[R, Q](implicit e1: Encoder[FilterAst.ImpCriteria.Query[Q]], e2: Encoder[FilterAst.ImpCriteria.Reqs[R]]): Encoder[FilterAst.ImpCriteria[R, Q]] = Encoder.instance {
      case a: FilterAst.ImpCriteria.Query[Q] => Json.obj("query" -> a.asJson)
      case a: FilterAst.ImpCriteria.Reqs[R]  => Json.obj("reqs"  -> a.asJson)
    }

    val decoderImpCriteria: Decoder[Valid.ImpCriteriaF[ACursor]] =
      decoderFilterAstImpCriteria[Valid.ReqSet, ACursor]
        .or(Decoder[Valid.ReqSet].map(FilterAst.ImpCriteria.Reqs(_))) // backwards-compatibility

    val encoderImpCriteria: Encoder[Valid.ImpCriteriaF[Json]] =
      encoderFilterAstImpCriteria

    implicit val decoderFilterAstImpliesAnyOf: Decoder[FilterAst.ImpliesAnyOf[Valid.ImpCriteriaF, ACursor]] =
      decoderImpCriteria.map(FilterAst.ImpliesAnyOf(_))

    implicit val encoderFilterAstImpliesAnyOf: Encoder[FilterAst.ImpliesAnyOf[Valid.ImpCriteriaF, Json]] =
      encoderImpCriteria.contramap(_.criteria)

    implicit val decoderFilterAstImpliedByAnyOf: Decoder[FilterAst.ImpliedByAnyOf[Valid.ImpCriteriaF, ACursor]] =
      decoderImpCriteria.map(FilterAst.ImpliedByAnyOf(_))

    implicit val encoderFilterAstImpliedByAnyOf: Encoder[FilterAst.ImpliedByAnyOf[Valid.ImpCriteriaF, Json]] =
      encoderImpCriteria.contramap(_.criteria)

    implicit val decoderFilterAstReqs: Decoder[FilterAst.Reqs[Valid.ReqSet]] =
      Decoder[Valid.ReqSet].map(FilterAst.Reqs.apply)

    implicit val encoderFilterAstReqs: Encoder[FilterAst.Reqs[Valid.ReqSet]] =
      Encoder[Valid.ReqSet].contramap(_.reqs)

    implicit val decoderFilterAstReqType: Decoder[FilterAst.ReqType[Valid.ReqType]] =
      Decoder[Valid.ReqType].map(FilterAst.ReqType.apply)

    implicit val encoderFilterAstReqType: Encoder[FilterAst.ReqType[Valid.ReqType]] =
      Encoder[Valid.ReqType].contramap(_.reqType)

    implicit def decoderFilterAstScopeDerivation[A: Decoder]: Decoder[FilterAst.Scope.Derivation[A]] =
      Decoder[Option[A]].map(FilterAst.Scope.Derivation.apply[A])

    implicit def encoderFilterAstScopeDerivation[A: Encoder]: Encoder[FilterAst.Scope.Derivation[A]] =
      Encoder[Option[A]].contramap(_.field)

    implicit def decoderFilterAstScope[A](implicit d1: Decoder[FilterAst.Scope.Derivation[A]]): Decoder[FilterAst.Scope[A]] = decodeSumBySoleKey {
      case ("derivation", c) => c.as[FilterAst.Scope.Derivation[A]]
    }

    implicit def encoderFilterAstScope[A](implicit e1: Encoder[FilterAst.Scope.Derivation[A]]): Encoder[FilterAst.Scope[A]] = Encoder.instance {
      case a: FilterAst.Scope.Derivation[A] => Json.obj("derivation" -> a.asJson)
    }

    implicit val codecFilterAstScope: JsonCodec[Valid.Scope] =
      codecNES

    implicit val decoderFilterAstScoped1: Decoder[FilterAst.Scoped1[Valid.Scope, ACursor]] =
      Decoder.instance { c =>
        for {
          main   <- c.get[Boolean]("main")
          scope  <- c.get[Valid.Scope]("scope")
        } yield FilterAst.Scoped1(main, scope, c.downField("clause"))
      }

    implicit val encoderFilterAstScoped1: Encoder[FilterAst.Scoped1[Valid.Scope, Json]] =
      Encoder.forProduct3("main", "scope", "clause")(a => (a.main, a.scope, a.clause))

    implicit val decoderFilterAstScoped2: Decoder[FilterAst.Scoped2[Valid.Scope, ACursor]] =
      Decoder.instance { c =>
        for {
          scope <- c.get[Valid.Scope]("scope")
        } yield FilterAst.Scoped2(scope, c.downField("clause"), c.downField("main"))
      }

    implicit val encoderFilterAstScoped2: Encoder[FilterAst.Scoped2[Valid.Scope, Json]] =
      Encoder.forProduct3("scope", "clause", "main")(a => (a.scope, a.clause, a.mainClause))

    implicit val codecFilterAstOrderOp: JsonCodec[FilterAst.OrderOp] =
      JsonCodec.enumAdt(AdtMacros.adtIsoSet[FilterAst.OrderOp, String] {
        case FilterAst.OrderOp.<  => "<"
        case FilterAst.OrderOp.>  => ">"
        case FilterAst.OrderOp.<= => "<="
        case FilterAst.OrderOp.>= => ">="
      })

    implicit val decoderFilterAstRelativeTags: Decoder[FilterAst.RelativeTags[Valid.ApTag]] =
      Decoder.forProduct2("op", "subject")(FilterAst.RelativeTags.apply[Valid.ApTag])

    implicit val encoderFilterAstRelativeTags: Encoder[FilterAst.RelativeTags[Valid.ApTag]] =
      Encoder.forProduct2("op", "subject")(a => (a.op, a.subject))

    JsonCodec.fix[ValidF]({
      case a: FilterAst.Text                           => Json.obj(KeyAstText           -> a.asJson)
      case a: FilterAst.Regex                          => Json.obj(KeyAstRegex          -> a.asJson)
      case a: FilterAst.Presence      [Valid.Attr]     => Json.obj(KeyAstPresence       -> a.asJson)
      case a: FilterAst.HasIssue      [Valid.IssueCat] => Json.obj(KeyAstHasIssue       -> a.asJson)
      case a: FilterAst.HashRef       [Valid.HashTag]  => Json.obj(KeyAstHashRef        -> a.asJson)
      case a: FilterAst.RelativeTags  [Valid.ApTag]    => Json.obj(KeyAstRelativeTags   -> a.asJson)
      case a@ FilterAst.ImpliesAnyOf  (_)              => Json.obj(KeyAstImpliesAnyOf   -> a.asJson)
      case a@ FilterAst.ImpliedByAnyOf(_)              => Json.obj(KeyAstImpliedByAnyOf -> a.asJson)
      case a: FilterAst.Reqs          [Valid.ReqSet]   => Json.obj(KeyAstReqs           -> a.asJson)
      case a: FilterAst.ReqType       [Valid.ReqType]  => Json.obj(KeyAstReqType        -> a.asJson)
      case a@ FilterAst.FieldProp     (_, _)           => Json.obj(KeyAstFieldProp      -> a.asJson)
      case a@ FilterAst.Scoped1       (_, _, _)        => Json.obj(KeyAstScoped1        -> a.asJson)
      case a@ FilterAst.Scoped2       (_, _, _)        => Json.obj(KeyAstScoped2        -> a.asJson)
      case FilterAst.Not              (clause)         => Json.obj(KeyAstNot            -> clause)
      case FilterAst.AllOf            (clauses)        => Json.obj(KeyAstAllOf          -> Json.arr(clauses.whole: _*))
      case FilterAst.AnyOf            (head, tail)     => Json.obj(KeyAstAnyOf          -> Json.arr(head +: tail.whole: _*))
    }, decoderFnSumBySoleKey {
      case (KeyAstText          , c) => c.as[FilterAst.Text]
      case (KeyAstRegex         , c) => c.as[FilterAst.Regex]
      case (KeyAstPresence      , c) => c.as[FilterAst.Presence      [Valid.Attr]]
      case (KeyAstHasIssue      , c) => c.as[FilterAst.HasIssue      [Valid.IssueCat]]
      case (KeyAstHashRef       , c) => c.as[FilterAst.HashRef       [Valid.HashTag]]
      case (KeyAstRelativeTags  , c) => c.as[FilterAst.RelativeTags  [Valid.ApTag]]
      case (KeyAstImpliesAnyOf  , c) => c.as[FilterAst.ImpliesAnyOf  [Valid.ImpCriteriaF, ACursor]]
      case (KeyAstImpliedByAnyOf, c) => c.as[FilterAst.ImpliedByAnyOf[Valid.ImpCriteriaF, ACursor]]
      case (KeyAstReqs          , c) => c.as[FilterAst.Reqs          [Valid.ReqSet]]
      case (KeyAstReqType       , c) => c.as[FilterAst.ReqType       [Valid.ReqType]]
      case (KeyAstScoped1       , c) => c.as[FilterAst.Scoped1       [Valid.Scope, ACursor]]
      case (KeyAstScoped2       , c) => c.as[FilterAst.Scoped2       [Valid.Scope, ACursor]]
      case (KeyAstFieldProp     , c) => c.as[Valid.FieldPropF        [ACursor]]
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
    import shipreq.webapp.member.project.data.savedview._

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

  private[json] implicit lazy val codecSavedViewGDv1: JsonCodec[SavedViewGDv1.NonEmptyValues] = {
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

  private[json] implicit lazy val codecSavedViewGD: JsonCodec[SavedViewGD.NonEmptyValues] = {
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
}