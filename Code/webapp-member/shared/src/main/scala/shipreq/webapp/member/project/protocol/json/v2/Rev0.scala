package shipreq.webapp.member.project.protocol.json.v2

import io.circe._
import io.circe.syntax._
import shipreq.base.util.JsonUtil._
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.Obfuscated
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.protocol.json.JsonCodec
import shipreq.webapp.member.protocol.json.JsonCodec.Implicits._

/** v2.0: For ShipReq Phase 3. */
object Rev0 {

  // implicit lazy val jsonCodecUserIdPublic: JsonCodec[UserId.Public] =
  //   JsonCodec.obfuscated

  implicit lazy val keyDecoderUserIdPublic: KeyDecoder[UserId.Public] =
    KeyDecoder.decodeKeyString.map(Obfuscated.apply)

  implicit lazy val keyEncoderUserIdPublic: KeyEncoder[UserId.Public] =
    KeyEncoder.encodeKeyString.contramap(_.value)

  implicit lazy val decoderProjectRole: Decoder[ProjectRole] =
    Decoder.instance(c =>
      c.value.asString match {
        case Some("admin")        => Right(ProjectRole.Admin)
        case Some("collaborator") => Right(ProjectRole.Collaborator)
        case Some("viewer")       => Right(ProjectRole.Viewer)
        case _                    => Left(DecodingFailure("Invalid project role: " + c.value.noSpaces, c.history))
      }
    )

  implicit lazy val encoderProjectRole: Encoder[ProjectRole] = Encoder.instance {
    case ProjectRole.Admin        => Json.fromString("admin")
    case ProjectRole.Collaborator => Json.fromString("collaborator")
    case ProjectRole.Viewer       => Json.fromString("viewer")
  }

  implicit lazy val decoderOptionProjectRole: Decoder[Option[ProjectRole]] =
    Decoder.instance(c =>
      c.value.asString match {
        case Some("-") => Right(None)
        case _         => decoderProjectRole(c).map(Some(_))
      }
    )

  implicit lazy val encoderOptionProjectRole: Encoder[Option[ProjectRole]] = Encoder.instance {
    case None    => Json.fromString("-")
    case Some(p) => encoderProjectRole(p)
  }

  object EventData {

    implicit val jsonCodecEventAccessUpdate: JsonCodec[Event.AccessUpdate] =
      JsonCodec.map[UserId.Public, Option[ProjectRole]]
        .xmap(Event.AccessUpdate.apply)(_.updates)
  }

  // ===================================================================================================================

  import shipreq.webapp.member.project.protocol.json.v1.Events.EventData._
  import shipreq.webapp.member.project.protocol.json.v1.Rev1.EventData._
  import shipreq.webapp.member.project.protocol.json.v1.Rev6.EventData._
  import shipreq.webapp.member.project.protocol.json.v1.Rev7.EventData._
  import shipreq.webapp.member.project.protocol.json.v1.PostEvents._
  import EventData._

  implicit lazy val decoderEvent: Decoder[Event] = decodeSumBySoleKey {
    case ("AccessUpdate"           , c) => c.as[Event.AccessUpdate]
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
    case a: Event.AccessUpdate            => Json.obj("AccessUpdate"            -> a.asJson)
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
