package shipreq.webapp.server.db

import io.circe._
import io.circe.syntax._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.JsonUtil
import shipreq.webapp.base.event.{ActiveEvent, Event, EventOrd}
import shipreq.webapp.base.protocol.json.v1.Events.EventData._
import shipreq.webapp.base.protocol.json.v1.Rev1.EventData._
import shipreq.webapp.server.logic.DB.ReadProjectEventError

object EventSerialisation {
  import Event._
  import EventTypes._

  val encode: ActiveEvent => (Short, Json) = {
    case e: ApplicableTagCreate     => (TypeApplicableTagCreateV2  , e.asJson)
    case e: ApplicableTagUpdate     => (TypeApplicableTagUpdateV2  , e.asJson)
    case e: CodeGroupCreate         => (TypeCodeGroupCreate        , e.asJson)
    case e: CodeGroupsDelete        => (TypeCodeGroupsDelete       , e.asJson)
    case e: CodeGroupUpdate         => (TypeCodeGroupUpdate        , e.asJson)
    case e: ContentRestore          => (TypeContentRestore         , e.asJson)
    case e: CustomIssueTypeCreate   => (TypeCustomIssueTypeCreate  , e.asJson)
    case e: CustomIssueTypeDelete   => (TypeCustomIssueTypeDelete  , e.asJson)
    case e: CustomIssueTypeRestore  => (TypeCustomIssueTypeRestore , e.asJson)
    case e: CustomIssueTypeUpdate   => (TypeCustomIssueTypeUpdate  , e.asJson)
    case e: CustomReqTypeCreate     => (TypeCustomReqTypeCreate    , e.asJson)
    case e: CustomReqTypeDeleteHard => (TypeCustomReqTypeDeleteHard, e.asJson)
    case e: CustomReqTypeDeleteSoft => (TypeCustomReqTypeDeleteSoft, e.asJson)
    case e: CustomReqTypeRestore    => (TypeCustomReqTypeRestore   , e.asJson)
    case e: CustomReqTypeUpdate     => (TypeCustomReqTypeUpdate    , e.asJson)
    case e: FieldCustomDelete       => (TypeFieldCustomDelete      , e.asJson)
    case e: FieldCustomImpCreate    => (TypeFieldCustomImpCreate   , e.asJson)
    case e: FieldCustomImpUpdate    => (TypeFieldCustomImpUpdate   , e.asJson)
    case e: FieldCustomRestore      => (TypeFieldCustomRestore     , e.asJson)
    case e: FieldCustomTagCreate    => (TypeFieldCustomTagCreate   , e.asJson)
    case e: FieldCustomTagUpdate    => (TypeFieldCustomTagUpdate   , e.asJson)
    case e: FieldCustomTextCreate   => (TypeFieldCustomTextCreate  , e.asJson)
    case e: FieldCustomTextUpdate   => (TypeFieldCustomTextUpdate  , e.asJson)
    case e: FieldReposition         => (TypeFieldReposition        , e.asJson)
    case e: FieldStaticAdd          => (TypeFieldStaticAdd         , e.asJson)
    case e: FieldStaticRemove       => (TypeFieldStaticRemove      , e.asJson)
    case e: GenericReqCreate        => (TypeGenericReqCreate       , e.asJson)
    case e: GenericReqTitleSet      => (TypeGenericReqTitleSet     , e.asJson)
    case e: GenericReqTypeSet       => (TypeGenericReqTypeSet      , e.asJson)
    case e: ManualIssueCreate       => (TypeManualIssueCreate      , e.asJson)
    case e: ManualIssueDelete       => (TypeManualIssueDelete      , e.asJson)
    case e: ManualIssueUpdate       => (TypeManualIssueUpdate      , e.asJson)
    case e: ProjectNameSet          => (TypeProjectNameSet         , e.asJson)
    case e: ProjectTemplateApply    => (TypeProjectTemplateApply   , e.asJson)
    case e: ReqCodesPatch           => (TypeReqCodesPatch          , e.asJson)
    case e: ReqFieldCustomTextSet   => (TypeReqFieldCustomTextSet  , e.asJson)
    case e: ReqImplicationsPatch    => (TypeReqImplicationsPatch   , e.asJson)
    case e: ReqsDelete              => (TypeReqsDelete             , e.asJson)
    case e: ReqTagsPatch            => (TypeReqTagsPatch           , e.asJson)
    case e: SavedViewCreate         => (TypeSavedViewCreate        , e.asJson)
    case e: SavedViewDefaultSet     => (TypeSavedViewDefaultSet    , e.asJson)
    case e: SavedViewDelete         => (TypeSavedViewDelete        , e.asJson)
    case e: SavedViewUpdate         => (TypeSavedViewUpdate        , e.asJson)
    case e: TagDelete               => (TypeTagDelete              , e.asJson)
    case e: TagGroupCreate          => (TypeTagGroupCreate         , e.asJson)
    case e: TagGroupUpdate          => (TypeTagGroupUpdate         , e.asJson)
    case e: TagRestore              => (TypeTagRestore             , e.asJson)
    case e: UseCaseCreate           => (TypeUseCaseCreate          , e.asJson)
    case e: UseCaseStepCreate       => (TypeUseCaseStepCreate      , e.asJson)
    case e: UseCaseStepDelete       => (TypeUseCaseStepDelete      , e.asJson)
    case e: UseCaseStepRestore      => (TypeUseCaseStepRestore     , e.asJson)
    case e: UseCaseStepShiftLeft    => (TypeUseCaseStepShiftLeft   , e.asJson)
    case e: UseCaseStepShiftRight   => (TypeUseCaseStepShiftRight  , e.asJson)
    case e: UseCaseStepUpdate       => (TypeUseCaseStepUpdate      , e.asJson)
    case e: UseCaseTitleSet         => (TypeUseCaseTitleSet        , e.asJson)
  }

  // ===================================================================================================================

  type Decoded = ReadProjectEventError \/ Event

  def decode(ord: EventOrd, typeId: Short, json: Json): Decoded = {

    def fail(msg: String): Decoded =
      -\/(ReadProjectEventError.DecodeFailure(ord, msg))

    def parse[A <: Event](implicit d: Decoder[A]): Decoded =
      d.decodeJson(json) match {
        case Right(a) => \/-(a)
        case Left(f)  => fail(JsonUtil.decodingFailureMsg(f))
      }

    typeId match {
      case TypeApplicableTagCreateV1   => parse[ApplicableTagCreateV1]
      case TypeApplicableTagCreateV2   => parse[ApplicableTagCreate]
      case TypeApplicableTagUpdateV1   => parse[ApplicableTagUpdateV1]
      case TypeApplicableTagUpdateV2   => parse[ApplicableTagUpdate]
      case TypeCodeGroupCreate         => parse[CodeGroupCreate]
      case TypeCodeGroupsDelete        => parse[CodeGroupsDelete]
      case TypeCodeGroupUpdate         => parse[CodeGroupUpdate]
      case TypeContentRestore          => parse[ContentRestore]
      case TypeCustomIssueTypeCreate   => parse[CustomIssueTypeCreate]
      case TypeCustomIssueTypeDelete   => parse[CustomIssueTypeDelete]
      case TypeCustomIssueTypeRestore  => parse[CustomIssueTypeRestore]
      case TypeCustomIssueTypeUpdate   => parse[CustomIssueTypeUpdate]
      case TypeCustomReqTypeCreate     => parse[CustomReqTypeCreate]
      case TypeCustomReqTypeDelete     => parse[CustomReqTypeDelete]
      case TypeCustomReqTypeDeleteHard => parse[CustomReqTypeDeleteHard]
      case TypeCustomReqTypeDeleteSoft => parse[CustomReqTypeDeleteSoft]
      case TypeCustomReqTypeRestore    => parse[CustomReqTypeRestore]
      case TypeCustomReqTypeUpdate     => parse[CustomReqTypeUpdate]
      case TypeFieldCustomDelete       => parse[FieldCustomDelete]
      case TypeFieldCustomImpCreate    => parse[FieldCustomImpCreate]
      case TypeFieldCustomImpUpdate    => parse[FieldCustomImpUpdate]
      case TypeFieldCustomRestore      => parse[FieldCustomRestore]
      case TypeFieldCustomTagCreate    => parse[FieldCustomTagCreate]
      case TypeFieldCustomTagUpdate    => parse[FieldCustomTagUpdate]
      case TypeFieldCustomTextCreate   => parse[FieldCustomTextCreate]
      case TypeFieldCustomTextUpdate   => parse[FieldCustomTextUpdate]
      case TypeFieldReposition         => parse[FieldReposition]
      case TypeFieldStaticAdd          => parse[FieldStaticAdd]
      case TypeFieldStaticRemove       => parse[FieldStaticRemove]
      case TypeGenericReqCreate        => parse[GenericReqCreate]
      case TypeGenericReqTitleSet      => parse[GenericReqTitleSet]
      case TypeGenericReqTypeSet       => parse[GenericReqTypeSet]
      case TypeManualIssueCreate       => parse[ManualIssueCreate]
      case TypeManualIssueDelete       => parse[ManualIssueDelete]
      case TypeManualIssueUpdate       => parse[ManualIssueUpdate]
      case TypeProjectNameSet          => parse[ProjectNameSet]
      case TypeProjectTemplateApply    => parse[ProjectTemplateApply]
      case TypeReqCodesPatch           => parse[ReqCodesPatch]
      case TypeReqFieldCustomTextSet   => parse[ReqFieldCustomTextSet]
      case TypeReqImplicationsPatch    => parse[ReqImplicationsPatch]
      case TypeReqsDelete              => parse[ReqsDelete]
      case TypeReqTagsPatch            => parse[ReqTagsPatch]
      case TypeSavedViewCreate         => parse[SavedViewCreate]
      case TypeSavedViewDefaultSet     => parse[SavedViewDefaultSet]
      case TypeSavedViewDelete         => parse[SavedViewDelete]
      case TypeSavedViewUpdate         => parse[SavedViewUpdate]
      case TypeTagDelete               => parse[TagDelete]
      case TypeTagGroupCreate          => parse[TagGroupCreate]
      case TypeTagGroupUpdate          => parse[TagGroupUpdate]
      case TypeTagRestore              => parse[TagRestore]
      case TypeUseCaseCreate           => parse[UseCaseCreate]
      case TypeUseCaseStepCreate       => parse[UseCaseStepCreate]
      case TypeUseCaseStepDelete       => parse[UseCaseStepDelete]
      case TypeUseCaseStepRestore      => parse[UseCaseStepRestore]
      case TypeUseCaseStepShiftLeft    => parse[UseCaseStepShiftLeft]
      case TypeUseCaseStepShiftRight   => parse[UseCaseStepShiftRight]
      case TypeUseCaseStepUpdate       => parse[UseCaseStepUpdate]
      case TypeUseCaseTitleSet         => parse[UseCaseTitleSet]
      case _                           => fail(s"No event registered to type: $typeId")
    }
  }
}
