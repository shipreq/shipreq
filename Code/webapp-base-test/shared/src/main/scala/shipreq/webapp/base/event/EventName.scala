package shipreq.webapp.base.event

import japgolly.microlibs.adt_macros.AdtMacros._
import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.{StaticLookupFn, Utils}
import shipreq.webapp.base.event.Event._

final case class EventName private[EventName] (value: String)

object EventName {

  implicit def univEq: UnivEq[EventName] = UnivEq.derive
  implicit val ordering: Ordering[EventName] = Ordering.by(_.value)

  // Note: backward-compatibility of these event name values needn't be maintained
  private val data =
    valuesForAdtF[Event, EventName] {
      case _: ApplicableTagCreate     => new EventName("ApplicableTagCreate")
      case _: ApplicableTagCreateV1   => new EventName("ApplicableTagCreateV1")
      case _: ApplicableTagUpdate     => new EventName("ApplicableTagUpdate")
      case _: ApplicableTagUpdateV1   => new EventName("ApplicableTagUpdateV1")
      case _: ContentRestore          => new EventName("ContentRestore")
      case _: CustomIssueTypeCreate   => new EventName("CustomIssueTypeCreate")
      case _: CustomIssueTypeDelete   => new EventName("CustomIssueTypeDelete")
      case _: CustomIssueTypeRestore  => new EventName("CustomIssueTypeRestore")
      case _: CustomIssueTypeUpdate   => new EventName("CustomIssueTypeUpdate")
      case _: CustomReqTypeCreate     => new EventName("CustomReqTypeCreate")
      case _: CustomReqTypeCreateV1   => new EventName("CustomReqTypeCreateV1")
      case _: CustomReqTypeDelete     => new EventName("CustomReqTypeDelete")
      case _: CustomReqTypeDeleteHard => new EventName("CustomReqTypeDeleteHard")
      case _: CustomReqTypeDeleteSoft => new EventName("CustomReqTypeDeleteSoft")
      case _: CustomReqTypeRestore    => new EventName("CustomReqTypeRestore")
      case _: CustomReqTypeUpdate     => new EventName("CustomReqTypeUpdate")
      case _: CustomReqTypeUpdateV1   => new EventName("CustomReqTypeUpdateV1")
      case _: FieldCustomDelete       => new EventName("FieldCustomDelete")
      case _: FieldCustomImpCreateV1  => new EventName("FieldCustomImpCreateV1")
      case _: FieldCustomImpCreate    => new EventName("FieldCustomImpCreate")
      case _: FieldCustomImpUpdateV1  => new EventName("FieldCustomImpUpdateV1")
      case _: FieldCustomImpUpdate    => new EventName("FieldCustomImpUpdate")
      case _: FieldCustomRestore      => new EventName("FieldCustomRestore")
      case _: FieldCustomTagCreateV1  => new EventName("FieldCustomTagCreateV1")
      case _: FieldCustomTagCreate    => new EventName("FieldCustomTagCreate")
      case _: FieldCustomTagUpdateV1  => new EventName("FieldCustomTagUpdateV1")
      case _: FieldCustomTagUpdate    => new EventName("FieldCustomTagUpdate")
      case _: FieldCustomTextCreateV1 => new EventName("FieldCustomTextCreateV1")
      case _: FieldCustomTextCreate   => new EventName("FieldCustomTextCreate")
      case _: FieldCustomTextUpdateV1 => new EventName("FieldCustomTextUpdateV1")
      case _: FieldCustomTextUpdate   => new EventName("FieldCustomTextUpdate")
      case _: FieldReposition         => new EventName("FieldReposition")
      case _: FieldStaticAdd          => new EventName("FieldStaticAdd")
      case _: FieldStaticRemove       => new EventName("FieldStaticRemove")
      case _: GenericReqCreate        => new EventName("GenericReqCreate")
      case _: GenericReqTitleSet      => new EventName("GenericReqTitleSet")
      case _: GenericReqTypeSet       => new EventName("GenericReqTypeSet")
      case _: ManualIssueCreate       => new EventName("ManualIssueCreate")
      case _: ManualIssueDelete       => new EventName("ManualIssueDelete")
      case _: ManualIssueUpdate       => new EventName("ManualIssueUpdate")
      case _: ProjectNameSet          => new EventName("ProjectNameSet")
      case _: ProjectTemplateApply    => new EventName("ProjectTemplateApply")
      case _: CodeGroupCreate         => new EventName("CodeGroupCreate")
      case _: CodeGroupsDelete        => new EventName("CodeGroupsDelete")
      case _: CodeGroupUpdate         => new EventName("CodeGroupUpdate")
      case _: ReqCodesPatch           => new EventName("ReqCodesPatch")
      case _: ReqFieldCustomTextSet   => new EventName("ReqFieldCustomTextSet")
      case _: ReqImplicationsPatch    => new EventName("ReqImplicationsPatch")
      case _: ReqsDelete              => new EventName("ReqsDelete")
      case _: ReqTagsPatch            => new EventName("ReqTagsPatch")
      case _: SavedViewCreateV1       => new EventName("SavedViewCreateV1")
      case _: SavedViewCreate         => new EventName("SavedViewCreate")
      case _: SavedViewDefaultSet     => new EventName("SavedViewDefaultSet")
      case _: SavedViewDelete         => new EventName("SavedViewDelete")
      case _: SavedViewUpdateV1       => new EventName("SavedViewUpdateV1")
      case _: SavedViewUpdate         => new EventName("SavedViewUpdate")
      case _: TagDelete               => new EventName("TagDelete")
      case _: TagGroupCreate          => new EventName("TagGroupCreate")
      case _: TagGroupUpdate          => new EventName("TagGroupUpdate")
      case _: TagRestore              => new EventName("TagRestore")
      case _: UseCaseCreate           => new EventName("UseCaseCreate")
      case _: UseCaseStepCreate       => new EventName("UseCaseStepCreate")
      case _: UseCaseStepDelete       => new EventName("UseCaseStepDelete")
      case _: UseCaseStepRestore      => new EventName("UseCaseStepRestore")
      case _: UseCaseStepShiftLeft    => new EventName("UseCaseStepShiftLeft")
      case _: UseCaseStepShiftRight   => new EventName("UseCaseStepShiftRight")
      case _: UseCaseStepUpdate       => new EventName("UseCaseStepUpdate")
      case _: UseCaseTitleSet         => new EventName("UseCaseTitleSet")
    }
    .map1(_.sorted)

  private def dups = Utils.dups(data._1.iterator.map(_.value)).toSet
  assert(dups.isEmpty, s"Duplicate names detected: $dups")

  val all: NonEmptySet[EventName] =
    data._1.toNES

  val allList: List[EventName] =
    data._1.whole.toList

  val size: Int =
    data._1.length

  private val fromString: String => EventName =
    StaticLookupFn.useMapBy(allList)(_.value).total

  def apply(s: String): EventName =
    fromString(s)

  def apply(e: Event): EventName =
    data._2(e)

  val maxNameLen: Int =
    allList.iterator.map(_.value.length).max
}
