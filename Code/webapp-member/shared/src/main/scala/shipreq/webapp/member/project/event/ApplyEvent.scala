package shipreq.webapp.member.project.event

import nyaya.prop.LogicPropExt
import shipreq.base.util.ErrorMsg
import shipreq.webapp.member.project.data.{DataProp, Project}
import shipreq.webapp.member.project.event.ApplyEventLib._

object ApplyEvent {
  type Result = ErrorMsg \/ Project
  type Events = IterableOnce[Event]

  /**
   * Applies trusted events (i.e. events that have been verified previously and usually stored in the DB already).
   */
  val trusted = new ApplyEvent()(Trusted)

  /**
   * Applies untrusted events (i.e. new events created in response to a user request).
   */
  val untrusted = new ApplyEvent()(Untrusted)

  private final val propertyMarker = "222e67c6-455b-4f09-ae0c-bac3f5a808f1"

  lazy val properties =
    DataProp.project.allIncludingConfig
      .rename("ApplyEvent.properties" + propertyMarker)

  def propertyFailure(err: ErrorMsg): Option[ErrorMsg] =
    Option.when(err.value.contains(propertyMarker))(
      ErrorMsg(err.value.replace(propertyMarker, "")))
}

final class ApplyEvent(implicit val trust: Trust)
    extends ApplyConfigEvent
       with ApplyContentEvent
       with ApplyReqCodeLogic
       with ApplyOtherEvent {

  import ApplyEvent.{Events, Result}

  def applyUnverified(events: Events)(p: Project): Result =
    safelyApplyUnverified(events).exec(p)

  def applyUnverified1(event: Event)(p: Project): Result =
    safelyApplyUnverified1(event).exec(p)

  def apply(ves: VerifiedEvent.Seq)(p: Project): Result =
    if (ves.isEmpty)
      \/-(p)
    else
      apply(VerifiedEvent.NonEmptySeq.force(ves))(p)

  def apply(ves: VerifiedEvent.NonEmptySeq)(p: Project): Result =
    safelyApplyUnverified(ves.iterator.map(_.event)).exec(p) match {
      case ok @ \/-(_) => ok
      case -\/(_)      => Eval.foldMapRun(ves)(safelyApply1).exec(p)
    }

  def apply1(event: VerifiedEvent)(p: Project): Result =
    safelyApply1(event).exec(p)

  // ===================================================================================================================
  // Safe

  private val validateDataProps: Eval[Unit] =
    whenUntrusted {
      val prop = ApplyEvent.properties
      Eval.failOptions { p =>
        val e = prop(p)
        if (e.success)
          None
        else
          Some(e.report)
      }
    }

  private val safely: Eval[Unit] => Eval[Unit] = {
    val onError: Throwable => ErrorMsg =
      e => {
        val msg = Option(e.getMessage).filter(_.nonEmpty)
        ErrorMsg(msg getOrElse s"Error occurred: $e")
      }
    i => (i >> validateDataProps).catchErrors(onError)
  }

  private def safelyApplyUnverified(events: Events): Eval[Unit] =
    safely(unsafelyApply(events))

  private def safelyApplyUnverified1(event: Event): Eval[Unit] =
    safely(unsafelyApply1(event))

  private def safelyApply1(ve: VerifiedEvent): Eval[Unit] = {
    val onFailure: ErrorMsg => Eval[Unit] =
      err => Eval.fail(err.withPrefix(s"[#${ve.ord.value}] "))
    safelyApplyUnverified1(ve.event).handleFailure(onFailure)
  }

  // ===================================================================================================================
  // Unsafe

  private def unsafelyApply(events: Events): Eval[Unit] =
    // events.iterator.foldLeft(Eval.unit)((q, e) => q >> Eval.restack(unsafelyApply1(e)))
    Eval.foldMapRun(events)(unsafelyApply1)

  private def unsafelyApply1(event: Event): Eval[Unit] = {
    import Event._
    event match {
      case e: ApplicableTagCreate     => ApplicableTagEvents     applyCreate                e
      case e: ApplicableTagCreateV1   => ApplicableTagEventsV1   applyCreate                e
      case e: ApplicableTagUpdate     => ApplicableTagEvents     applyUpdate                e
      case e: ApplicableTagUpdateV1   => ApplicableTagEventsV1   applyUpdate                e
      case e: CodeGroupCreate         => CodeGroupEvents         applyCreate                e
      case e: CodeGroupsDelete        => CodeGroupEvents         applyDelete                e
      case e: CodeGroupUpdate         => CodeGroupEvents         applyUpdate                e
      case e: ContentRestore          => ContentCommon           applyRestoreContent        e
      case e: CustomIssueTypeCreate   => CustomIssueTypeEvents   applyCreate                e
      case e: CustomIssueTypeDelete   => CustomIssueTypeEvents   applyDelete                e
      case e: CustomIssueTypeRestore  => CustomIssueTypeEvents   applyRestore               e
      case e: CustomIssueTypeUpdate   => CustomIssueTypeEvents   applyUpdate                e
      case e: CustomReqTypeCreate     => CustomReqTypeEvents     applyCreate                e
      case e: CustomReqTypeCreateV1   => CustomReqTypeEventsV1   applyCreate                e
      case e: CustomReqTypeDelete     => CustomReqTypeEventsV1   applyDelete                e
      case e: CustomReqTypeDeleteSoft => CustomReqTypeEvents     applySoftDelete            e
      case e: CustomReqTypeDeleteHard => CustomReqTypeEvents     applyHardDelete            e
      case e: CustomReqTypeRestore    => CustomReqTypeEvents     applyRestore               e
      case e: CustomReqTypeUpdate     => CustomReqTypeEvents     applyUpdate                e
      case e: CustomReqTypeUpdateV1   => CustomReqTypeEventsV1   applyUpdate                e
      case e: FieldCustomDelete       => FieldEvents             applyCustomDelete          e
      case e: FieldCustomImpCreateV1  => CustomImpFieldEventsV1  applyCreate                e
      case e: FieldCustomImpCreate    => CustomImpFieldEvents    applyCreate                e
      case e: FieldCustomImpUpdateV1  => CustomImpFieldEventsV1  applyUpdate                e
      case e: FieldCustomImpUpdate    => CustomImpFieldEvents    applyUpdate                e
      case e: FieldCustomRestore      => FieldEvents             applyCustomRestore         e
      case e: FieldCustomTagCreateV1  => CustomTagFieldEventsV1  applyCreate                e
      case e: FieldCustomTagCreate    => CustomTagFieldEvents    applyCreate                e
      case e: FieldCustomTagUpdateV1  => CustomTagFieldEventsV1  applyUpdate                e
      case e: FieldCustomTagUpdate    => CustomTagFieldEvents    applyUpdate                e
      case e: FieldCustomTextCreateV1 => CustomTextFieldEventsV1 applyCreate                e
      case e: FieldCustomTextCreate   => CustomTextFieldEvents   applyCreate                e
      case e: FieldCustomTextUpdateV1 => CustomTextFieldEventsV1 applyUpdate                e
      case e: FieldCustomTextUpdate   => CustomTextFieldEvents   applyUpdate                e
      case e: FieldReposition         => FieldEvents             applyReposition            e
      case e: FieldStaticAdd          => FieldEvents             applyStaticAdd             e
      case e: FieldStaticRemove       => FieldEvents             applyStaticRemove          e
      case e: GenericReqCreate        => GenericReqEvents        applyGenericReqCreate      e
      case e: GenericReqTitleSet      => GenericReqEvents        applyGenericReqTitleSet    e
      case e: GenericReqTypeSet       => GenericReqEvents        applyGenericReqTypeSet     e
      case e: ManualIssueCreate       => ManualIssueEvents       applyCreate                e
      case e: ManualIssueDelete       => ManualIssueEvents       applyDelete                e
      case e: ManualIssueUpdate       => ManualIssueEvents       applyUpdate                e
      case e: ProjectNameSet          => OtherEvents             applyProjectNameSet        e
      case e: ReqCodesPatch           => ReqCodeLogic            applyReqCodesPatch         e
      case e: ReqFieldCustomTextSet   => ContentCommon           applyReqFieldCustomTextSet e
      case e: ReqImplicationsPatch    => ContentCommon           applyReqImplicationsPatch  e
      case e: ReqsDelete              => ContentCommon           applyDelete                e
      case e: ReqTagsPatch            => ContentCommon           applyReqTagsPatch          e
      case e: SavedViewCreateV1       => SavedViewEvents         applyCreate                e
      case e: SavedViewCreate         => SavedViewEvents         applyCreate                e
      case e: SavedViewDefaultSet     => SavedViewEvents         applyDefaultSet            e
      case e: SavedViewDelete         => SavedViewEvents         applyDelete                e
      case e: SavedViewUpdateV1       => SavedViewEvents         applyUpdate                e
      case e: SavedViewUpdate         => SavedViewEvents         applyUpdate                e
      case e: TagDelete               => TagEvents               applyDelete                e
      case e: TagGroupCreate          => TagGroupEvents          applyCreate                e
      case e: TagGroupUpdate          => TagGroupEvents          applyUpdate                e
      case e: TagRestore              => TagEvents               applyRestore               e
      case e: UseCaseCreate           => UseCaseEvents           applyCreate                e
      case e: UseCaseStepCreate       => UseCaseEvents           applyStepCreate            e
      case e: UseCaseStepDelete       => UseCaseEvents           applyStepDelete            e
      case e: UseCaseStepRestore      => UseCaseEvents           applyStepRestore           e
      case e: UseCaseStepShiftLeft    => UseCaseEvents           applyStepShiftLeft         e
      case e: UseCaseStepShiftRight   => UseCaseEvents           applyStepShiftRight        e
      case e: UseCaseStepUpdate       => UseCaseEvents           applyStepUpdate            e
      case e: UseCaseTitleSet         => UseCaseEvents           applyTitleSet              e
      case e: ProjectTemplateApply    => safelyApplyUnverified(e.template.events)
    }
  }
}
