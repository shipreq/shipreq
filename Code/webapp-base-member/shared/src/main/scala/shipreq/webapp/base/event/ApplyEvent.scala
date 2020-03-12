package shipreq.webapp.base.event

import nyaya.prop.LogicPropExt
import scalaz.{-\/, \/, \/-}
import shipreq.webapp.base.data.{DataProp, Project}
import ApplyEventLib._, SE.SE
import ApplyEvent.{Events, Result}

object ApplyEvent {
  type Result = String \/ Project
  type Events = TraversableOnce[Event]

  /**
   * Applies trusted events (i.e. events that have been verified previously and usually stored in the DB already).
   */
  val trusted = new ApplyEvent()(Trusted)

  /**
   * Applies untrusted events (i.e. new events created in response to a user request).
   */
  val untrusted = new ApplyEvent()(Untrusted)
}

final class ApplyEvent(implicit val trust: Trust)
    extends ApplyConfigEvent
       with ApplyContentEvent
       with ApplyOtherEvent {

  def apply(events: Events)(p: Project): Result =
    applyAllSafely(events) exec p

  def apply1(event: Event)(p: Project): Result =
    applyOneSafely(event) exec p

  private val validateDataProps: SE[Unit] =
    whenUntrusted {
      val prop = DataProp.project.allIncludingConfig
      SE.testO { p =>
        val e = prop(p)
        if (e.success)
          None
        else
          Some(e.report)
      }
    }

  def applyVerified(ves: TraversableOnce[VerifiedEvent])(p: Project): Result =
    if (ves.isEmpty)
      \/-(p)
    else
      applyAllSafely(ves.toIterator.map(_.event)).exec(p)

  private def safely(apply: SE[Unit]): SE[Unit] =
    (apply >> validateDataProps) attempt onError

  private def applyAllSafely(events: Events): SE[Unit] =
    safely(applyAllUnsafely(events))

  private def applyAllUnsafely(events: Events): SE[Unit] =
    SE.foldMapRun(events)(applyOneUnsafely)

  private def applyOneSafely(event: Event): SE[Unit] =
    safely(applyOneUnsafely(event))

  private def applyOneUnsafely(event: Event): SE[Unit] = {
    import Event._
    event match {
      case e: ApplicableTagCreate     => ApplicableTagEvents    applyCreate                e
      case e: ApplicableTagCreateV1   => ApplicableTagEventsV1  applyCreate                e
      case e: ApplicableTagUpdate     => ApplicableTagEvents    applyUpdate                e
      case e: ApplicableTagUpdateV1   => ApplicableTagEventsV1  applyUpdate                e
      case e: CodeGroupCreate         => CodeGroupEvents        applyCreate                e
      case e: CodeGroupsDelete        => CodeGroupEvents        applyDelete                e
      case e: CodeGroupUpdate         => CodeGroupEvents        applyUpdate                e
      case e: ContentRestore          => ContentCommon          applyRestoreContent        e
      case e: CustomIssueTypeCreate   => CustomIssueTypeEvents  applyCreate                e
      case e: CustomIssueTypeDelete   => CustomIssueTypeEvents  applyDelete                e
      case e: CustomIssueTypeRestore  => CustomIssueTypeEvents  applyRestore               e
      case e: CustomIssueTypeUpdate   => CustomIssueTypeEvents  applyUpdate                e
      case e: CustomReqTypeCreate     => CustomReqTypeEvents    applyCreate                e
      case e: CustomReqTypeDelete     => CustomReqTypeEvents    applyDelete                e
      case e: CustomReqTypeDeleteSoft => CustomReqTypeEvents    applySoftDelete            e
      case e: CustomReqTypeDeleteHard => CustomReqTypeEvents    applyHardDelete            e
      case e: CustomReqTypeRestore    => CustomReqTypeEvents    applyRestore               e
      case e: CustomReqTypeUpdate     => CustomReqTypeEvents    applyUpdate                e
      case e: FieldCustomDelete       => FieldEvents            applyCustomDelete          e
      case e: FieldCustomImpCreate    => CustomImpFieldEvents   applyCreate                e
      case e: FieldCustomImpUpdate    => CustomImpFieldEvents   applyUpdate                e
      case e: FieldCustomRestore      => FieldEvents            applyCustomRestore         e
      case e: FieldCustomTagCreate    => CustomTagFieldEvents   applyCreate                e
      case e: FieldCustomTagUpdate    => CustomTagFieldEvents   applyUpdate                e
      case e: FieldCustomTextCreate   => CustomTextFieldEvents  applyCreate                e
      case e: FieldCustomTextUpdate   => CustomTextFieldEvents  applyUpdate                e
      case e: FieldReposition         => FieldEvents            applyReposition            e
      case e: FieldStaticAdd          => FieldEvents            applyStaticAdd             e
      case e: FieldStaticRemove       => FieldEvents            applyStaticRemove          e
      case e: GenericReqCreate        => GenericReqEvents       applyGenericReqCreate      e
      case e: GenericReqTitleSet      => GenericReqEvents       applyGenericReqTitleSet    e
      case e: GenericReqTypeSet       => GenericReqEvents       applyGenericReqTypeSet     e
      case e: ManualIssueCreate       => ManualIssueEvents      applyCreate                e
      case e: ManualIssueDelete       => ManualIssueEvents      applyDelete                e
      case e: ManualIssueUpdate       => ManualIssueEvents      applyUpdate                e
      case e: ProjectNameSet          => OtherEvents            applyProjectNameSet        e
      case e: ReqCodesPatch           => ReqCodeLogic           applyReqCodesPatch         e
      case e: ReqFieldCustomTextSet   => ContentCommon          applyReqFieldCustomTextSet e
      case e: ReqImplicationsPatch    => ContentCommon          applyReqImplicationsPatch  e
      case e: ReqsDelete              => ContentCommon          applyDelete                e
      case e: ReqTagsPatch            => ContentCommon          applyReqTagsPatch          e
      case e: SavedViewCreate         => SavedViewEvents        applyCreate                e
      case e: SavedViewDefaultSet     => SavedViewEvents        applyDefaultSet            e
      case e: SavedViewDelete         => SavedViewEvents        applyDelete                e
      case e: SavedViewUpdate         => SavedViewEvents        applyUpdate                e
      case e: TagDelete               => TagEvents              applyDelete                e
      case e: TagGroupCreate          => TagGroupEvents         applyCreate                e
      case e: TagGroupUpdate          => TagGroupEvents         applyUpdate                e
      case e: TagRestore              => TagEvents              applyRestore               e
      case e: UseCaseCreate           => UseCaseEvents          applyCreate                e
      case e: UseCaseStepCreate       => UseCaseEvents          applyStepCreate            e
      case e: UseCaseStepDelete       => UseCaseEvents          applyStepDelete            e
      case e: UseCaseStepRestore      => UseCaseEvents          applyStepRestore           e
      case e: UseCaseStepShiftLeft    => UseCaseEvents          applyStepShiftLeft         e
      case e: UseCaseStepShiftRight   => UseCaseEvents          applyStepShiftRight        e
      case e: UseCaseStepUpdate       => UseCaseEvents          applyStepUpdate            e
      case e: UseCaseTitleSet         => UseCaseEvents          applyTitleSet              e
      case e: ProjectTemplateApply    => safely(applyAllUnsafely(e.template.events))
    }
  }

  private val onError: Throwable => String =
    e => {
      val msg = Option(e.getMessage).filter(_.nonEmpty)
      msg getOrElse s"Error occurred: $e"
    }

}
