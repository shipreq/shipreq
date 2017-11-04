package shipreq.webapp.base.event

import japgolly.microlibs.nonempty.NonEmptyVector
import nyaya.prop.LogicPropExt
import scala.annotation.tailrec
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.Valid
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{DataProp, Project}
import shipreq.webapp.base.hash._
import ApplyEventLib._, SE.SE
import ApplyEvent.{Events, Result, eventBatcher}

object ApplyEvent {
  type Result = String \/ Project
  type Events = Iterable[Event]

  /**
   * Applies trusted events (i.e. events that have been verified previously and usually stored in the DB already).
   */
  val trusted = new ApplyEvent()(Trusted)

  /**
   * Applies untrusted events (i.e. new events created in response to a user request).
   */
  val untrusted = new ApplyEvent()(Untrusted)

  val eventBatcher: ProjectHashModule.Batcher[VerifiedEvent, Event] =
    ProjectHashModule.Batcher(_.event, _.hashRecs)

  case class LogicVer(value: Char) extends AnyVal {
    def isCurrent: Boolean =
      value ==* LogicVer.Current.value
  }

  object LogicVer {

    // When this first changes.
    //   1) Update BinCodecEvents & make it stop using ConstPickler.
    //   2) Update RandomData.
    val all: NonEmptyVector[LogicVer] =
      NonEmptyVector one LogicVer('1')

    /**
     * When logic changes in a way that breaks backwards-compatibility this value must be changed to a new value.
     *
     * Doing so allows logic bugfixes or improvements to be made here without breaking the ability to load and apply
     * events created previously. Application logic can use this value to identify and ignore data integrity checks where
     * discrepancy is expected and desired.
     */
    val Current: LogicVer = all.last

    assert(all.length == 1, "If you're gonna actually use multiple logicVer, then remove LogicVer.SoleInstance")
    def SoleInstance = Current

    implicit def equality: UnivEq[LogicVer] = UnivEq.derive
  }
}

final class ApplyEvent(implicit val trust: Trust)
    extends ApplyConfigEvent
       with ApplyContentEvent
       with ApplyOtherEvent {

  def apply(events: Events)(p: Project): Result =
    applyAllSafe(events) exec p

  def apply1(event: Event)(p: Project): Result =
    apply1Safe(event) exec p

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

  def applyVerified(ves: Vector[VerifiedEvent])(p: Project): Result =
    if (ves.isEmpty)
      \/-(p)
    else {
      // debug(ves, p)

      val plan: SE[Unit] =
        applyEventBatches(eventBatcher optimal ves)
          .improveFailure(applyEventBatches(eventBatcher oneByOne ves)) {
            case (_, -\/(e)) => e
            case (e, \/-(_)) => s"Batch application failed but incremental application passed (!)\n$e"
          }

      plan.exec(p)
    }

  private def applyEventBatches(batches: eventBatcher.Batches): SE[Unit] =
    SE.foldMapRun(batches)(applyEventBatch)

  private val applyEventBatch: eventBatcher.Batch => SE[Unit] =
    eb =>
      SE.get.flatMap(p1 =>
        applyAllSafe(eb.elements) >> SE.testO { p2 =>
          val errs = HashLogic.validate(eb.recs, before = p1, current = p2)
          if (errs.isEmpty)
            None
          else Some {
            val each = errs.map("* " + _.msg).mkString("\n")
            val events = eb.elements.map("* " + _).mkString("\n")
            s"Hash Discrepancy:\n$each\nEvents:\n$events"
          }
        }
      )

  /*
  private def debug(ves: Iterable[VerifiedEvent], p0: Project): Unit = {
    println("=" * 120)

    def printHashRecs(hrs: TraversableOnce[HashRec]): Unit = {
      for (r <- hrs) println("  - " + r)
      println()
    }

    def printProjectHashes(p: Project): Unit = {
      println("Hashes:")
      printHashRecs(HashRec(p))
    }

    val it = ves.iterator

    @tailrec
    def go(p: Project): Unit =
      if (it.hasNext) {
        val ve = it.next()
        println("Applying event: " + ve.event)
        printHashRecs(ve.hashRecs)
        apply1(ve.event)(p) match {
          case \/-(p2) => printProjectHashes(p2); go(p2)
          case -\/(e) => println(e)
        }
      }

    go(p0)
  }
  */

  private def safely(apply: SE[Unit]): SE[Unit] =
    (apply >> validateDataProps) attempt onError

  private val onError: Throwable => String =
    e => {
      val msg = Option(e.getMessage).filter(_.nonEmpty)
      msg getOrElse s"Error occurred: $e"
    }

  private def apply1Unsafe(event: Event): SE[Unit] =
    event match {
      case e: ApplicableTagCreate    => ApplicableTagEvents    applyCreate                e
      case e: ApplicableTagUpdate    => ApplicableTagEvents    applyUpdate                e
      case e: ContentRestore         => ContentCommon          applyRestoreContent        e
      case e: CustomIssueTypeCreate  => CustomIssueTypeEvents  applyCreate                e
      case e: CustomIssueTypeDelete  => CustomIssueTypeEvents  applyDelete                e
      case e: CustomIssueTypeRestore => CustomIssueTypeEvents  applyRestore               e
      case e: CustomIssueTypeUpdate  => CustomIssueTypeEvents  applyUpdate                e
      case e: CustomReqTypeCreate    => CustomReqTypeEvents    applyCreate                e
      case e: CustomReqTypeDelete    => CustomReqTypeEvents    applyDelete                e
      case e: CustomReqTypeRestore   => CustomReqTypeEvents    applyRestore               e
      case e: CustomReqTypeUpdate    => CustomReqTypeEvents    applyUpdate                e
      case e: FieldCustomDelete      => FieldEvents            applyCustomDelete          e
      case e: FieldCustomImpCreate   => CustomImpFieldEvents   applyCreate                e
      case e: FieldCustomImpUpdate   => CustomImpFieldEvents   applyUpdate                e
      case e: FieldCustomRestore     => FieldEvents            applyCustomRestore         e
      case e: FieldCustomTagCreate   => CustomTagFieldEvents   applyCreate                e
      case e: FieldCustomTagUpdate   => CustomTagFieldEvents   applyUpdate                e
      case e: FieldCustomTextCreate  => CustomTextFieldEvents  applyCreate                e
      case e: FieldCustomTextUpdate  => CustomTextFieldEvents  applyUpdate                e
      case e: FieldReposition        => FieldEvents            applyReposition            e
      case e: FieldStaticAdd         => FieldEvents            applyStaticAdd             e
      case e: FieldStaticRemove      => FieldEvents            applyStaticRemove          e
      case e: GenericReqCreate       => GenericReqEvents       applyGenericReqCreate      e
      case e: GenericReqTitleSet     => GenericReqEvents       applyGenericReqTitleSet    e
      case e: GenericReqTypeSet      => GenericReqEvents       applyGenericReqTypeSet     e
      case e: ProjectNameSet         => OtherEvents            applyProjectNameSet        e
      case e: CodeGroupCreate        => CodeGroupEvents        applyCreate                e
      case e: CodeGroupsDelete       => CodeGroupEvents        applyDelete                e
      case e: CodeGroupUpdate        => CodeGroupEvents        applyUpdate                e
      case e: ReqCodesPatch          => ReqCodeLogic           applyReqCodesPatch         e
      case e: ReqFieldCustomTextSet  => ContentCommon          applyReqFieldCustomTextSet e
      case e: ReqImplicationsPatch   => ContentCommon          applyReqImplicationsPatch  e
      case e: ReqsDelete             => ContentCommon          applyDelete                e
      case e: ReqTagsPatch           => ContentCommon          applyReqTagsPatch          e
      case e: SavedViewCreate        => SavedViewEvents        applyCreate                e
      case e: SavedViewDefaultSet    => SavedViewEvents        applyDefaultSet            e
      case e: SavedViewDelete        => SavedViewEvents        applyDelete                e
      case e: SavedViewUpdate        => SavedViewEvents        applyUpdate                e
      case e: TagDelete              => TagEvents              applyDelete                e
      case e: TagGroupCreate         => TagGroupEvents         applyCreate                e
      case e: TagGroupUpdate         => TagGroupEvents         applyUpdate                e
      case e: TagRestore             => TagEvents              applyRestore               e
      case e: UseCaseCreate          => UseCaseEvents          applyCreate                e
      case e: UseCaseStepCreate      => UseCaseEvents          applyStepCreate            e
      case e: UseCaseStepDelete      => UseCaseEvents          applyStepDelete            e
      case e: UseCaseStepRestore     => UseCaseEvents          applyStepRestore           e
      case e: UseCaseStepShiftLeft   => UseCaseEvents          applyStepShiftLeft         e
      case e: UseCaseStepShiftRight  => UseCaseEvents          applyStepShiftRight        e
      case e: UseCaseStepUpdate      => UseCaseEvents          applyStepUpdate            e
      case e: UseCaseTitleSet        => UseCaseEvents          applyTitleSet              e
      case e: ProjectTemplateApply   => safely(applyAllUnsafe(e.template.events))
    }

  private def apply1Safe(event: Event): SE[Unit] =
    safely(apply1Unsafe(event))

  private def applyAllUnsafe(events: Events): SE[Unit] =
    SE.foldMapRun(events)(apply1Unsafe)

  private def applyAllSafe(events: Events): SE[Unit] =
    safely(applyAllUnsafe(events))
}
