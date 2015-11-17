package shipreq.webapp.base.event

import nyaya.prop.LogicPropExt
import scala.annotation.tailrec
import scalaz.{-\/, \/-, \/}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{univEqOps, NonEmptyVector, Valid, UnivEq}
import shipreq.webapp.base.data.{Project, DataProp}
import shipreq.webapp.base.hash.HashRec
import ApplyEventLib._, SE.SE
import ApplyEvent.{Events, Result}

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

    implicit def equality: UnivEq[LogicVer] = UnivEq.derive
  }
}

final class ApplyEvent(implicit val trust: Trust) extends ApplyConfigEvent with ApplyContentEvent {

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

  def applyVerified(ves: Iterable[VerifiedEvent])(p: Project): Result =
    if (ves.isEmpty)
      \/-(p)
    else {
      // debug(ves, p)
      val events          = ves.map(_.event)(collection.breakOut): List[Event]
      val initialHashRecs = HashRec(p)
      val finalHashRecs   = ves.iterator.map(_.hashRecs).foldLeft(initialHashRecs)(HashRec.merge)
      val plan            = applyAllSafe(events) >> validateHashRecs(finalHashRecs)

      plan.exec(p).leftMap(bulkError =>
        findFirstFailure(p, ves) match {
          case \/-(msg) => msg
          case -\/(p2)  => s"Bulk validation failed but incremental passed.\n$bulkError"
        })
    }

  private def validateHashRecs(recs: HashRec.Collection): SE[Unit] =
    SE.testO(p =>
      if (recs.forall(_.validate(p) :: Valid))
        None
      else {
        val failures = recs.iterator
          .map(r => (r, r validateF p))
          .filterDefined_2
          .map { case (r, f) => s"$r failed: ${f.msg}" }
          .toVector
          .sorted
        Some(s"Hash Mismatch. ${failures.size} mismatches:${failures.map("\n  - " + _) mkString ""}")
      }
    )

  private def findFirstFailure(p: Project, ves: Iterable[VerifiedEvent]): Project \/ String = {
    val it = ves.iterator

    @tailrec
    def go(index: Int, p: Project, lastHR: HashRec.Collection): Project \/ String =
      if (it.hasNext) {
        val h    = it.next()
        val hr   = HashRec.merge(lastHR, h.hashRecs)
        val plan = apply1Safe(h.event) >> validateHashRecs(hr)

        plan exec p match {
          case \/-(p2)  => go(index + 1, p2, hr)
          case -\/(err) => \/-(s"$err\nEvent #$index = ${h.event}")
        }
      } else
        -\/(p)

    go(0, p, HashRec(p))
  }

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
      case e: CreateCustomIssueType => CustomIssueTypeEvents  applyCreate                e
      case e: UpdateCustomIssueType => CustomIssueTypeEvents  applyUpdate                e
      case e: DeleteCustomIssueType => CustomIssueTypeEvents  applyDelete                e
      case e: CreateCustomReqType   => CustomReqTypeEvents    applyCreate                e
      case e: UpdateCustomReqType   => CustomReqTypeEvents    applyUpdate                e
      case e: DeleteCustomReqType   => CustomReqTypeEvents    applyDelete                e
      case e: CreateApplicableTag   => ApplicableTagEvents    applyCreate                e
      case e: UpdateApplicableTag   => ApplicableTagEvents    applyUpdate                e
      case e: CreateTagGroup        => TagGroupEvents         applyCreate                e
      case e: UpdateTagGroup        => TagGroupEvents         applyUpdate                e
      case e: DeleteTag             => TagEvents              applyDelete                e
      case e: CreateCustomTextField => CustomTextFieldEvents  applyCreate                e
      case e: UpdateCustomTextField => CustomTextFieldEvents  applyUpdate                e
      case e: CreateCustomTagField  => CustomTagFieldEvents   applyCreate                e
      case e: UpdateCustomTagField  => CustomTagFieldEvents   applyUpdate                e
      case e: CreateCustomImpField  => CustomImpFieldEvents   applyCreate                e
      case e: UpdateCustomImpField  => CustomImpFieldEvents   applyUpdate                e
      case e: DeleteCustomField     => FieldEvents            applyDeleteCF              e
      case e: DeleteStaticField     => FieldEvents            applyDeleteSF              e
      case e: AddStaticField        => FieldEvents            applyAddStaticField        e
      case e: RepositionField       => FieldEvents            applyReposition            e
      case e: PatchReqTags          => ContentCommon          applyPatchTags             e
      case e: PatchImplicationSrc   => ContentCommon          applyPatchImplicationSrc   e
      case e: PatchImplicationTgt   => ContentCommon          applyPatchImplicationTgt   e
      case e: SetCustomTextField    => ContentCommon          applySetCustomTextField    e
      case e: DeleteReqs            => ContentCommon          applyDelete                e
      case e: RestoreContent        => ContentCommon          applyRestoreContent        e
      case e: CreateGenericReq      => GenericReqEvents       applyCreateGenericReq      e
      case e: SetGenericReqTitle    => GenericReqEvents       applySetGenericReqTitle    e
      case e: SetGenericReqType     => GenericReqEvents       applySetGenericReqType     e
      case e: SetUseCaseTitle       => UseCaseEvents          applySetUseCaseTitle       e
      case e: CreateUseCase         => UseCaseEvents          applyCreateUseCase         e
      case e: AddUseCaseStep        => UseCaseEvents          applyAddUseCaseStep        e
      case e: ShiftUseCaseStepLeft  => UseCaseEvents          applyShiftUseCaseStepLeft  e
      case e: ShiftUseCaseStepRight => UseCaseEvents          applyShiftUseCaseStepRight e
      case e: UpdateUseCaseStep     => UseCaseEvents          applyUpdateUseCaseStep     e
      case e: DeleteUseCaseStep     => UseCaseEvents          applyDeleteUseCaseStep     e
      case e: PatchReqCodes         => ReqCodeLogic           applyPatchReqCodes         e
      case e: CreateReqCodeGroup    => ReqCodeGroupEvents     applyCreate                e
      case e: UpdateReqCodeGroup    => ReqCodeGroupEvents     applyUpdate                e
      case e: DeleteReqCodeGroups   => ReqCodeGroupEvents     applyDelete                e
      case e: ApplyTemplate         => safely(applyAllUnsafe(e.t.events))
    }

  private def apply1Safe(event: Event): SE[Unit] =
    safely(apply1Unsafe(event))

  private def applyAllUnsafe(events: Events): SE[Unit] =
    SE.foldMapRun(events)(apply1Unsafe)

  private def applyAllSafe(events: Events): SE[Unit] =
    safely(applyAllUnsafe(events))
}
