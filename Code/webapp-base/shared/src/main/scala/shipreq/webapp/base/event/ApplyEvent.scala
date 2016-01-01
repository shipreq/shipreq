package shipreq.webapp.base.event

import nyaya.prop.LogicPropExt
import scala.annotation.tailrec
import scalaz.{-\/, \/-, \/}
import shipreq.base.util.{Valid, UnivEq}
import shipreq.webapp.base.data.{Project, DataProp}
import shipreq.webapp.base.hash.HashRec
import ApplyEventLib._, SE.SE

object ApplyEvent {

  /**
   * Applies trusted events (i.e. events that have been verified previously and usually stored in the DB already).
   */
  val trusted = new ApplyEvent()(Trusted)

  /**
   * Applies untrusted events (i.e. new events created in response to a user request).
   */
  val untrusted = new ApplyEvent()(Untrusted)

  case class LogicVer(value: Char) extends AnyVal
  object LogicVer {
    /**
     * When logic changes in a way that breaks backwards-compatibility this value must be changed to a new value.
     *
     * Doing so allows logic bugfixes or improvements to be made here without breaking the ability to load and apply
     * events created previously. Application logic can use this value to identify and ignore data integrity checks where
     * discrepancy is expected and desired.
     */
    final val Current = LogicVer('1')
    // When this ↑ first changes.
    //   1) Update HashRec.merge to handle it.
    //   2) Update BinCodecEvents & make it stop using ConstPickler.
    //   3) Update RandomData.

    implicit def equality: UnivEq[LogicVer] = UnivEq.derive
  }
}

final class ApplyEvent(implicit val trust: Trust) extends ApplyConfigEvent with ApplyContentEvent {
  type Result = String \/ Project
  type Events = Iterable[Event]

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

  def applyVerified(ves: Traversable[VerifiedEvent])(p: Project): Result =
    if (ves.isEmpty)
      \/-(p)
    else {
      val s = ves.toStream
      val events = s.map(_.event)
      val finalHashRecs = s.map(_.hashRecs) reduceLeft HashRec.merge
      val plan = applyAllSafe(events) >> validateHashRecs(finalHashRecs)
      plan exec p leftMap (bulkError =>
        findFirstFailure(0, s, p, None) getOrElse
          s"Bulk validation failed but incremental passed.\n$bulkError"
      )
    }

  private def validateHashRecs(recs: HashRec.Collection): SE[Unit] =
    SE.testO(p =>
      if (recs.forall(_.validate(p) :: Valid))
        None
      else {
        val failures = recs.iterator
          .map(r => (r, r validateF p))
          .filter(_._2.isDefined)
          .map { t =>
            val r = t._1
            val f = t._2.get
            s"$r failed: ${f.msg}"
          }
          .toVector
          .sorted
        Some(s"Hash Mismatch. ${failures.size} mismatches:${failures.map("\n  - " + _) mkString ""}")
      }
    )

  @tailrec
  private def findFirstFailure(index: Int, ves: Stream[VerifiedEvent], p: Project, lastHR: Option[HashRec.Collection]): Option[String] =
    if (ves.isEmpty)
      None
    else {
      val h = ves.head
      val hr = lastHR.fold(h.hashRecs)(HashRec.merge(_, h.hashRecs))
      val plan = apply1Safe(h.event) >> validateHashRecs(hr)
      plan exec p match {
        case \/-(p2)  => findFirstFailure(index + 1, ves.tail, p2, Some(hr))
        case -\/(err) => Some(s"$err\nEvent #$index = ${h.event}")
      }
    }

  private def safely(apply: SE[Unit]): SE[Unit] =
    (apply >> validateDataProps) attempt onError

  private val onError: Throwable => String =
    e => {
      val msg = Option(e.getMessage).filter(_.nonEmpty)
      msg getOrElse s"Error occurred: $e"
    }

  private def apply1Unsafe(event: Event): SE[Unit] =
    event match {
      case e: CreateCustomIssueType => CustomIssueTypeEvents applyCreate e
      case e: UpdateCustomIssueType => CustomIssueTypeEvents applyUpdate e
      case e: DeleteCustomIssueType => CustomIssueTypeEvents applyDelete e

      case e: CreateCustomReqType => CustomReqTypeEvents applyCreate e
      case e: UpdateCustomReqType => CustomReqTypeEvents applyUpdate e
      case e: DeleteCustomReqType => CustomReqTypeEvents applyDelete e

      case e: CreateApplicableTag => ApplicableTagEvents applyCreate e
      case e: UpdateApplicableTag => ApplicableTagEvents applyUpdate e
      case e: CreateTagGroup      => TagGroupEvents      applyCreate e
      case e: UpdateTagGroup      => TagGroupEvents      applyUpdate e
      case e: DeleteTag           => TagEvents           applyDelete e

      case e: CreateCustomTextField => CustomTextFieldEvents applyCreate e
      case e: UpdateCustomTextField => CustomTextFieldEvents applyUpdate e
      case e: CreateCustomTagField  => CustomTagFieldEvents  applyCreate e
      case e: UpdateCustomTagField  => CustomTagFieldEvents  applyUpdate e
      case e: CreateCustomImpField  => CustomImpFieldEvents  applyCreate e
      case e: UpdateCustomImpField  => CustomImpFieldEvents  applyUpdate e
      case e: DeleteCustomField     => FieldEvents           applyDeleteCF e
      case e: DeleteStaticField     => FieldEvents           applyDeleteSF e
      case e: AddStaticField        => FieldEvents           applyAddStaticField e
      case e: RepositionField       => FieldEvents           applyReposition e

      case e: CreateGenericReq    => ReqEvents    createGeneric            e
      case e: PatchReqCodes       => ReqCodeLogic applyPatchReqCodes       e
      case e: PatchReqTags        => ReqEvents    applyPatchTags           e
      case e: PatchImplicationSrc => ReqEvents    applyPatchImplicationSrc e
      case e: PatchImplicationTgt => ReqEvents    applyPatchImplicationTgt e
      case e: SetGenericReqTitle  => ReqEvents    applySetGenericReqTitle  e
      case e: SetGenericReqType   => ReqEvents    applySetGenericReqType   e
      case e: SetCustomTextField  => ReqEvents    applySetCustomTextField  e
      case e: DeleteReqs          => ReqEvents    applyDelete              e

      case e: CreateReqCodeGroup  => ReqCodeGroupEvents applyCreate e
      case e: UpdateReqCodeGroup  => ReqCodeGroupEvents applyUpdate e
      case e: DeleteReqCodeGroups => ReqCodeGroupEvents applyDelete e

      case e: RestoreContent => applyRestoreContent(e)

      case e: ApplyTemplate => safely(applyAllUnsafe(e.t.events))
    }

  private def apply1Safe(event: Event): SE[Unit] =
    safely(apply1Unsafe(event))

  private def applyAllUnsafe(events: Events): SE[Unit] =
    SE.foldMapRun(events)(apply1Unsafe)

  private def applyAllSafe(events: Events): SE[Unit] =
    safely(applyAllUnsafe(events))
}
