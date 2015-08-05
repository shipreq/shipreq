package shipreq.webapp.base.event

import japgolly.nyaya.LogicPropExt
import scala.collection.GenTraversable
import shipreq.webapp.base.data.{Project, DataProp}
import ApplyEventLib._

object ApplyEvent {
  val trusted   = new ApplyEvent()(Trusted)
  val untrusted = new ApplyEvent()(Untrusted)
}

class ApplyEvent(implicit val trust: Trust) extends ApplyContentEvent {

  def apply(events: GenTraversable[Event]): AP =
    apFoldLeft(events)(_apply1) >=> validateDataProps

  def apply1(event: Event): AP =
    _apply1(event) >=> validateDataProps

  private val validateDataProps: AP = whenUntrusted {
    val prop = DataProp.project.allIncludingConfig
    App { p =>
      val e = prop(p)
      if (e.success)
        ok(p)
      else
        fail(e.report)
    }
  }

  def applyVerified(ves: GenTraversable[VerifiedEvent]): AP =
    if (ves.isEmpty)
      nop
    else
      App { p =>
        val applyAll = apFoldLeft(ves)(ve => _apply1(ve.event)) >=> validateDataProps
        applyAll(p).flatMap(validateHash(_, ves.last))
        // TODO On failure, replay to find the first mismatching event
      }

  def validateHash(p: Project, ve: VerifiedEvent): Result[Project] = {
    val h2 = ve.hashScheme.hashProject hash p
    if (ve.hash == h2)
      ok(p)
    else
      fail(s"Hash mismatch on $ve. Got $h2.")
  }

  private def _apply1(event: Event): AP =
    event match {
      case e: CreateCustomIssueType => CustomIssueTypeEvents applyCreate e
      case e: UpdateCustomIssueType => CustomIssueTypeEvents applyUpdate e
      case e: DeleteCustomIssueType => CustomIssueTypeEvents applyDelete e

      case e: CreateCustomReqType => CustomReqTypeEvents applyCreate e
      case e: UpdateCustomReqType => CustomReqTypeEvents applyUpdate e
      case e: DeleteCustomReqType => CustomReqTypeEvents applyDelete e

      case e: CreateApplicableTag => ApplicableTagEvents applyCreate e
      case e: UpdateApplicableTag => ApplicableTagEvents applyUpdate e
      case e: DeleteApplicableTag => ApplicableTagEvents applyDelete e

      case e: CreateTagGroup      => TagGroupEvents      applyCreate e
      case e: UpdateTagGroup      => TagGroupEvents      applyUpdate e
      case e: DeleteTagGroup      => TagGroupEvents      applyDelete e

      case e: CreateCustomTextField => CustomTextFieldEvents applyCreate e
      case e: UpdateCustomTextField => CustomTextFieldEvents applyUpdate e
      case e: CreateCustomTagField  => CustomTagFieldEvents  applyCreate e
      case e: UpdateCustomTagField  => CustomTagFieldEvents  applyUpdate e
      case e: CreateCustomImpField  => CustomImpFieldEvents  applyCreate e
      case e: UpdateCustomImpField  => CustomImpFieldEvents  applyUpdate e
      case e: DeleteCustomField     => FieldEvents           applyDeleteC e
      case e: DeleteStaticField     => FieldEvents           applyDeleteS e
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
      case e: DeleteReq           => ReqEvents    applyDelete              e

      case e: CreateReqCodeGroup => ReqCodeGroupEvents applyCreate e
      case e: UpdateReqCodeGroup => ReqCodeGroupEvents applyUpdate e
      case e: DeleteReqCodeGroup => ReqCodeGroupEvents applyDelete e

      case e: ApplyTemplate => apply(e.t.events)
    }
}
