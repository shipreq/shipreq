package shipreq.webapp.base.event

import scalaz.{-\/, \/-}
import utest._
import shipreq.base.util.UnivEq._
import shipreq.webapp.base.AppConsts
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.BaseTestUtil._
import ApplyEventTestFns._
import DeletionAction._

object ApplyEventTestFns {

  val tooLongStr = "a" * (AppConsts.largeTextMaxLength + 1)

  // TODO Move to Project once Rev is removed
  val emptyProject = {
    import DataImplicits._
    val rev = Rev(0)
    implicit def autoRevAnd[D](d: D): RevAnd[D] = RevAnd(rev, d)

    val cit = emptyDataMap(CustomIssueType)
    val crt = emptyDataMap(CustomReqType)
    val fs  = FieldSet(emptyDataMap(CustomField), StaticField.values.whole)
    val tt  = TagTree.empty
    val cfg = ProjectConfig(cit, crt, fs, tt)

    val reqs     = Requirements.empty
    val reqCodes = ReqCodes.empty
    val reqText  = ReqData.emptyText
    val reqTags  = ReqData.emptyTags
    val reqImps  = Implications.empty

    Project(cfg, reqs, reqCodes, reqText, reqTags, reqImps)
  }

  val apply = new ApplyEvent()(Untrusted)

  def fmtEvents(es: Seq[Event]): String = {
    val t = es.length
    es.zipWithIndex.map { case (e, i) => s"[${i + 1}/$t] $e" } mkString "\n"
  }

  def assertPass(es: Event*)(implicit init: InitialEvents): Unit =
    _assertPass(es: _*)

  def _assertPass(es: Event*)(implicit init: InitialEvents): Project = {
    val es2 = init ++ es
    val r = apply(es2)(emptyProject)
    val p =
      r match {
        case \/-(v) => v
        case -\/(e) => fail(s"\nPass expected but failed with '$e'.\nEvents were:\n${fmtEvents(es2)}")
      }
    assertQty(p, es2: _*)
    p
  }

  def assertFail(errFrag: String)(es: Event*)(implicit init: InitialEvents): Unit = {
    val r = apply(init ++ es)(emptyProject)
    r match {
      case -\/(e) => assertContainsCI(e, errFrag)
      case \/-(_) => fail(s"\nFailure expected but didn't occur.\nEvents were:\n${fmtEvents(es)}")
    }
  }

  def assertQty(p: Project, es: Event*): Unit = {
    var customIssueTypes = 0
    var customReqTypes   = 0
    var tags             = 0
    var customFields     = 0
    var activeFields     = emptyProject.config.fields.data.order.length
    var activeReqs       = 0
    var rcgs             = 0

    def ifHard(d: DeletionAction, f: => Unit): Unit =
      if (d == HardDel) f

    es foreach {
      case _: CreateGenericReq      => activeReqs += 1
      case _: CreateCustomIssueType => customIssueTypes += 1
      case _: CreateCustomReqType   => customReqTypes += 1
      case _: CreateCustomTextField
         | _: CreateCustomTagField
         | _: CreateCustomImpField  => activeFields += 1; customFields += 1
      case _: CreateTagGroup
         | _: CreateApplicableTag   => tags += 1
      case _: CreateReqCodeGroup    => rcgs += 1

      case DeleteCustomIssueType(_, d)       => ifHard(d, customIssueTypes -= 1)
      case DeleteCustomReqType  (_, d)       => ifHard(d, customReqTypes -= 1)
      case DeleteTag            (_, d)       => ifHard(d, tags -= 1)
      case DeleteStaticField    (_)          => activeFields -= 1
      case DeleteReq            (_, SoftDel) => activeReqs -= 1
      case DeleteReq            (_, Restore) => activeReqs += 1
      case DeleteCustomField    (_, HardDel) => activeFields -= 1; customFields -= 1
      case DeleteCustomField    (_, SoftDel) => activeFields -= 1
      case DeleteCustomField    (_, Restore) => activeFields += 1
      case DeleteReqCodeGroup   (_)          => rcgs -= 1

      case _: UpdateCustomIssueType
         | _: UpdateCustomReqType
         | _: UpdateCustomTextField
         | _: UpdateCustomTagField
         | _: UpdateCustomImpField
         | _: RepositionField
         | _: PatchReqCodes
         | _: PatchReqTags
         | _: PatchImplicationSrc
         | _: PatchImplicationTgt
         | _: UpdateReqCodeGroup
         | _: UpdateApplicableTag
         | _: UpdateTagGroup => ()
    }

    assertEq("Σ CustomIssueTypes", p.config.customIssueTypes.data.size, customIssueTypes)
    assertEq("Σ CustomReqTypes", p.config.customReqTypes.data.size, customReqTypes)
    assertEq("Σ Tags", tags, p.config.tags.data.size)
    assertEq("Σ CustomFields", customFields, p.config.fields.data.customFields.size)
    assertEq("Σ ActiveFields", activeFields, p.config.fields.data.fields.unmust.count(_.live :: Live))
    assertEq("Σ ActiveReqs", activeReqs, p.reqs.data.reqs.values.filter(_.live :: Live).size)
    assertEq("Σ ReqCodeGroups", rcgs, p.reqCodes.data.activeGroups.size)
  }
}

case class InitialEvents(es: Event*) {
  def ++(next: Seq[Event]) = es ++ next
}
trait NoInitialEvents {
  implicit val init = InitialEvents()
}
object NoInitialEvents extends NoInitialEvents

// =====================================================================================================================
abstract class SharedTests(implicit val init: InitialEvents) extends TestSuite {
  type CE <: Event
  val c1 : CE
  val c2 : CE
  val u1 : Event
  val sd1: Event
  val hd1: Event
  val r1 : Event

  def setId(c: CE, id: Int): CE
  def copyId(to: CE, from: CE): CE

  override def tests = TestSuite {
    'create {
      'one      - assertPass(c1)
      'two      - assertPass(c1, c2)
      'zeroId   - assertFail(" id ")          (setId(c1, 0))
      'negId    - assertFail(" id ")          (setId(c1, -1))
      'dupId    - assertFail("already exists")(c1, copyId(c2, from = c1))
    }

    'update {
      'notFound - assertFail("not found")(u1)
      'afterHD  - assertFail("not found")(c1, hd1, u1)
      'dead     - assertFail("dead")     (c1, sd1, u1)
    }

    'delete {
      'okHard    - assertPass(c1, hd1)
      'okSoft    - assertPass(c1, sd1)
      'okRest    - assertPass(c1, sd1, r1)
      'okMulti   - assertPass(c1, sd1, r1, sd1, r1, hd1)
      'notFound  - List(hd1, sd1, r1).foreach(d => assertFail("not found")(d))
      'hardTwice - assertFail("not found")(c1, hd1, hd1)
      'hardRest  - assertFail("not found")(c1, hd1, r1)

      // Disabling for now. These are NOP issues, not integrity issues.
      // 'softTwice - assertFail("x")(c1, sd1, sd1)
      // 'restTwice - assertFail("x")(c1, sd1, r1, r1)
      // 'restLive  - assertFail("x")(c1, r1)
    }
  }
}
