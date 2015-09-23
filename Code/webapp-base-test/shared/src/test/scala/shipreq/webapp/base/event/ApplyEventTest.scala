package shipreq.webapp.base.event

import scalaz.{-\/, \/-}
import utest._
import shipreq.base.util.UnivEq.Implicits._
import shipreq.webapp.base.AppConsts
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.BaseTestUtil._
import ApplyEventTestFns._
import DeletionAction._

object ApplyEventTestFns {

  val tooLongStr = "a" * (AppConsts.largeTextMaxLength + 1)

  val apply = ApplyEvent.untrusted

  def fmtEvents(es: Seq[Event]): String = {
    val t = es.length
    es.zipWithIndex.map { case (e, i) => s"[${i + 1}/$t] $e" } mkString "\n"
  }

  def assertPass(es: Event*)(implicit init: InitialEvents): Unit =
    _assertPass(es: _*)

  def _assertPass(es: Event*)(implicit init: InitialEvents): Project = {
    val es2 = init ++ es
    val r = apply(es2)(Project.empty)
    val p =
      r match {
        case \/-(v) => v
        case -\/(e) => fail(s"\nPass expected but failed with '$e'.\nEvents were:\n${fmtEvents(es2)}")
      }
    assertQty(p, es2: _*)
    p
  }

  def assertFail(errFrag: String)(es: Event*)(implicit init: InitialEvents): Unit = {
    val r = apply(init ++ es)(Project.empty)
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
    var activeReqs       = 0
    var rcgs             = 0

    es foreach {
      case _: CreateGenericReq      => activeReqs += 1
      case _: CreateCustomIssueType => customIssueTypes += 1
      case _: CreateCustomReqType   => customReqTypes += 1
      case _: CreateCustomTextField
         | _: CreateCustomTagField
         | _: CreateCustomImpField  => customFields += 1
      case _: CreateTagGroup
         | _: CreateApplicableTag   => tags += 1
      case _: CreateReqCodeGroup    => rcgs += 1

      case DeleteReq            (_, Delete ) => activeReqs -= 1
      case DeleteReq            (_, Restore) => activeReqs += 1
      case DeleteReqCodeGroup   (_)          => rcgs -= 1

      case ApplyTemplate(t) => t match {
        case ProjectTemplate.Default =>
          customReqTypes   +=  9
          customIssueTypes +=  3
          tags             += 16
          customFields     +=  5
      }

      case _: UpdateCustomIssueType
         | _: UpdateCustomReqType
         | _: UpdateCustomTextField
         | _: UpdateCustomTagField
         | _: UpdateCustomImpField
         | _: DeleteCustomIssueType
         | _: DeleteCustomReqType
         | _: DeleteTag
         | _: DeleteCustomField
         | _: DeleteStaticField
         | _: AddStaticField
         | _: RepositionField
         | _: PatchReqCodes
         | _: PatchReqTags
         | _: PatchImplicationSrc
         | _: PatchImplicationTgt
         | _: SetGenericReqType
         | _: SetGenericReqTitle
         | _: SetCustomTextField
         | _: UpdateReqCodeGroup
         | _: UpdateApplicableTag
         | _: UpdateTagGroup => ()
    }

    val cfg = p.config
    assertEq("Σ CustomIssueTypes", cfg.customIssueTypes.size, customIssueTypes)
    assertEq("Σ CustomReqTypes", cfg.customReqTypes.size, customReqTypes)
    assertEq("Σ Tags", tags, cfg.tags.size)
    assertEq("Σ CustomFields", customFields, cfg.fields.customFields.size)
    assertEq("Σ ActiveReqs", activeReqs, p.reqs.reqs.values.count(_.live(cfg.customReqTypes) :: Live))
    assertEq("Σ ReqCodeGroups", rcgs, p.reqCodes.activeGroups.size)
    validateIdCeilings(p)
  }

  def validateIdCeilings(p: Project): Unit = {
    val a = p.idCeilings
    val b = IdCeilings.calculate(p)
    if ( a.customIssueType < b.customIssueType
      || a.customReqType   < b.customReqType
      || a.customField     < b.customField
      || a.tag             < b.tag
      || a.req             < b.req
      || a.reqCode         < b.reqCode
      ) fail(s"Have $a < Calc $b")
  }
}

case class InitialEvents(es: Event*) {
  def ++(next: Seq[Event]) = es ++ next
}
trait NoInitialEvents {
  implicit val init = InitialEvents()
}
object NoInitialEvents extends NoInitialEvents

class EventTester(implicit init: InitialEvents) {
  var p = _assertPass()
  var es = init.es.toVector

  var testNo = 0

  def justApply(es: Event*): Unit =
    es foreach (apply(_)(_ => ()))

  def apply(e: Event)(assert: (=> String) => Unit): Unit = {
    testNo += 1
    ApplyEventTestFns.apply.apply1(e)(p) match {
      case \/-(p2)  => p = p2
      case -\/(err) => fail(s"$e was expected to pass but failed with: $err")
    }
    es :+= e
    assert(s"Step #$testNo")
    assertQty(p, es: _*)
  }
}

// =====================================================================================================================
abstract class SharedTests(implicit val init: InitialEvents) extends TestSuite {
  type CE <: Event
  val c1 : CE
  val c2 : CE
  val u1 : Event
  val sd1: Event
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
      'dead     - assertFail("dead")     (c1, sd1, u1)
      //'afterHD  - assertFail("not found")(c1, hd1, u1)
    }

    'delete {
      'okSoft    - assertPass(c1, sd1)
      'okRest    - assertPass(c1, sd1, r1)
      'okMulti   - assertPass(c1, sd1, r1, sd1, r1)
      'notFound  - List(sd1, r1).foreach(d => assertFail("not found")(d))

      // All hard-deletion has been removed
      // 'okHard    - assertPass(c1, hd1)
      // 'hardTwice - assertFail("not found")(c1, hd1, hd1)
      // 'hardRest  - assertFail("not found")(c1, hd1, r1)

      // Disabling for now. These are NOP issues, not integrity issues.
      // 'softTwice - assertFail("x")(c1, sd1, sd1)
      // 'restTwice - assertFail("x")(c1, sd1, r1, r1)
      // 'restLive  - assertFail("x")(c1, r1)
    }
  }
}
