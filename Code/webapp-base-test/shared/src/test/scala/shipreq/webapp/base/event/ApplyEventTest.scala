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

  def assertPass(es: Event*): Unit =
    _assertPass(es: _*)

  def _assertPass(es: Event*): Project = {
    val r = apply(es) run emptyProject
    val p =
      r match {
        case \/-(v) => v
        case -\/(e) => fail(s"\nPass expected but failed with '$e'.\nEvents were:\n${fmtEvents(es)}")
      }
    assertQty(p, es: _*)
    p
  }

  def assertFail(errFrag: String)(es: Event*): Unit = {
    val r = apply(es) run emptyProject
    r match {
      case -\/(e) => assert(e contains errFrag)
      case \/-(_) => fail(s"\nFailure expected but didn't occur.\nEvents were:\n${fmtEvents(es)}")
    }
  }

  def assertQty(p: Project, es: Event*): Unit = {
    var customReqTypes = 0
    var atags          = 0
    var tagGroups      = 0
    def ifHard(d: DeletionAction, f: => Unit): Unit =
      if (d == HardDel) f
    es foreach {
      case _: CreateCustomReqType => customReqTypes += 1
      case _: CreateTagGroup      => tagGroups += 1
      case _: CreateApplicableTag => atags += 1
      case DeleteCustomReqType(_, d) => ifHard(d, customReqTypes -= 1)
      case DeleteApplicableTag(_, d) => ifHard(d, atags -= 1)
      case DeleteTagGroup     (_, d) => ifHard(d, tagGroups -= 1)
      case _: UpdateCustomReqType
         | _: UpdateApplicableTag
         | _: UpdateTagGroup => ()
    }
    val actualAtags = p.config.atags.size
    assertEq("CustomReqTypes", p.config.customReqTypes.data.size, customReqTypes)
    assertEq("ApplicableTags", actualAtags, atags)
    assertEq("TagGroups",      p.config.tags.data.size - actualAtags, tagGroups)
  }
}

// =====================================================================================================================
abstract class SharedTests extends TestSuite {
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
