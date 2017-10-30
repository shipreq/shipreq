package shipreq.webapp.base.event

import scalaz.{-\/, \/-}
import utest._
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import ApplyEventTestFns._

object ApplyEventTestFns {

  val tooLongStr = "a" * (WebappConfig.largeTextMaxLength + 1)

  val apply = ApplyEvent.untrusted

  def fmtEvents(es: Seq[Event]): String = {
    val t = es.length
    es.zipWithIndex.map { case (e, i) => s"[${i + 1}/$t] $e" } mkString "\n"
  }

  def assertPass(es: Event*)(implicit init: InitialEvents): Unit =
    _assertPass(es: _*)

  def _assertPass(es: Event*)(implicit init: InitialEvents): Project = {
    val es2 = init ++ es

    def go(ae: ApplyEvent): Project = {
      val r = ae(es2)(Project.empty)
      val p =
        r match {
          case \/-(v) => v
          case -\/(e) => fail(s"\nPass expected but failed with '$e' (${ae.trust}).\nEvents were:\n${fmtEvents(es2)}")
        }
      assertQty(p, es2: _*)
      p
    }

    val p = go(ApplyEvent.untrusted)
    val p2 = go(ApplyEvent.trusted)
    assertEq(p, p2)
    p
  }

  def assertFail(errFrag: String)(es: Event*)(implicit init: InitialEvents): Unit = {
    // Only the last event should fail - apply init and ensure ok
    val vb = Vector.newBuilder[Event]
    vb ++= init.es
    vb ++= es
    val ev = vb.result()
    val p1 = _assertPass(ev.init: _*)(NoInitialEvents.init)

    // Now apply the last event
    val r = apply.apply1(ev.last)(p1)
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
    var activeRCGs       = 0
    var useCases         = 0
    var genericReqs      = 0
    var delReasons       = 0
    var savedViews       = 0

    es foreach {
      case _: GenericReqCreate      => genericReqs += 1
      case _: UseCaseCreate         => useCases += 1
      case _: CustomIssueTypeCreate => customIssueTypes += 1
      case _: CustomReqTypeCreate   => customReqTypes += 1
      case _: FieldCustomTextCreate
         | _: FieldCustomTagCreate
         | _: FieldCustomImpCreate  => customFields += 1
      case _: TagGroupCreate
         | _: ApplicableTagCreate   => tags += 1
      case _: CodeGroupCreate    => activeRCGs += 1
      case e: CodeGroupsDelete   => activeRCGs -= e.ids.size

      case ProjectTemplateApply(t) => t match {
        case ProjectTemplate.Default =>
          customReqTypes   +=  9
          customIssueTypes +=  3
          tags             += 16
          customFields     +=  5
      }

      case d: ReqsDelete =>
        if (d.reason.nonEmpty)
          delReasons += 1
        activeRCGs -= d.codeGroups.size

      case r: ContentRestore =>
        activeRCGs += r.codeGroups.size

      case _: SavedViewCreate => savedViews += 1
      case _: SavedViewDelete => savedViews -= 1

      case _: ApplicableTagUpdate
         | _: CustomIssueTypeDelete
         | _: CustomIssueTypeRestore
         | _: CustomIssueTypeUpdate
         | _: CustomReqTypeDelete
         | _: CustomReqTypeRestore
         | _: CustomReqTypeUpdate
         | _: FieldCustomDelete
         | _: FieldCustomImpUpdate
         | _: FieldCustomRestore
         | _: FieldCustomTagUpdate
         | _: FieldCustomTextUpdate
         | _: FieldReposition
         | _: FieldStaticAdd
         | _: FieldStaticRemove
         | _: GenericReqTitleSet
         | _: GenericReqTypeSet
         | _: ProjectNameSet
         | _: CodeGroupUpdate
         | _: ReqCodesPatch
         | _: ReqFieldCustomTextSet
         | _: ReqImplicationsPatch
         | _: ReqTagsPatch
         | _: SavedViewDefaultSet
         | _: SavedViewUpdate
         | _: TagDelete
         | _: TagGroupUpdate
         | _: TagRestore
         | _: UseCaseStepCreate
         | _: UseCaseStepDelete
         | _: UseCaseStepRestore
         | _: UseCaseStepShiftLeft
         | _: UseCaseStepShiftRight
         | _: UseCaseTitleSet
         | _: UseCaseStepUpdate => ()
    }

    val cfg = p.config
    assertEq("Σ CustomIssueTypes", cfg.customIssueTypes.size, customIssueTypes)
    assertEq("Σ CustomReqTypes", cfg.reqTypes.custom.size, customReqTypes)
    assertEq("Σ Tags", tags, cfg.tags.size)
    assertEq("Σ CustomFields", customFields, cfg.fields.customFields.size)
    assertEq("Σ Generic Reqs", genericReqs, p.reqs.genericReqs.size)
    assertEq("Σ Use Cases", useCases, p.reqs.useCases.imap.size)
    assertEq("Σ Reqs", genericReqs + useCases, p.reqs.size)
    assertEq("Σ CodeGroups (active)", activeRCGs, p.reqCodes.groups.count(_.live is Live))
    assertEq("Σ DeletionReasons", delReasons, p.deletionReasons.reasons.size)
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
  def add(es: Event*) = InitialEvents(this.es ++ es: _*)
  def filter(f: Event => Boolean) = InitialEvents(es.filter(f): _*)
}
trait NoInitialEvents {
  implicit val init = InitialEvents()
}
object NoInitialEvents extends NoInitialEvents

class EventTester(implicit init: InitialEvents) {
  var p = _assertPass()(init)
  var es = init.es.toVector

  var makeName: (Int, Event) => String =
    (i, e) => s"Step #$i (${e.getClass.getSimpleName})"

  var testNo = 0

  def justApply(es: Event*): Unit =
    es foreach (apply(_)(_ => ()))

  def apply(e: Event)(test: (=> String) => Unit): Unit = {
    testNo += 1
    def name = makeName(testNo, e)
    ApplyEventTestFns.apply.apply1(e)(p) match {
      case \/-(p2)  => p = p2
      case -\/(err) => fail(s"$name failed: $err")
    }
    es :+= e
    test(name)
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
