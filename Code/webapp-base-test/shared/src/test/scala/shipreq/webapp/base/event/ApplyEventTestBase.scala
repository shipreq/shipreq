package shipreq.webapp.base.event

import scalaz.{-\/, \/-}
import sourcecode.Line
import utest.{assert => _, _}
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import ApplyEventTestFns._
import Event._

object ApplyEventTestFns {

  val tooLongStr = "a" * (WebappConfig.largeTextMaxLength + 1)

  val apply = ApplyEvent.untrusted

  def fmtEvents(es: Seq[Event]): String = {
    val t = es.length
    es.zipWithIndex.map { case (e, i) => s"[${i + 1}/$t] $e" } mkString "\n"
  }

  def assertPass(es: Event*)(implicit init: InitialEvents, l: Line): Unit =
    _assertPass(es: _*)

  def _assertPass(es: Event*)(implicit init: InitialEvents, l: Line): Project = {
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

  def assertFail(errFrag: String)(es: Event*)(implicit init: InitialEvents, l: Line): Unit = {
    // Only the last event should fail - apply init and ensure ok
    val vb = Vector.newBuilder[Event]
    vb ++= init.es
    vb ++= es
    val ev = vb.result()
    val p1 = _assertPass(ev.init: _*)(NoInitialEvents.init, l)

    // Now apply the last event
    val r = apply.apply1(ev.last)(p1)
    r match {
      case -\/(e) => assertContainsCI(e, errFrag)
      case \/-(_) => fail(s"\nFailure expected but didn't occur.\nEvents were:\n${fmtEvents(es)}")
    }
  }

  def assertQty(p: Project, es: Event*)(implicit l: Line): Unit = {
    var customIssueTypes = 0
    var customReqTypes   = 0
    var tags             = 0
    var customFields     = 0
    var activeRCGs       = 0
    var useCases         = 0
    var genericReqs      = 0
    var delReasons       = 0
    var manualIssues     = 0
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
         | _: ApplicableTagCreate
         | _: ApplicableTagCreateV1 => tags += 1
      case _: CodeGroupCreate       => activeRCGs += 1
      case e: CodeGroupsDelete      => activeRCGs -= e.ids.size

      case ProjectTemplateApply(t) => t match {
        case ProjectTemplate.V1 =>
          customReqTypes   += 5
          customIssueTypes += 2
          tags             += 9
          customFields     += 4
          savedViews       += 2
      }

      case d: ReqsDelete =>
        if (d.reason.nonEmpty)
          delReasons += 1
        activeRCGs -= d.codeGroups.size

      case r: ContentRestore =>
        activeRCGs += r.codeGroups.size

      case _: SavedViewCreate => savedViews += 1
      case _: SavedViewDelete => savedViews -= 1

      case _: ManualIssueCreate => manualIssues += 1
      case _: ManualIssueDelete => manualIssues -= 1

      case _: ApplicableTagUpdate
         | _: ApplicableTagUpdateV1
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
         | _: ManualIssueUpdate
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
    assert(cfg.reqTypes.custom.size <= customReqTypes, "Σ CustomReqTypes")
    assert(cfg.fields.customFields.size <= customFields, "Σ CustomFields")
    assertEq("Σ CustomIssueTypes", cfg.customIssueTypes.size, customIssueTypes)
    assertEq("Σ Tags", tags, cfg.tags.tree.size)
    assertEq("Σ Generic Reqs", genericReqs, p.content.reqs.genericReqs.size)
    assertEq("Σ Use Cases", useCases, p.content.reqs.useCases.imap.size)
    assertEq("Σ Reqs", genericReqs + useCases, p.content.reqs.size)
    assertEq("Σ CodeGroups (active)", activeRCGs, p.content.reqCodes.groups.count(_.live is Live))
    assertEq("Σ DeletionReasons", delReasons, p.content.deletionReasons.reasons.size)
    assertEq("Σ ManualIssues", manualIssues, p.manualIssues.imap.size)
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

class EventTester(implicit init: InitialEvents, l: Line) {
  var p = _assertPass()(init, l)
  var es = init.es.toVector

  var makeName: (Int, Event) => String =
    (i, e) => s"Step #$i (${e.getClass.getSimpleName})"

  var testNo = 0

  def justApply(es: Event*)(implicit l: Line): Unit =
    es foreach (apply(_)(_ => ()))

  def apply(e: Event)(test: (=> String) => Unit)(implicit l: Line): Unit = {
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

  def prepForSoftDelete(es: Event*): Seq[Event] =
    c1 +: es

  override def tests = Tests {
    'create {
      'one      - assertPass(c1)
      'two      - assertPass(c1, c2)
      'zeroId   - assertFail(" id ")          (setId(c1, 0))
      'negId    - assertFail(" id ")          (setId(c1, -1))
      'dupId    - assertFail("already exists")(c1, copyId(c2, from = c1))
    }

    'update {
      'notFound - assertFail("not found")(u1)
      'dead     - assertFail("dead")     (prepForSoftDelete(sd1, u1): _*)
      //'afterHD  - assertFail("not found")(c1, hd1, u1)
    }

    'delete {
      'okSoft    - assertPass(prepForSoftDelete(sd1): _*)
      'okRest    - assertPass(prepForSoftDelete(sd1, r1): _*)
      'okMulti   - assertPass(prepForSoftDelete(sd1, r1, sd1, r1): _*)
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
