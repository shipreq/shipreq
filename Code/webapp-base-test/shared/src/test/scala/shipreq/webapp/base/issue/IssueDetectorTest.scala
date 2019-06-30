package shipreq.webapp.base.issue

import japgolly.microlibs.nonempty.{NonEmpty, NonEmptySet}
import nyaya.util.Multimap
import scala.reflect.ClassTag
import sourcecode.Line
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.text.{Text => T}
import utest._

object IssueDetectorTest extends TestSuite {

  import SampleProject3.{Values => P3, project => p3}
  import SampleProject4.{Values => P4, project => p4}
  import SampleProject6.{Values => P6, project => p6}

  private lazy val demoId         = p3.content.reqCodes("demo").get.activeId.get.value.RCG
  private lazy val demoWhateverId = p3.content.reqCodes("demo.whatever").get.activeId.get.value.ARC

  private case class IssueFilter(ok: Issue => Boolean)
  private object IssueFilter {
    def any = apply(_ => true)

    def apply[I <: Issue](implicit ct: ClassTag[I]): IssueFilter =
      new IssueFilter(ct.unapply(_).isDefined)

    def collect(f: PartialFunction[Issue, Any]): IssueFilter =
      new IssueFilter(f.isDefinedAt)

    def apply(ic: IssueClass): IssueFilter =
      new IssueFilter(_.cls ==* ic)

    def apply(ic: IssueCategory): IssueFilter =
      new IssueFilter(_.cls.category ==* ic)
  }

  private def updateReqTags(id: ReqId)(del: ApplicableTagId*)(add: ApplicableTagId*) =
    Event.ReqTagsPatch(id, NonEmpty force SetDiff(removed = del.toSet, added = add.toSet))

  private def updateTagGroup(id: TagGroupId, mutexChildren: MutexChildren) = {
    import TagGroupGD._
    Event.TagGroupUpdate(id, nev(MutexChildren(mutexChildren)))
  }

  private def assertIssues(project: Project)(expected: Issue*)(implicit l: Line, f: IssueFilter): Unit =
    assertIssuesWithFilter(project, f.ok)(expected: _*)

  private def assertIssuesWithFilter(project: Project, filter: Issue => Boolean)(expected: Issue*)(implicit l: Line): Unit = {
    val it = IssueTracker(project)
    def actual = it.issues.vector.iterator.map(_.issue).filter(filter)
    assertSeqIgnoreOrder("assertIssues", actual, expect = expected)
  }

  private def test(p: Project)(events: Event*)(expect: Issue*)(implicit l: Line, f: IssueFilter): Unit =
    assertIssues(applyEventsSuccessfully(p, events: _*))(expect: _*)

  private def debugTags(project: Project): Project = {
    println(project.config.tags.prettyPrint)
    for (r <- project.content.reqs.reqIterator.toList.sortBy(_.id.value)) {
      val tags = project.content.reqTags(r.id).map(_.value).toList.sorted.mkString(", ")
      val isLive = r.live(project.config.reqTypes) is Dead
      println(s"(#${r.id.value}) $tags${if (isLive) " [DEAD]" else ""}")
    }
    project
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object BlankTests {
    private implicit val filter = IssueFilter.collect {
      case _: Issue.BlankTitle | _: Issue.BlankUseCaseStep => ()
    }

    def title() = test(p4)(
      Event.GenericReqTitleSet(P6.frs(1), ∅),
      Event.UseCaseTitleSet(P6.uc1, ∅),
    )(
      Issue.BlankTitle(P6.frs(1)),
      Issue.BlankTitle(P6.uc1),
    )

    def ucs() = test(p4)(
      Event.UseCaseStepUpdate(10, UseCaseStepGD.ValueForTitle(∅)), // UC-n.0 -- no issue, uses UC title
      Event.UseCaseStepUpdate(11, UseCaseStepGD.ValueForTitle(∅)), // UC-n.0.1
      Event.UseCaseStepUpdate(14, UseCaseStepGD.ValueForTitle(∅)), // UC-n.1
      Event.UseCaseStepUpdate(13, UseCaseStepGD.ValueForTitle(∅)), // UC-n.0.3 -- dead step
      Event.UseCaseStepDelete(13),
    )(
      Issue.BlankUseCaseStep(11),
      Issue.BlankUseCaseStep(14),
    )

    def emptyStepAndTitle() = test(p4)(
      Event.UseCaseTitleSet(P6.uc1, ∅),
      Event.UseCaseStepUpdate(10, UseCaseStepGD.ValueForTitle(∅)),
    )(
      Issue.BlankTitle(P6.uc1),
      // don't report both the step and title title
    )
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object BlankCustomFieldTests {
    private implicit val filter = IssueFilter(IssueClass.BlankCustomField)

    import P4._

    def notAllReqTypes() = test(p4)(
      Event.FieldCustomRestore(reporterField), // only applies to DD & UC
    )(
      Issue.BlankCustomField(frs(1), priField),
      Issue.BlankCustomField(frs(2), priField),
      Issue.BlankCustomField(uc1, priField),
      Issue.BlankCustomField(uc1, reporterField),
    )

    private def makeMfMandatoryForFrAndUc = {
      val I = CustomImpFieldGD
      val frAndUc = onlyReqTypes(fr, StaticReqType.UseCase)
      Event.FieldCustomImpUpdate(mfField, I.nev(I.ValueForMandatory(Mandatory), I.ValueForReqTypes(frAndUc)))
    }

    // fr1 <- mf12,19
    // fr2 <-
    // uc1 <- fr1
    def imps1() = test(p4)(
      makeMfMandatoryForFrAndUc,
      Event.ReqImplicationsPatch(frs(2), Backwards, nesd[ReqId](mfs(1), mfs(13), mfs(22), frs(1))()),
      Event.ReqImplicationsPatch(uc1, Backwards, nesd[ReqId]()(frs(1))),
    )(
      Issue.BlankCustomField(frs(1), priField),
      Issue.BlankCustomField(frs(2), priField),
      Issue.BlankCustomField(uc1, priField),
      Issue.BlankCustomField(frs(2), mfField),
    )

    // fr1 <- mf12,19
    // fr2 <- fr1 [fr2 is DEAD]
    // uc1 <- fr2 [fr2 is DEAD]
    def imps2() = test(p4)(
      makeMfMandatoryForFrAndUc,
      Event.ReqImplicationsPatch(frs(2), Backwards, nesd[ReqId](mfs(1), mfs(13), mfs(22))(frs(1))),
      Event.ReqImplicationsPatch(uc1, Backwards, nesd[ReqId]()(frs(2))),
      Event.ReqsDelete(NonEmptySet(frs(2)), ∅, ∅),
    )(
      Issue.BlankCustomField(frs(1), priField),
      Issue.BlankCustomField(uc1, priField),
      Issue.BlankCustomField(uc1, mfField),
    )
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object ConflictingTagTests {
    private implicit val filter = IssueFilter[Issue.ConflictingTags]

    def ko() = test(p3)(
      updateTagGroup(20, MutexChildren),
      Event.ContentRestore(Set(1119), ∅),
      updateReqTags(1101)()(4),
      updateReqTags(1104)()(2, 24, 25),
      updateReqTags(1119)()(2, 3),
      Event.ReqsDelete(NonEmptySet(1119), ∅, ∅),
    )(
      Issue.ConflictingTags(1101, 1, NonEmptySet(ReqTagLoc.Tags)),
      Issue.ConflictingTags(1104, 1, NonEmptySet(ReqTagLoc.Tags)),
      Issue.ConflictingTags(1104, 20, NonEmptySet(ReqTagLoc.Tags)),
      Issue.ConflictingTags(1107, 20, NonEmptySet(ReqTagLoc.Tags)),
    )

    def deadTag() = test(p3)(
      updateReqTags(1101)()(4),
      Event.TagDelete(4.AT),
    )()

    def deadTagGroup() = test(p3)(
      updateReqTags(1101)()(4),
      Event.TagDelete(P3.priTG),
    )()

    def tagInText() = {
      import T.GenericReqTitle.TagRef
      test(p3)(
        Event.GenericReqTitleSet(1002, Vector(TagRef(P3.priHigh), TagRef(P3.priLow))), // no tags
        Event.GenericReqTitleSet(1103, Vector(TagRef(P3.priLow))), // + highPri in tags
        Event.GenericReqTitleSet(1104, Vector(TagRef(P3.priMed))), // + priMed in tags
      )(
        Issue.ConflictingTags(1002, P3.priTG, NonEmptySet(ReqTextLoc.Title)),
        Issue.ConflictingTags(1103, P3.priTG, NonEmptySet(ReqTagLoc.Tags, ReqTextLoc.Title)),
      )
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object DeadRefTests {
    private implicit val filter = IssueFilter.collect {
      case i: Issue.DeadRefInRcg => i
      case i: Issue.DeadRefInReq => i
    }

    def issueDesc() = test(p3)(
      Event.ReqsDelete(NonEmptySet(P3.frs(2), P3.mfs(26)), ∅, ∅),
    )()

    def inRcg() = test(p3)(
      Event.CodeGroupUpdate(demoId, CodeGroupGD.ValueForTitle(Vector(T.CodeGroupTitle.ReqRef(P3.frs(2))))),
      Event.ReqsDelete(NonEmptySet.one(P3.frs(2)), ∅, ∅),
    )(
      Issue.DeadRefInRcg(demoId, ContentRef.ReqRef(P3.frs(2)))
    )

    def toRcg() = test(p3)(
      ContentEventTestHelp.createRCG(987, "haha.boop"),
      Event.GenericReqTitleSet(1001, Vector(T.GenericReqTitle.CodeRef(987.RCG))),
      Event.GenericReqTitleSet(1002, Vector(T.GenericReqTitle.CodeRef(demoId))),
      Event.CodeGroupsDelete(NonEmptySet.one(demoId)),
    )(
      Issue.DeadRefInReq(1002, ReqTextLoc.Title, ContentRef.CodeRef(demoId)),
    )
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object DeadTagTests {
    private implicit val filter = IssueFilter[Issue.DeadTag]

    import T.GenericReqTitle.TagRef

    def ko() = test(p3)(
      Event.GenericReqTitleSet(P3.frs(1), Vector(TagRef(P3.priHigh), TagRef(P3.priMed), TagRef(P3.priLow))),
      Event.TagDelete(P3.priHigh),
      Event.TagDelete(P3.priLow),
    )(
      Issue.DeadTag(P3.frs(1), ReqTextLoc.Title, P3.priHigh),
      Issue.DeadTag(P3.frs(1), ReqTextLoc.Title, P3.priLow),
    )
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object EmptyCodeGroupTests {
    private implicit val filter = IssueFilter[Issue.EmptyCodeGroup]

    def ko() = test(p3)(
      Event.ReqCodesPatch(P3.frs(1), Set(demoWhateverId), ∅, Multimap.empty),
    )(Issue.EmptyCodeGroup(demoId))

    def deadChild() = test(p3)(
      Event.ReqsDelete(NonEmptySet.one(P3.frs(1)), ∅, ∅),
    )(Issue.EmptyCodeGroup(demoId))

    def deadCodeGroup() = test(p3)(
      Event.ReqsDelete(NonEmptySet.one(P3.frs(1)), Set(demoId), ∅),
    )()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object IssueTagTests {
    private implicit val filter = IssueFilter.collect {
      case i: Issue.IssueTagInRcg     => i
      case i: Issue.IssueTagInReq     => i
      case i: Issue.DeadIssueTagInRcg => i
      case i: Issue.DeadIssueTagInReq => i
    }

    private val delFRs = Event.ReqsDelete(NonEmptySet(P3.frs(1), P3.frs(2)), ∅, ∅)

    def rcg() = test(p3)(
      delFRs,
      Event.CodeGroupUpdate(demoId, CodeGroupGD.ValueForTitle(Vector(T.CodeGroupTitle.Issue(1, ∅)))),
    )(
      Issue.IssueTagInRcg(demoId, T.CodeGroupTitle.Issue(1, ∅)),
    )

    def deadIssue() = test(p3)(
      Event.CustomIssueTypeDelete(1),
      Event.CustomIssueTypeDelete(2),
    )(
      Issue.DeadIssueTagInReq(P3.frs(1), ReqTextLoc.Title, T.GenericReqTitle.Issue(1, ∅)),
      Issue.DeadIssueTagInReq(P3.frs(2), ReqTextLoc.Title, T.GenericReqTitle.Issue(2, SampleProject3.inlineIssueDesc)),
    )

    def ok() = test(p3)(delFRs)()

    def txtField() = test(p3)(
      delFRs,
      Event.ReqFieldCustomTextSet(P3.mfs(3), P3.descField, Vector(T.CustomTextField.Issue(1, ∅))),
    )(
      Issue.IssueTagInReq(P3.mfs(3), ReqTextLoc.CustomTextField(P3.descField), T.CustomTextField.Issue(1, ∅)),
    )

    def ucs() = test(p6)(
      delFRs,
      Event.UseCaseStepUpdate(13, UseCaseStepGD.ValueForTitle(Vector(T.UseCaseStep.Issue(1, ∅)))),
    )(
      Issue.IssueTagInReq(P6.uc1, ReqTextLoc.UseCaseStep(13), T.UseCaseStep.Issue(1, ∅)),
    )

    def deadCtx() = test(p6)(
      Event.UseCaseStepUpdate(13, UseCaseStepGD.ValueForTitle(Vector(T.UseCaseStep.Issue(1, ∅)))),
      Event.ReqFieldCustomTextSet(P3.mfs(3), P3.descField, Vector(T.CustomTextField.Issue(1, ∅))),
      Event.ReqsDelete(NonEmptySet(P3.frs(1), P3.frs(2), P6.uc1), ∅, ∅),
      Event.FieldCustomDelete(P3.descField),
    )()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object UninhabitableTagFieldTests {
    private implicit val filter = IssueFilter[Issue.UninhabitableTagField]

    def ko() = test(p3)(
      Event.TagDelete(P3.priTG),
    )(Issue.UninhabitableTagField(P3.priField))

    def deadField() = test(p3)(
      Event.FieldCustomDelete(P3.priField),
      Event.TagDelete(P3.priTG),
    )()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

//  debugTags(p3)

  override def tests = Tests {

    // Just testing sample projects' states without any modification.
    // In targeted tests below however, we modify projects to elicit specific issues.
    'sampleProjects {
      implicit val filter = IssueFilter.any

      'p3 - assertIssues(p3)(
        Issue.BlankCustomField(P3.frs(1), P3.priField),
        Issue.BlankCustomField(P3.frs(2), P3.priField),
        Issue.DeadRefInReq(P3.frs(2), ReqTextLoc.Title, ContentRef.ReqRef(P3.mfs(28))),
        Issue.IssueTagInReq(P3.frs(1), ReqTextLoc.Title, T.GenericReqTitle.Issue(1, ∅)),
        Issue.IssueTagInReq(P3.frs(2), ReqTextLoc.Title, T.GenericReqTitle.Issue(2, SampleProject3.inlineIssueDesc)),
      )

      'p4 - assertIssues(p4)(
        Issue.BlankCustomField(P4.frs(1), P4.priField),
        Issue.BlankCustomField(P4.frs(2), P4.priField),
        Issue.BlankCustomField(P4.uc1, P4.priField),
        Issue.DeadRefInReq(P4.frs(2), ReqTextLoc.Title, ContentRef.ReqRef(P4.mfs(28))),
        Issue.IssueTagInReq(P4.frs(1), ReqTextLoc.Title, T.GenericReqTitle.Issue(1, ∅)),
        Issue.IssueTagInReq(P4.frs(2), ReqTextLoc.Title, T.GenericReqTitle.Issue(2, SampleProject3.inlineIssueDesc)),
      )

      'p6 - assertIssues(p6)(
        Issue.BlankCustomField(P6.frs(1), P6.priField),
        Issue.BlankCustomField(P6.frs(2), P6.priField),
        Issue.BlankCustomField(P6.uc1, P6.priField),
        Issue.BlankUseCaseStep(UseCaseStepId(18)),
        Issue.BlankUseCaseStep(UseCaseStepId(19)),
        Issue.DeadRefInReq(P6.frs(2), ReqTextLoc.Title, ContentRef.ReqRef(P6.mfs(28))),
        Issue.DeadRefInReq(P6.uc1, ReqTextLoc.Title, ContentRef.UseCaseStepRef(16)),
        Issue.DeadRefInReq(P6.uc1, ReqTextLoc.Title, ContentRef.UseCaseStepRef(17)),
        Issue.IssueTagInReq(P6.frs(1), ReqTextLoc.Title, T.GenericReqTitle.Issue(1, ∅)),
        Issue.IssueTagInReq(P6.frs(2), ReqTextLoc.Title, T.GenericReqTitle.Issue(2, SampleProject3.inlineIssueDesc)),
      )
    }

    'Blank {
      import BlankTests._
      'title             - title()
      'ucs               - ucs()
      'emptyStepAndTitle - emptyStepAndTitle()
    }

    'BlankCustomField {
      import BlankCustomFieldTests._
      'notAllReqTypes - notAllReqTypes()
      'imps1          - imps1()
      'imps2          - imps2()
    }

    'ConflictingTag {
      import ConflictingTagTests._
      'ko           - ko()
      'deadTag      - deadTag()
      'deadTagGroup - deadTagGroup()
      'tagInText    - tagInText()
    }

    'DeadRef {
      import DeadRefTests._
      'issueDesc - issueDesc()
      'inRcg     - inRcg()
      'toRcg     - toRcg()
    }

    'DeadTag {
      import DeadTagTests._
      'ko - ko()
    }

    'EmptyCodeGroup {
      import EmptyCodeGroupTests._
      'ko            - ko()
      'deadChild     - deadChild()
      'deadCodeGroup - deadCodeGroup()
    }

    'IssueTags {
      import IssueTagTests._
      'rcg       - rcg()
      'deadIssue - deadIssue()
      'deadCtx   - deadCtx()
      'ok        - ok()
      'txtField  - txtField()
      'ucs       - ucs()
    }

    'UninhabitableTagField {
      import UninhabitableTagFieldTests._
      'ko        - ko()
      'deadField - deadField()
    }
  }
}
