package shipreq.webapp.base.issue

import japgolly.microlibs.nonempty.{NonEmpty, NonEmptySet}
import japgolly.microlibs.stdlib_ext.MutableArray
import nyaya.util.Multimap
import scala.reflect.ClassTag
import sourcecode.Line
import shipreq.base.util.SetDiff
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.text.{Text => T}
import utest._
import Event._

object IssueDetectorTest extends TestSuite {

  import SampleProject3.{Values => P3, project => p3}
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
  }

  private def updateReqTags(id: ReqId)(del: ApplicableTagId*)(add: ApplicableTagId*): ReqTagsPatch =
    ReqTagsPatch(id, NonEmpty force SetDiff(removed = del.toSet, added = add.toSet))

  private def updateTagGroup(id: TagGroupId, mutexChildren: MutexChildren) = {
    import TagGroupGD._
    TagGroupUpdate(id, nev(MutexChildren(mutexChildren)))
  }

  private def assertIssues(project: Project)(expected: Issue*)(implicit l: Line, f: IssueFilter): Unit =
    assertIssuesWithFilter(project, f.ok)(expected: _*)

  private def assertIssuesWithFilter(project: Project, filter: Issue => Boolean)(expected: Issue*)(implicit l: Line): Unit = {
    val it = IssueTracker(project)
    def is = it.issues.vector.iterator.map(_.issue).filter(filter)
    val actual = MutableArray(is).sortBySchwartzian(_.toString).to[List]
    val expect = MutableArray(expected).sortBySchwartzian(_.toString).to[List]
    if (actual.size != expect.size) { // TODO Fix assertSeq
      println("ACTUAL: ")
      actual.foreach(i => println("  - " + i))
      println("EXPECT: ")
      expect.foreach(i => println("  - " + i))
    }
    assertSeq("assertIssues", actual, expect)
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

  private object ConflictingTagTests {
    private implicit val filter = IssueFilter[Issue.ConflictingTags]

    def ko() = test(p3)(
      updateTagGroup(20, MutexChildren),
      ContentRestore(Set(1119), Set.empty),
      updateReqTags(1101)()(4),
      updateReqTags(1104)()(2, 24, 25),
      updateReqTags(1119)()(2, 3),
      ReqsDelete(NonEmptySet(1119), Set.empty, Vector.empty),
    )(
      Issue.ConflictingTags(1101, 1, NonEmptySet(ReqTagLoc.Tags)),
      Issue.ConflictingTags(1104, 1, NonEmptySet(ReqTagLoc.Tags)),
      Issue.ConflictingTags(1104, 20, NonEmptySet(ReqTagLoc.Tags)),
      Issue.ConflictingTags(1107, 20, NonEmptySet(ReqTagLoc.Tags)),
    )

    def deadTag() = test(p3)(
      updateReqTags(1101)()(4),
      TagDelete(4.AT),
    )()

    def deadTagGroup() = test(p3)(
      updateReqTags(1101)()(4),
      TagDelete(P3.priTG),
    )()

    def tagInText() = {
      import T.GenericReqTitle.TagRef
      test(p3)(
        GenericReqTitleSet(1002, Vector(TagRef(P3.priHigh), TagRef(P3.priLow))), // no tags
        GenericReqTitleSet(1103, Vector(TagRef(P3.priLow))), // + highPri in tags
        GenericReqTitleSet(1104, Vector(TagRef(P3.priMed))), // + priMed in tags
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
      ReqsDelete(NonEmptySet(P3.frs(2), P3.mfs(26)), Set.empty, Vector.empty),
    )()

    def inRcg() = test(p3)(
      CodeGroupUpdate(demoId, CodeGroupGD.ValueForTitle(Vector(T.CodeGroupTitle.ReqRef(P3.frs(2))))),
      ReqsDelete(NonEmptySet.one(P3.frs(2)), Set.empty, Vector.empty),
    )(
      Issue.DeadRefInRcg(demoId, ContentRef.ReqRef(P3.frs(2)))
    )

    def toRcg() = test(p3)(
      ContentEventTestHelp.createRCG(987, "haha.boop"),
      GenericReqTitleSet(1001, Vector(T.GenericReqTitle.CodeRef(987.RCG))),
      GenericReqTitleSet(1002, Vector(T.GenericReqTitle.CodeRef(demoId))),
      CodeGroupsDelete(NonEmptySet.one(demoId)),
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
      TagDelete(P3.priHigh),
      TagDelete(P3.priLow),
    )(
      Issue.DeadTag(P3.frs(1), ReqTextLoc.Title, P3.priHigh),
      Issue.DeadTag(P3.frs(1), ReqTextLoc.Title, P3.priLow),
    )
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object EmptyCodeGroupTests {
    private implicit val filter = IssueFilter[Issue.EmptyCodeGroup]

    def ko() = test(p3)(
      ReqCodesPatch(P3.frs(1), Set(demoWhateverId), Set.empty, Multimap.empty),
    )(Issue.EmptyCodeGroup(demoId))

    def deadChild() = test(p3)(
      ReqsDelete(NonEmptySet.one(P3.frs(1)), Set.empty, Vector.empty),
    )(Issue.EmptyCodeGroup(demoId))

    def deadCodeGroup() = test(p3)(
      ReqsDelete(NonEmptySet.one(P3.frs(1)), Set(demoId), Vector.empty),
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

    private val delFRs = ReqsDelete(NonEmptySet(P3.frs(1), P3.frs(2)), Set.empty, Vector.empty)

    def rcg() = test(p3)(
      delFRs,
      CodeGroupUpdate(demoId, CodeGroupGD.ValueForTitle(Vector(T.CodeGroupTitle.Issue(1, Vector.empty)))),
    )(
      Issue.IssueTagInRcg(demoId, T.CodeGroupTitle.Issue(1, Vector.empty)),
    )

    def deadIssue() = test(p3)(
      CustomIssueTypeDelete(1),
      CustomIssueTypeDelete(2),
    )(
      Issue.DeadIssueTagInReq(P3.frs(1), ReqTextLoc.Title, T.GenericReqTitle.Issue(1, Vector.empty)),
      Issue.DeadIssueTagInReq(P3.frs(2), ReqTextLoc.Title, T.GenericReqTitle.Issue(2, SampleProject3.inlineIssueDesc)),
    )

    def ok() = test(p3)(delFRs)()

    def txtField() = test(p3)(
      delFRs,
      ReqFieldCustomTextSet(P3.mfs(3), P3.descField, Vector(T.CustomTextField.Issue(1, Vector.empty))),
    )(
      Issue.IssueTagInReq(P3.mfs(3), ReqTextLoc.CustomTextField(P3.descField), T.CustomTextField.Issue(1, Vector.empty)),
    )

    def ucs() = test(p6)(
      delFRs,
      UseCaseStepUpdate(13, UseCaseStepGD.ValueForTitle(Vector(T.UseCaseStep.Issue(1, Vector.empty)))),
    )(
      Issue.IssueTagInReq(P6.uc1, ReqTextLoc.UseCaseStep(13), T.UseCaseStep.Issue(1, Vector.empty)),
    )

    def deadCtx() = test(p6)(
      UseCaseStepUpdate(13, UseCaseStepGD.ValueForTitle(Vector(T.UseCaseStep.Issue(1, Vector.empty)))),
      ReqFieldCustomTextSet(P3.mfs(3), P3.descField, Vector(T.CustomTextField.Issue(1, Vector.empty))),
      ReqsDelete(NonEmptySet(P3.frs(1), P3.frs(2), P6.uc1), Set.empty, Vector.empty),
      FieldCustomDelete(P3.descField),
    )()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object UninhabitableTagFieldTests {
    private implicit val filter = IssueFilter[Issue.UninhabitableTagField]

    def ko() = test(p3)(
      TagDelete(P3.priTG),
    )(Issue.UninhabitableTagField(P3.priField))

    def deadField() = test(p3)(
      FieldCustomDelete(P3.priField),
      TagDelete(P3.priTG),
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
        Issue.DeadRefInReq(P3.frs(2), ReqTextLoc.Title, ContentRef.ReqRef(P3.mfs(28))),
        Issue.IssueTagInReq(P3.frs(1), ReqTextLoc.Title, T.GenericReqTitle.Issue(1, Vector.empty)),
        Issue.IssueTagInReq(P3.frs(2), ReqTextLoc.Title, T.GenericReqTitle.Issue(2, SampleProject3.inlineIssueDesc)),
      )

      'p6 - assertIssues(p6)(
        Issue.DeadRefInReq(P6.frs(2), ReqTextLoc.Title, ContentRef.ReqRef(P6.mfs(28))),
        Issue.DeadRefInReq(P6.uc1, ReqTextLoc.Title, ContentRef.UseCaseStepRef(16)),
        Issue.DeadRefInReq(P6.uc1, ReqTextLoc.Title, ContentRef.UseCaseStepRef(17)),
        Issue.IssueTagInReq(P6.frs(1), ReqTextLoc.Title, T.GenericReqTitle.Issue(1, Vector.empty)),
        Issue.IssueTagInReq(P6.frs(2), ReqTextLoc.Title, T.GenericReqTitle.Issue(2, SampleProject3.inlineIssueDesc)),
      )
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
