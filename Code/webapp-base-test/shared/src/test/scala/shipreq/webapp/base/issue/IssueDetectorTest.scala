package shipreq.webapp.base.issue

import japgolly.microlibs.nonempty.{NonEmpty, NonEmptySet}
import nyaya.util.Multimap
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import sourcecode.Line
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.event._
import shipreq.webapp.base.event.RetiredGenericData._
import shipreq.webapp.base.test._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.text.{Text => T}
import utest._

object IssueDetectorTest extends TestSuite {

  import SampleProject3.{Values => P3, project => p3}
  import SampleProject4.{Values => P4, project => p4}
  import SampleProject6.{Values => P6, project => p6}
  import SampleProject7.{Values => P7, project => p7}

  private lazy val demoId         = p3.content.reqCodes.need("demo").activeId.get.value.RCG
  private lazy val demoWhateverId = p3.content.reqCodes.need("demo.whatever").activeId.get.value.ARC

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

  private def updateTagGroup(id: TagGroupId, exclusivity: Exclusivity) = {
    import TagGroupGD._
    Event.TagGroupUpdate(id, nev(Exclusivity(exclusivity)))
  }

  private def assertIssues(project: Project)(expected: IssueLite*)(implicit l: Line, f: IssueFilter): Unit =
    assertIssuesWithFilter(project, f.ok)(expected: _*)

  private def assertIssuesWithFilter(project: Project, filter: Issue => Boolean)(expected: IssueLite*)(implicit l: Line): Unit = {
    val it = IssueTracker(project)
    def actual = it.issues.vector.iterator.filter(filter).map(IssueLite.fromIssue)
    assertSeqIgnoreOrder("assertIssues", actual, expect = expected)
  }

  private def test(p: Project)(events: Event*)(expect: IssueLite*)(implicit l: Line, f: IssueFilter): Unit =
    assertIssues(applyEventsSuccessfully(p, events: _*))(expect: _*)

//  private def debugTags(project: Project): Project = {
//    println(project.config.tags.prettyPrint)
//    for (r <- project.content.reqs.reqIterator().toList.sortBy(_.id.value)) {
//      val tags = project.content.reqTags(r.id).map(_.value).toList.sorted.mkString(", ")
//      val isLive = r.live(project.config.reqTypes) is Dead
//      println(s"(#${r.id.value}) $tags${if (isLive) " [DEAD]" else ""}")
//    }
//    project
//  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object BlankTests {
    private implicit val filter = IssueFilter.collect {
      case _: Issue.BlankTitle | _: Issue.BlankUseCaseStep => ()
    }

    def title() = test(p4)(
      Event.GenericReqTitleSet(P6.frs(1), ∅),
      Event.UseCaseTitleSet(P6.uc1, ∅),
    )(
      IssueLite.BlankTitle(P6.frs(1)),
      IssueLite.BlankTitle(P6.uc1),
    )

    def ucSteps() = test(p4)(
      Event.UseCaseStepUpdate(10, UseCaseStepGD.ValueForTitle(∅)), // 0 -- no issue, uses UC title
      Event.UseCaseStepUpdate(11, UseCaseStepGD.ValueForTitle(∅)), // 0.1 (<= 0.3 dead)
      Event.UseCaseStepUpdate(12, UseCaseStepGD.ValueForTitle(∅)), // 0.2 <= 1.1
      Event.UseCaseStepUpdate(13, UseCaseStepGD.ValueForTitle(∅)), // 0.3 => 0.1 -- dead step
      Event.UseCaseStepUpdate(14, UseCaseStepGD.ValueForTitle(∅)), // 1
      Event.UseCaseStepUpdate(15, UseCaseStepGD.ValueForTitle(∅)), // 1.1 => 0.2
      Event.UseCaseStepDelete(13),
    )(
      IssueLite.BlankUseCaseStep(11),
      IssueLite.BlankUseCaseStep(14),
    )

    // This was a bug with flow between fields (i.e. NA -> E)
    def ucSteps2() = test(p6)(
      Event.UseCaseStepUpdate(14, UseCaseStepGD.ValueForTitle(∅)),
      Event.UseCaseStepUpdate(14, UseCaseStepGD.ValueForFlowOut(nesd()(18))),
    )(
      IssueLite.BlankUseCaseStep(UseCaseStepId(19)),
    )

    def emptyStepAndTitle() = test(p4)(
      Event.UseCaseTitleSet(P6.uc1, ∅),
      Event.UseCaseStepUpdate(10, UseCaseStepGD.ValueForTitle(∅)),
    )(
      IssueLite.BlankTitle(P6.uc1),
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
      IssueLite.BlankCustomField(frs(1), priField),
      IssueLite.BlankCustomField(frs(2), priField),
      IssueLite.BlankCustomField(uc1, priField),
      IssueLite.BlankCustomField(uc1, reporterField),
    )

    private def makeMfMandatoryForFrAndUc = {
      val I = CustomImpFieldGDv1
      val frAndUc = onlyReqTypes(fr, StaticReqType.UseCase)
      Event.FieldCustomImpUpdateV1(mfField, I.nev(I.ValueForMandatory(Mandatory), I.ValueForApplicableReqTypes(frAndUc)))
    }

    // fr1 <- mf12,19
    // fr2 <-
    // uc1 <- fr1
    def imps1() = test(p4)(
      makeMfMandatoryForFrAndUc,
      Event.ReqImplicationsPatch(frs(2), Backwards, nesd[ReqId](mfs(1), mfs(13), mfs(22), frs(1))()),
      Event.ReqImplicationsPatch(uc1, Backwards, nesd[ReqId]()(frs(1))),
    )(
      IssueLite.BlankCustomField(frs(1), priField),
      IssueLite.BlankCustomField(frs(2), priField),
      IssueLite.BlankCustomField(uc1, priField),
      IssueLite.BlankCustomField(frs(2), mfField),
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
      IssueLite.BlankCustomField(frs(1), priField),
      IssueLite.BlankCustomField(uc1, priField),
      IssueLite.BlankCustomField(uc1, mfField),
    )
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object ConflictingTagTests {
    private implicit val filter = IssueFilter[Issue.ConflictingTags]

    def ko() = test(p3)(
      updateTagGroup(20, Exclusive),
      Event.ContentRestore(Set(1119), ∅),
      updateReqTags(1101)()(4),
      updateReqTags(1104)()(2, 24, 25),
      updateReqTags(1119)()(2, 3),
      Event.ReqsDelete(NonEmptySet(1119), ∅, ∅),
    )(
      IssueLite.ConflictingTags(1101, 1, NonEmptySet(Location.Tags)),
      IssueLite.ConflictingTags(1104, 1, NonEmptySet(Location.Tags)),
      IssueLite.ConflictingTags(1104, 20, NonEmptySet(Location.Tags)),
      IssueLite.ConflictingTags(1107, 20, NonEmptySet(Location.Tags)),
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
        Event.GenericReqTitleSet(1002, ArraySeq(TagRef(P3.priHigh), TagRef(P3.priLow))), // no tags
        Event.GenericReqTitleSet(1103, ArraySeq(TagRef(P3.priLow))), // + highPri in tags
        Event.GenericReqTitleSet(1104, ArraySeq(TagRef(P3.priMed))), // + priMed in tags
      )(
        IssueLite.ConflictingTags(1002, P3.priTG, NonEmptySet(Location.Text.Title)),
        IssueLite.ConflictingTags(1103, P3.priTG, NonEmptySet(Location.Tags, Location.Text.Title)),
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
      Event.CodeGroupUpdate(demoId, CodeGroupGD.ValueForTitle(ArraySeq(T.CodeGroupTitle.ReqRef(P3.frs(2))))),
      Event.ReqsDelete(NonEmptySet.one(P3.frs(2)), ∅, ∅),
    )(
      IssueLite.DeadRefInRcg(demoId, ContentRef.ReqRef(P3.frs(2)))
    )

    def toRcg() = test(p3)(
      ContentEventTestHelp.createRCG(987, "haha.boop"),
      Event.GenericReqTitleSet(1001, ArraySeq(T.GenericReqTitle.CodeRef(987.RCG))),
      Event.GenericReqTitleSet(1002, ArraySeq(T.GenericReqTitle.CodeRef(demoId))),
      Event.CodeGroupsDelete(NonEmptySet.one(demoId)),
    )(
      IssueLite.DeadRefInReq(1002, Location.Text.Title, ContentRef.CodeRef(demoId)),
    )
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object DeadTagTests {
    private implicit val filter = IssueFilter[Issue.DeadTag]

    import T.GenericReqTitle.TagRef

    def ko() = test(p3)(
      Event.GenericReqTitleSet(P3.frs(1), ArraySeq(TagRef(P3.priHigh), TagRef(P3.priMed), TagRef(P3.priLow))),
      Event.TagDelete(P3.priHigh),
      Event.TagDelete(P3.priLow),
    )(
      IssueLite.DeadTag(P3.frs(1), Location.Text.Title, P3.priHigh),
      IssueLite.DeadTag(P3.frs(1), Location.Text.Title, P3.priLow),
    )
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object EmptyCodeGroupTests {
    private implicit val filter = IssueFilter[Issue.EmptyCodeGroup]

    def ko() = test(p3)(
      Event.ReqCodesPatch(P3.frs(1), Set(demoWhateverId), ∅, Multimap.empty),
    )(IssueLite.EmptyCodeGroup(demoId))

    def deadChild() = test(p3)(
      Event.ReqsDelete(NonEmptySet.one(P3.frs(1)), ∅, ∅),
    )(IssueLite.EmptyCodeGroup(demoId))

    def deadCodeGroup() = test(p3)(
      Event.ReqsDelete(NonEmptySet.one(P3.frs(1)), Set(demoId), ∅),
    )()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object FieldDefaultTagDeadTests {
    private implicit val filter = IssueFilter[Issue.FieldDefaultTagDead]
    import P7._

    def ko() = test(p7)()(
      IssueLite.FieldDefaultTagDead(statusField, uat, Set(brs(1), brs(2))),
      IssueLite.FieldDefaultTagDead(statusField, uat2, Set(frs(1), frs(2))),
    )

    def otherwise() = test(p7)(
      Event.FieldCustomTagUpdate(statusField, CustomTagFieldGD(FieldReqTypeRules.defaultTo(uat3).notApplicable(mf))),
    )(
      IssueLite.FieldDefaultTagDead(statusField, uat3, Set(brs(1), brs(2), frs(1), frs(2), uc2)),
    )

    def liveOnly() = test(p7)(
      Event.FieldCustomDelete(statusField),
    )()

    def unrelated() = test(p7)(
      Event.FieldCustomDelete(statusField),
      Event.TagDelete(priMed),
    )(
      IssueLite.FieldDefaultTagDead(priField, priMed, Set(brs(1), brs(2), brs(3))),
    )
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object FieldDefaultTagNotApplicableTests {
    private implicit val filter = IssueFilter[Issue.FieldDefaultTagNotApplicable]
    import P7._

    def ok() = test(p7)(
      Event.ApplicableTagUpdate(priHigh, ApplicableTagGD.ValueForApplicableReqTypes(onlyReqTypes(co))),
    )()

    def specific() = test(p7)(
      Event.ApplicableTagUpdate(priMed, ApplicableTagGD.ValueForApplicableReqTypes(onlyReqTypes(co))),
    )(
      IssueLite.FieldDefaultTagNotApplicable(priField, priMed, br),
    )

    def otherwise() = test(p7)(
      Event.FieldCustomTagUpdate(priField, CustomTagFieldGD(
        FieldReqTypeRules.defaultTo(priMed).notApplicable(mf, dd).optional(fr).mandatory(uc, si))),
      Event.ApplicableTagUpdate(priMed, ApplicableTagGD.ValueForApplicableReqTypes(onlyReqTypes(co))),
    )(
      IssueLite.FieldDefaultTagNotApplicable(priField, priMed, br),
    )

    def liveReqTypeOnly() = test(p7)(
      Event.ApplicableTagUpdate(priMed, ApplicableTagGD.ValueForApplicableReqTypes(onlyReqTypes(co))),
      Event.CustomReqTypeDeleteSoft(br)
    )()

    def liveFieldOnly() = test(p7)(
      Event.ApplicableTagUpdate(priMed, ApplicableTagGD.ValueForApplicableReqTypes(onlyReqTypes(co))),
      Event.FieldCustomDelete(priField),
    )()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object FieldDefaultTagUnrelatedTests {
    private implicit val filter = IssueFilter[Issue.FieldDefaultTagUnrelated]
    import P7._

    def ko() = test(p7)()(
      IssueLite.FieldDefaultTagUnrelated(relField, priMed),
      IssueLite.FieldDefaultTagUnrelated(verField, priLow),
    )

    def deadTag() = test(p7)(
      Event.TagDelete(priMed),
      Event.TagDelete(priLow),
    )(
      IssueLite.FieldDefaultTagUnrelated(relField, priMed),
      IssueLite.FieldDefaultTagUnrelated(verField, priLow),
    )

    def liveFieldOnly() = test(p7)(
      Event.FieldCustomDelete(relField),
      Event.FieldCustomDelete(verField),
    )()

    def liveReqTypeOnly() = test(p7)(
      Event.CustomReqTypeDeleteSoft(co),
      Event.CustomReqTypeDeleteSoft(mf),
    )(
      IssueLite.FieldDefaultTagUnrelated(verField, priLow),
    )
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object ImplicationRequiredTests {
    private implicit val filter = IssueFilter[Issue.ImplicationRequired]

    import P4._

    // ∅ -> fr1 -> fr2
    def ko() = test(p4)(
      Event.ReqImplicationsPatch(frs(1), Backwards, nesd[ReqId](mfs(12), mfs(19))()),
      Event.ReqImplicationsPatch(frs(2), Backwards, nesd[ReqId](mfs(1), mfs(13), mfs(22))()),
    )(
      IssueLite.ImplicationRequired(frs(1)),
    )
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
      Event.CodeGroupUpdate(demoId, CodeGroupGD.ValueForTitle(ArraySeq(T.CodeGroupTitle.Issue(1, ∅)))),
    )(
      IssueLite.IssueTagInRcg(demoId, T.CodeGroupTitle.Issue(1, ∅)),
    )

    def deadIssue() = test(p3)(
      Event.CustomIssueTypeDelete(1),
      Event.CustomIssueTypeDelete(2),
    )(
      IssueLite.DeadIssueTagInReq(P3.frs(1), Location.Text.Title, T.GenericReqTitle.Issue(1, ∅)),
      IssueLite.DeadIssueTagInReq(P3.frs(2), Location.Text.Title, T.GenericReqTitle.Issue(2, SampleProject3.inlineIssueDesc)),
    )

    def ok() = test(p3)(delFRs)()

    def txtField() = test(p3)(
      delFRs,
      Event.ReqFieldCustomTextSet(P3.mfs(3), P3.descField, ArraySeq(T.CustomTextField.Issue(1, ∅))),
    )(
      IssueLite.IssueTagInReq(P3.mfs(3), Location.Text.CustomTextField(P3.descField), T.CustomTextField.Issue(1, ∅)),
    )

    def ucs() = test(p6)(
      delFRs,
      Event.UseCaseStepUpdate(13, UseCaseStepGD.ValueForTitle(ArraySeq(T.UseCaseStep.Issue(1, ∅)))),
    )(
      IssueLite.IssueTagInReq(P6.uc1, Location.Text.UseCaseStep(13), T.UseCaseStep.Issue(1, ∅)),
    )

    def deadCtx() = test(p6)(
      Event.UseCaseStepUpdate(13, UseCaseStepGD.ValueForTitle(ArraySeq(T.UseCaseStep.Issue(1, ∅)))),
      Event.ReqFieldCustomTextSet(P3.mfs(3), P3.descField, ArraySeq(T.CustomTextField.Issue(1, ∅))),
      Event.ReqsDelete(NonEmptySet(P3.frs(1), P3.frs(2), P6.uc1), ∅, ∅),
      Event.FieldCustomDelete(P3.descField),
    )()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object NonApplicableFieldTests {
    private implicit val filter = IssueFilter[Issue.NonApplicableField]

    def onlyLiveFields() = test(p7)(
      Event.FieldCustomDelete(P7.alternativesField)
    )()

    def onlyDeadApplicable() = test(p7)(
      Event.FieldCustomTextUpdate(P7.bizJustField, CustomTextFieldGD("X", FieldReqTypeRules.mandatory.optional(P7.si))),
    )(IssueLite.NonApplicableField(P7.alternativesField))

    def noRules() = test(p7)(
      Event.FieldCustomTextUpdate(P7.alternativesField, CustomTextFieldGD("X", FieldReqTypeRules.notApplicable)),
    )(IssueLite.NonApplicableField(P7.alternativesField))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object NonApplicableTagTests {
    private implicit val filter = IssueFilter[Issue.NonApplicableTag]

    import T.GenericReqTitle.TagRef
    import P3._

    def ko() = test(p3)(
      Event.GenericReqTitleSet(frs(1), ArraySeq(TagRef(priHigh), TagRef(priMed))),
      Event.ApplicableTagUpdate(priHigh, ApplicableTagGD.ValueForApplicableReqTypes(onlyReqTypes(uc))),
    )(
      IssueLite.NonApplicableTag(frs(1), Location.Text.Title, priHigh),
    )

    def dead() = test(p3)(
      Event.GenericReqTitleSet(frs(1), ArraySeq(TagRef(priHigh), TagRef(priMed))),
      Event.ApplicableTagUpdate(priHigh, ApplicableTagGD.ValueForApplicableReqTypes(onlyReqTypes(uc))),
      Event.TagDelete(priHigh),
    )()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object UninhabitableTagFieldTests {
    private implicit val filter = IssueFilter[Issue.UninhabitableTagField]

    def ko() = test(p3)(
      Event.TagDelete(P3.priTG),
    )(IssueLite.UninhabitableTagField(P3.priField))

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
    "sampleProjects" - {
      implicit val filter = IssueFilter.any

      "p3" - assertIssues(p3)(
        IssueLite.BlankCustomField(P3.frs(1), P3.priField),
        IssueLite.BlankCustomField(P3.frs(2), P3.priField),
        IssueLite.DeadRefInReq(P3.frs(2), Location.Text.Title, ContentRef.ReqRef(P3.mfs(28))),
        IssueLite.IssueTagInReq(P3.frs(1), Location.Text.Title, T.GenericReqTitle.Issue(1, ∅)),
        IssueLite.IssueTagInReq(P3.frs(2), Location.Text.Title, T.GenericReqTitle.Issue(2, SampleProject3.inlineIssueDesc)),
      )

      "p4" - assertIssues(p4)(
        IssueLite.BlankCustomField(P4.frs(1), P4.priField),
        IssueLite.BlankCustomField(P4.frs(2), P4.priField),
        IssueLite.BlankCustomField(P4.uc1, P4.priField),
        IssueLite.DeadRefInReq(P4.frs(2), Location.Text.Title, ContentRef.ReqRef(P4.mfs(28))),
        IssueLite.IssueTagInReq(P4.frs(1), Location.Text.Title, T.GenericReqTitle.Issue(1, ∅)),
        IssueLite.IssueTagInReq(P4.frs(2), Location.Text.Title, T.GenericReqTitle.Issue(2, SampleProject3.inlineIssueDesc)),
      )

      "p6" - assertIssues(p6)(
        IssueLite.BlankCustomField(P6.frs(1), P6.priField),
        IssueLite.BlankCustomField(P6.frs(2), P6.priField),
        IssueLite.BlankCustomField(P6.uc1, P6.priField),
        IssueLite.BlankCustomField(P6.uc2, P6.priField),
        IssueLite.BlankUseCaseStep(UseCaseStepId(18)),
        IssueLite.BlankUseCaseStep(UseCaseStepId(19)),
        IssueLite.DeadRefInReq(P6.frs(2), Location.Text.Title, ContentRef.ReqRef(P6.mfs(28))),
        IssueLite.DeadRefInReq(P6.uc1, Location.Text.Title, ContentRef.UseCaseStepRef(16)),
        IssueLite.DeadRefInReq(P6.uc1, Location.Text.Title, ContentRef.UseCaseStepRef(17)),
        IssueLite.IssueTagInReq(P6.frs(1), Location.Text.Title, T.GenericReqTitle.Issue(1, ∅)),
        IssueLite.IssueTagInReq(P6.frs(2), Location.Text.Title, T.GenericReqTitle.Issue(2, SampleProject3.inlineIssueDesc)),
      )
    }

    "Blank" - {
      import BlankTests._
      "title"             - title()
      "ucSteps"           - ucSteps()
      "ucSteps2"          - ucSteps2()
      "emptyStepAndTitle" - emptyStepAndTitle()
    }

    "BlankCustomField" - {
      import BlankCustomFieldTests._
      "notAllReqTypes" - notAllReqTypes()
      "imps1"          - imps1()
      "imps2"          - imps2()
    }

    "ConflictingTag" - {
      import ConflictingTagTests._
      "ko"           - ko()
      "deadTag"      - deadTag()
      "deadTagGroup" - deadTagGroup()
      "tagInText"    - tagInText()
    }

    "DeadRef" - {
      import DeadRefTests._
      "issueDesc" - issueDesc()
      "inRcg"     - inRcg()
      "toRcg"     - toRcg()
    }

    "DeadTag" - {
      import DeadTagTests._
      "ko" - ko()
    }

    "EmptyCodeGroup" - {
      import EmptyCodeGroupTests._
      "ko"            - ko()
      "deadChild"     - deadChild()
      "deadCodeGroup" - deadCodeGroup()
    }

    "FieldDefaultTagDead" - {
      import FieldDefaultTagDeadTests._
      "ko"        - ko()
      "otherwise" - otherwise()
      "unrelated" - unrelated()
      "liveOnly"  - liveOnly()
    }

    "FieldDefaultTagNotApplicable" - {
      import FieldDefaultTagNotApplicableTests._
      "ok"              - ok()
      "specific"        - specific()
      "otherwise"       - otherwise()
      "liveReqTypeOnly" - liveReqTypeOnly()
      "liveFieldOnly"   - liveFieldOnly()
    }

    "FieldDefaultTagUnrelated" - {
      import FieldDefaultTagUnrelatedTests._
      "ko"              - ko()
      "deadTag"         - deadTag()
      "liveFieldOnly"   - liveFieldOnly()
      "liveReqTypeOnly" - liveReqTypeOnly()
    }

    "ImplicationRequired" - {
      import ImplicationRequiredTests._
      "ko" - ko()
    }

    "IssueTags" - {
      import IssueTagTests._
      "rcg"       - rcg()
      "deadIssue" - deadIssue()
      "deadCtx"   - deadCtx()
      "ok"        - ok()
      "txtField"  - txtField()
      "ucs"       - ucs()
    }

    "NonApplicableField" - {
      import NonApplicableFieldTests._
      "onlyLiveFields"     - onlyLiveFields()
      "onlyDeadApplicable" - onlyDeadApplicable()
      "noRules"            - noRules()
    }

    "NonApplicableTag" - {
      import NonApplicableTagTests._
      "ko"   - ko()
      "dead" - dead()
    }

    "UninhabitableTagField" - {
      import UninhabitableTagFieldTests._
      "ko"        - ko()
      "deadField" - deadField()
    }
  }
}
