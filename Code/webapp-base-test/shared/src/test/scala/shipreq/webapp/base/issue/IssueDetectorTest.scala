package shipreq.webapp.base.issue

import japgolly.microlibs.nonempty.{NonEmpty, NonEmptySet}
import japgolly.microlibs.stdlib_ext.MutableArray
import nyaya.util.Multimap
import scala.reflect.ClassTag
import sourcecode.Line
import shipreq.base.util.SetDiff
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.SampleProject3
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import utest._
import Event._
import shipreq.webapp.base.text.Text

object IssueDetectorTest extends TestSuite {

  import SampleProject3.{Values => P3, project => p3}

  private case class IssueFilter[I <: Issue]()(implicit val c: ClassTag[I])

  private def updateReqTags(id: ReqId)(del: ApplicableTagId*)(add: ApplicableTagId*): ReqTagsPatch =
    ReqTagsPatch(id, NonEmpty force SetDiff(removed = del.toSet, added = add.toSet))

  private def updateTagGroup(id: TagGroupId, mutexChildren: MutexChildren) = {
    import TagGroupGD._
    TagGroupUpdate(id, nev(MutexChildren(mutexChildren)))
  }

  private def assertIssues[I <: Issue](project: Project)(expected: I*)(implicit l: Line, f: IssueFilter[I]): Unit =
    assertIssuesOfType[I](project)(expected: _*)(l, f.c)

  private def assertIssuesOfType[I <: Issue](project: Project)(expected: I*)(implicit l: Line, c: ClassTag[I]): Unit =
    assertIssuesWithFilter(project, c.unapply(_).isDefined)(expected: _*)

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

    def ko() = assertIssues(applyEventsSuccessfully(p3,
      updateTagGroup(20, MutexChildren),
      ContentRestore(Set(1119), Set.empty),
      updateReqTags(1101)()(4),
      updateReqTags(1104)()(2, 24, 25),
      updateReqTags(1119)()(2, 3),
      ReqsDelete(NonEmptySet(1119), Set.empty, Vector.empty),
    ))(
      Issue.ConflictingTags(1101, 1, NonEmptySet(ReqTagLoc.Tags)),
      Issue.ConflictingTags(1104, 1, NonEmptySet(ReqTagLoc.Tags)),
      Issue.ConflictingTags(1104, 20, NonEmptySet(ReqTagLoc.Tags)),
      Issue.ConflictingTags(1107, 20, NonEmptySet(ReqTagLoc.Tags)),
    )

    def deadTag() = assertIssues(applyEventsSuccessfully(p3,
      updateReqTags(1101)()(4),
      TagDelete(4.AT),
    ))()

    def deadTagGroup() = assertIssues(applyEventsSuccessfully(p3,
      updateReqTags(1101)()(4),
      TagDelete(P3.priTG),
    ))()

    def tagInText() = {
      import Text.GenericReqTitle.TagRef
      assertIssues(applyEventsSuccessfully(p3,
        GenericReqTitleSet(1002, Vector(TagRef(P3.priHigh), TagRef(P3.priLow))), // no tags
        GenericReqTitleSet(1103, Vector(TagRef(P3.priLow))), // + highPri in tags
        GenericReqTitleSet(1104, Vector(TagRef(P3.priMed))), // + priMed in tags
      ))(
        Issue.ConflictingTags(1002, P3.priTG, NonEmptySet(ReqTextLoc.Title)),
        Issue.ConflictingTags(1103, P3.priTG, NonEmptySet(ReqTagLoc.Tags, ReqTextLoc.Title)),
      )
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object EmptyCodeGroupTests {
    private implicit val filter = IssueFilter[Issue.EmptyCodeGroup]

    private def demoId         = p3.content.reqCodes("demo").get.activeId.get.value.RCG
    private def demoWhateverId = p3.content.reqCodes("demo.whatever").get.activeId.get.value.ARC

    def ko() = assertIssues(applyEventsSuccessfully(p3,
      ReqCodesPatch(P3.frs(1), Set(demoWhateverId), Set.empty, Multimap.empty),
    ))(Issue.EmptyCodeGroup("demo"))

    def deadChild() = assertIssues(applyEventsSuccessfully(p3,
      ReqsDelete(NonEmptySet.one(P3.frs(1)), Set.empty, Vector.empty),
    ))(Issue.EmptyCodeGroup("demo"))

    def deadCodeGroup() = assertIssues(applyEventsSuccessfully(p3,
      ReqsDelete(NonEmptySet.one(P3.frs(1)), Set(demoId), Vector.empty),
    ))()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object UninhabitableTagFieldTests {
    private implicit val filter = IssueFilter[Issue.UninhabitableTagField]

    def ko() = assertIssues(applyEventsSuccessfully(p3,
      TagDelete(P3.priTG),
    ))(Issue.UninhabitableTagField(P3.priField))

    def deadField() = assertIssues(applyEventsSuccessfully(p3,
      FieldCustomDelete(P3.priField),
      TagDelete(P3.priTG),
    ))()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

//  debugTags(p3)

  override def tests = Tests {

    // Just testing sample projects' states without any modification.
    // In targeted tests below however, we modify projects to elicit specific issues.
    'sampleProjects {
      implicit val filter = IssueFilter[Issue]
      'p3 - assertIssues(p3)()
    }

    'ConflictingTag {
      import ConflictingTagTests._
      'ko           - ko()
      'deadTag      - deadTag()
      'deadTagGroup - deadTagGroup()
      'tagInText    - tagInText()
    }

    'EmptyCodeGroup {
      import EmptyCodeGroupTests._
      'ko            - ko()
      'deadChild     - deadChild()
      'deadCodeGroup - deadCodeGroup()
    }

    'UninhabitableTagField {
      import UninhabitableTagFieldTests._
      'ko        - ko()
      'deadField - deadField()
    }
  }
}
