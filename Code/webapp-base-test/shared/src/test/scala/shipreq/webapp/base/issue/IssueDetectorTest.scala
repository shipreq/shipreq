package shipreq.webapp.base.issue

import japgolly.microlibs.nonempty.{NonEmpty, NonEmptySet}
import japgolly.microlibs.stdlib_ext.MutableArray
import shipreq.base.util.SetDiff
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.SampleProject3
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import utest._
import Event._

object IssueDetectorTest extends TestSuite {

  import SampleProject3.{project => p3}

  private def updateReqTags(id: ReqId)(del: ApplicableTagId*)(add: ApplicableTagId*): ReqTagsPatch =
    ReqTagsPatch(id, NonEmpty force SetDiff(removed = del.toSet, added = add.toSet))

  private def updateTagGroup(id: TagGroupId, mutexChildren: MutexChildren) = {
    import TagGroupGD._
    TagGroupUpdate(id, nev(MutexChildren(mutexChildren)))
  }

  private object ConflictingTagTests {
    def debug(project: Project): Project = {
      println(project.config.tags.prettyPrint)
      for (r <- project.content.reqs.reqIterator.toList.sortBy(_.id.value)) {
        val tags = project.content.reqTags(r.id).map(_.value).toList.sorted.mkString(", ")
        val isLive = r.live(project.config.reqTypes) is Dead
        println(s"(#${r.id.value}) $tags${if (isLive) " [DEAD]" else ""}")
      }
      project
    }

    def test(project: Project)(expected: Issue*): Unit = {
      val i = IssueDetectors.ConflictingTagDetector.instance
      val is = i.init(project)
      val actual = MutableArray(is).sortBySchwartzian(_.toString).to[List]
      val expect = MutableArray(expected).sortBySchwartzian(_.toString).to[List]
      assertSeq(actual, expect)
    }

    def ok() = test(p3)()

    def ko() = test(applyEventsSuccessfully(p3,
      updateTagGroup(20, MutexChildren),
      ContentRestore(Set(1119), Set.empty),
      updateReqTags(1101)()(4),
      updateReqTags(1104)()(2, 24, 25),
      updateReqTags(1119)()(2, 3),
      ReqsDelete(NonEmptySet(1119), Set.empty, Vector.empty),
    ))(
      Issue.ConflictingTags(1101, 1),
      Issue.ConflictingTags(1104, 1),
      Issue.ConflictingTags(1104, 20),
      Issue.ConflictingTags(1107, 20),
    )

    def deadTag() = test(applyEventsSuccessfully(p3,
      updateReqTags(1101)()(4),
      TagDelete(4.AT),
    ))()

    def deadTagGroup() = test(applyEventsSuccessfully(p3,
      updateReqTags(1101)()(4),
      TagDelete(1.TG),
    ))()
  }

  override def tests = Tests {

    'ConflictingTag {
      import ConflictingTagTests._
      'ok           - ok()
      'ko           - ko()
      'deadTag      - deadTag()
      'deadTagGroup - deadTagGroup()
    }

  }
}
