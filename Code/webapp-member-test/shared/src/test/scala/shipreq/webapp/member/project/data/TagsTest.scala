package shipreq.webapp.member.project.data

import japgolly.microlibs.utils.Memo
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util._
import shipreq.webapp.member.test.project.UnsafeTypes._
import sourcecode.Line
import utest._

object TagsTest extends TestSuite {

  private def tg(id: TagGroupId, live: Live)(children: TagId*): TagInTree =
    TagInTree(TagGroup(id, s"Group.${id.value}", None, NonExclusive, live), children.toVector)

  private def at(id: ApplicableTagId, live: Live)(children: TagId*): TagInTree =
    TagInTree(ApplicableTag(id, HashRefKey(s"tag.${id.value}"), None, None, ApplicableReqTypes.empty, live), children.toVector)

  private def mkTags(tts: TagInTree*): Tags =
    Tags(TagTree.empty.addAll(tts: _*))

  private val indent: Int => String =
    Memo.int("." * _)

  private def assertFlatRows(actual: Vector[FlatTag], expect: String)(implicit l: Line): Unit = {
    val a = actual.iterator.map(t => s"${indent(t.depth)}${t.tag.id.value}").mkString("\n")
    assertMultiline(a, expect.trim)
  }

  private def assertRecursiveIterator(actual: RecursiveTagIterator, expect: String)(implicit l: Line): Unit = {
    val sb = new StringBuilder

    def add(lvl: Int, id: TagId): Unit = {
      if (sb.nonEmpty) sb.append('\n')
      sb.append(indent(lvl))
      sb.append(id.value)
    }

    def go(it: RecursiveTagIterator): Unit = {
      val level = it.level
      for (t <- it.tagGroupIterator()) {
        add(level, t.id)
        go(it.nextLevel(t))
      }
      for (t <- it.applicableTagIterator())
        add(level, t.id)
    }
    go(actual)

    val a = sb.toString()
    assertMultiline(a, expect.trim)
  }

  private def testTags(tags: Tags, expectedLiveTree: String, expectedDeadTree: String)(implicit l: Line): Unit = {
    // Dead
    assertFlatRows(tags.flatRowsUnfiltered, expectedDeadTree)
    assertRecursiveIterator(tags.recursiveIterator(ShowDead), expectedDeadTree)

    // Live
    assertFlatRows(tags.flatRowsLive, expectedLiveTree)
    assertRecursiveIterator(tags.recursiveIterator(HideDead), expectedLiveTree)
  }

  override def tests = Tests {

    "treeViews" - {

      val tags = mkTags(
          tg(1, Dead)(3.AT, 4.AT),
          tg(2, Live)(),
          at(3, Live)(),
          at(4, Dead)(),
          at(5, Live)(),
        )

      testTags(tags,
        s"""
           |2
           |5
           |""".stripMargin,
        s"""
           |1
           |.3
           |.4
           |2
           |5
           |""".stripMargin)
    }

  }
}
