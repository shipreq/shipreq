package com.beardedlogic.usecase
package test

import org.scalatest.matchers.{ShouldMatchers, Matcher, MatchResult}
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import lib.NodeUtils._
import lib.StepTree._
import lib.TypeTags._
import lib.UseCaseCtx
import lib.msg.MessageCentre
import net.liftweb.http.LiftRules

/**
 * @since 30/04/2013
 */
trait TestHelpers extends MockitoSugar with ShouldMatchers {

  if (!LiftRules.doneBoot) (new bootstrap.liftweb.Boot).configureLift

  def eventually(cond: => Any) {
    val test = (sleep: Int) => try { cond; true } catch { case _: Throwable => Thread.sleep(sleep); false }
    if (!test(10))
      if (!test(10))
        if (!test(20))
          if (!test(20))
            if (!test(50))
              if (!test(100))
                cond
  }

  def eventuallyIf(wait: Boolean)(cond: => Any) { if (wait) eventually(cond) else cond }

  def matchTree(expected: List[StepNode]) = TestHelpers.TreeMatcher(expected)

  def mockUseCaseCtx: UseCaseCtx = {
    val u = mock[UseCaseCtx]
    when(u.msgCentre).thenReturn(mock[MessageCentre])
    when(u.stepLabelMapProvider).thenReturn(() => Option(u.stepLabelMap).getOrElse(Map.empty[String,String]))
    when(u.number).thenReturn(1: Short)
    u
  }

  def mockUseCaseCtx(stepLabelMap: Map[String,String]): UseCaseCtx = {
    val u = mockUseCaseCtx
    when(u.stepLabelMap).thenReturn(stepLabelMap)
    u
  }
}

object TestHelpers extends TestHelpers {

  case class TreeMatcher(expected: List[StepNode]) extends Matcher[List[StepNode]] {
    def apply(actual: List[StepNode]): MatchResult = {
      val result = removeIds(actual) == removeIds(expected)
      MatchResult(result,
        "Trees didn't match.\n" + inspectTrees("EXPECTED", expected, "ACTUAL", actual),
        "Trees matched but shouldn't have.\n" + inspectTree(actual))
    }
  }

  /**
   * Old way of generating trees.
   */
  object TreeDSL {
    import lib.StepLabels.LabelMakers

    case class NC(val node: String, val children: List[NC])
    def $(nodes: NC*) = nodes.toList
    implicit def nodeWithoutChildren(n: String) = NC(n, Nil)
    implicit class StringAsNode(val s: String) { def ~>(children: List[NC]) = NC(s, children) }
    implicit class NCListExt(val ncs: List[NC]) {
      val regex = """^(\S+?)/(\S+)$""".r
      val labelSplit = """^(\S+\.)?([^\.]+)$""".r
      def toStepNodes: List[StepNode] = toStepNodes(0, "", true)
      def toStepNodesN: List[StepNode] = toStepNodes(0, "", false)
      def toStepNodes(lvl: Int, idPrefix: String, genIds: Boolean): List[StepNode] = ncs.map { nc =>
        val (lbl, txt) = if (regex.pattern.matcher(nc.node).matches) {
          val regex(l, t) = nc.node; (l, t)
        } else
          (nc.node, "Step:" + nc.node)
        val id = idPrefix + lbl
        val ch = nc.children.toStepNodes(lvl + 1, id + ".", genIds)
        val labelSplit(lblPrefix, lblSuffix) = lbl
        val lblIndex = LabelMakers(lvl)(lblSuffix)
        val id2 = if (genIds) id else null
        StepNode(id2.asLocalStepId, lvl, lblIndex, Step(txt), ch)
      }
    }

    type NodeChange = Tuple2[String, List[NC]]
    def changeChildren(nodes: List[StepNode], changes: NodeChange*): List[StepNode] = nodes.map { n =>
      val matches = for ((id, c) <- changes if id == n.id) yield c
      val ch = if (matches.isEmpty) n.children else matches(0).toStepNodes
      n.copy(children = changeChildren(ch, changes: _*))
    }

  }
}