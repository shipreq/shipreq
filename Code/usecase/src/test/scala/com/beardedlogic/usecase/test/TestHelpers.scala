package com.beardedlogic.usecase
package test

import net.liftweb.http.LiftRules
import org.scalatest.matchers.{ShouldMatchers, Matcher, MatchResult}
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import lib.{BiMap, StepNode}
import NodeUtils._
import lib.tree._
import lib.field._
import TreeOps._
import lib.field.CourseFields
import lib.TypeTags._
import lib.UseCaseCtx
import lib.msg.MessageCentre
import model.FieldKey

/**
 * @since 30/04/2013
 */
trait TestHelpers extends MockitoSugar with ShouldMatchers {
  import TestHelpers._

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

  def any[T](implicit m: Manifest[T]) = org.mockito.Matchers.any(m.runtimeClass.asInstanceOf[Class[T]])

  def matchTree(expected: List[StepNodeWithText]) = TestHelpers.TreeMatcher(expected)

  def mockUseCaseCtx: UseCaseCtx = {
    val u = mock[UseCaseCtx]
    when(u.msgCentre).thenReturn(mock[MessageCentre])
    when(u.stepLabelMapProvider).thenReturn(() => Option(u.stepLabelMap).getOrElse(BiMap.empty))
    when(u.number).thenReturn(1: Short)
    u
  }

  def mockUseCaseCtx(stepLabelMap: Map[String @@ LocalStepId,String]): UseCaseCtx = {
    val u = mockUseCaseCtx
    when(u.stepLabelMap).thenReturn(BiMap(stepLabelMap))
    u
  }

  def buildStateForTest(nodes: List[StepNodeWithText]): List[StepState] = {
    val cf = new NormalAndAlternateCourseFields(new UseCaseCtx(null), mock[FieldKey])
    cf.buildStateForTest(nodes)
  }
}

object TestHelpers extends TestHelpers {

  implicit class CourseFieldExt(val cf: CourseFields) extends AnyVal {

    def coursesWithText: List[StepNodeWithText] = convertNodeTree[StepNode, StepNodeWithText](cf.courses, {
      case (n, lvl, lbl, children) =>
        val savedSteps = try cf.ucCtx.savedSteps.ba catch {case _: Throwable => Map.empty[String @@ LocalStepId, Long_StepDataId]}
        val txt = cf.test__textFields.get(n.id).map(_.textWithNormalisedRefs(savedSteps)).getOrElse("".hasNormalisedRefs)
        StepNodeWithText(n.id, lvl, lbl, txt, children)
    }, cf.startingLabelIndices.startingLabelIndex _)

    def setCoursesWithTextAndInit(nodes: List[StepNodeWithText]) {
      cf.courses = nodes.map(_.toStepNode)
      cf.init
      val savedSteps = BiMap.empty[Long_StepDataId, String @@ LocalStepId]
      nodes.foreachNode(n => cf.test__textFields(n.id).setTextFromLoad(n.text.hasNormalisedRefs, savedSteps))
    }

    def buildStateForTest(nodes: List[StepNodeWithText]): List[StepState] =
      convertNodeTree[StepNodeWithText, StepState](nodes, {
        case (n, level, index, children) => StepState(n.id, n.text.hasNormalisedRefs, children)
      }, cf.startingLabelIndices.startingLabelIndex _)

    def setCoursesWithText(nodes: List[StepNodeWithText]) {
      cf.setState(CourseFieldState(cf.buildStateForTest(nodes)))()
    }
  }

  case class TreeMatcher(expected: List[StepNodeWithText]) extends Matcher[List[StepNodeWithText]] {
    def apply(actual: List[StepNodeWithText]): MatchResult = {
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
      def toStepNodes: List[StepNodeWithText] = toStepNodes(0, "", true)
      def toStepNodesN: List[StepNodeWithText] = toStepNodes(0, "", false)
      def toStepNodes(lvl: Int, idPrefix: String, genIds: Boolean): List[StepNodeWithText] = ncs.map { nc =>
        val (lbl, txt) = if (regex.pattern.matcher(nc.node).matches) {
          val regex(l, t) = nc.node; (l, t)
        } else
          (nc.node, "Step:" + nc.node)
        val id = idPrefix + lbl
        val ch = nc.children.toStepNodes(lvl + 1, id + ".", genIds)
        val labelSplit(lblPrefix, lblSuffix) = lbl
        val lblIndex = LabelMakers(lvl)(lblSuffix)
        val id2 = if (genIds) id else null
        StepNodeWithText(id2.asLocalStepId, lvl, lblIndex, txt, ch)
      }
    }

    type NodeChange = Tuple2[String, List[NC]]
    def changeChildren(nodes: List[StepNodeWithText], changes: NodeChange*): List[StepNodeWithText] = nodes.map { n =>
      val matches = for ((id, c) <- changes if id == n.id) yield c
      val ch = if (matches.isEmpty) n.children else matches(0).toStepNodes
      n.copy(children = changeChildren(ch, changes: _*))
    }

  }
}
