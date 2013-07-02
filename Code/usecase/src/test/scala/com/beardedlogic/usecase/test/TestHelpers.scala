package com.beardedlogic.usecase
package test

import java.io.File
import org.apache.commons.io.FileUtils
import org.scalatest.matchers.{ShouldMatchers, Matcher, MatchResult}
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import net.liftweb.common.Empty
import net.liftweb.http.{S, LiftSession, LiftRules}
import net.liftweb.util.StringHelpers

import lib._
import NodeUtils._
import lib.tree._
import lib.field._
import TreeOps._
import lib.TypeTags._
import lib.msg._
import model._

/**
 * @since 30/04/2013
 */
trait TestHelpers extends MockitoSugar with ShouldMatchers {
  import TestHelpers._

  if (!LiftRules.doneBoot) (new bootstrap.liftweb.Boot).configureLift
  if (Defaults.FieldList.get == null) Defaults.FieldList << mockFieldList(Defaults.FieldListDefs)

  def mockFieldList(defs: List[FieldDef[_]]): FieldList = {
    val pv = PlainValue[DataType.FieldList](-666, -1, 1)
    var i = -999
    val keys = defs.map {
      case d: TextFieldDef                        => i -= 1; FieldKey(i, FieldKeyType.Text, Some(d.title))
      case _: NormalAndAlternateCourseFields.type => FieldKey(-1100, FieldKeyType.NormalAndAlternateCourses, None)
      case _: ExceptionCourseFields.type          => FieldKey(-1101, FieldKeyType.ExceptionCourses, None)
    }
    FieldList(pv, keys)
  }

  def eventually(cond: => Any) {
    val test = (sleep: Int) => try { cond; true } catch { case _: Throwable => Thread.sleep(sleep); false }
    if (!test(10))
      if (!test(10))
        if (!test(20))
          if (!test(20))
            if (!test(50))
              if (!test(100))
                if (!test(1000))
                  cond
  }

  def eventuallyIf(wait: Boolean)(cond: => Any) { if (wait) eventually(cond) else cond }

  def any[T](implicit m: Manifest[T]) = org.mockito.Matchers.any(m.runtimeClass.asInstanceOf[Class[T]])

  def matchTree(expected: List[StepNodeWithText]) = TestHelpers.TreeMatcher(expected)

  def mockUseCaseCtx: UseCaseCtx = {
    val u = mock[UseCaseCtx]
    when(u.savedSteps).thenReturn(CachedFunction.static1[FieldSaveCtx, BiMap[Long_StepDataId, String @@ LocalId]](BiMap.empty))
    when(u.stepLabelMap).thenReturn(CachedFunction.lazy0(BiMap.empty[String @@ LocalId, String @@ Label]))
    when(u.msgCentre).thenReturn(mock[MessageCentre])
    when(u.number).thenReturn(1: Short)
    u
  }

  def mockUseCaseCtx(stepLabelMap: Map[String @@ LocalId, String @@ Label]): UseCaseCtx = {
    val u = mockUseCaseCtx
    when(u.stepLabelMap).thenReturn(CachedFunction.lazy0(BiMap(stepLabelMap)))
    u
  }

  def buildStateForTest(nodes: List[StepNodeWithText]): List[StepState] = {
    val cf = new NormalAndAlternateCourseFields(new UseCaseCtx(null), mock[FieldKey])
    cf.buildStateForTest(nodes)
  }

  def createTempDir(prefix: String, suffix: String = ""): File = {
    val tmpDir = File.createTempFile(prefix, suffix)
    tmpDir.delete
    tmpDir.mkdir
    FileUtils.forceDeleteOnExit(tmpDir)
    tmpDir
  }

  def testListOfZeroOrOne[T](expectation: Option[Any], actual: List[T])(testFn: T => Any) {
    if (expectation.isEmpty)
      actual should be('empty)
    else {
      actual should have size (1)
      testFn(actual(0))
    }
  }

  def inMockSession[U](block: => U): U = {
    val session: LiftSession = new LiftSession("", StringHelpers.randomString(20), Empty)
    S.initIfUninitted(session) {block}
  }

  def withSessionAttrs[U](attrs: (String, String)*)(block: => U): U = inMockSession {withSessionAttrs(Map(attrs: _*))(block)}
  def withSessionAttrs[U](attrs: Map[String, String])(block: => U): U = S.withAttrs(S.mapToAttrs(attrs))(block)
}

object TestHelpers extends TestHelpers {

  implicit class MyRichInt(val i: Int) extends AnyVal {
    def times(block: => Any) { 1 to i foreach(_ => block) }
  }

  implicit class CourseFieldExt(val cf: CourseFields) extends AnyVal {

    def coursesWithText: List[StepNodeWithText] = convertNodeTree[StepNode, StepNodeWithText](cf.courses, {
      case (n, lvl, lbl, children) =>
        val savedSteps = try cf.ucCtx.savedSteps.get.ba catch {case _: Throwable => Map.empty[String @@ LocalId, Long_StepDataId]}
        val txt = cf.test__textFields.get(n.id).map(_.textWithNormalisedRefs(savedSteps)).getOrElse("".hasNormalisedRefs)
        StepNodeWithText(n.id, lvl, lbl, txt, children)
    }, cf.startingLabelIndices.startingLabelIndex _)

    def setCoursesWithTextAndInit(nodes: List[StepNodeWithText]) {
      cf.setCourses(nodes.map(_.toStepNode))(NoReactionOrNewMessages)
      cf.init
      val savedSteps = BiMap.empty[Long_StepDataId, String @@ LocalId]
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
        StepNodeWithText(id2.asLocalId, lvl, lblIndex, txt, ch)
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
