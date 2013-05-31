package com.beardedlogic.usecase
package lib
package field

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import StepTree._
import model._
import msg.MessageCentre
import test.TestHelpers
import CourseFields._
import TypeTags._
import NodeUtils._

class CourseFieldsTest extends FunSpec with TestHelpers {

  implicit def autoTagLocalStepIds(s: String) = s.asLocalStepId
  implicit def autoTagNormalisedRefs(s: String) = s.hasNormalisedRefs

  val Key_NC = new FieldKey(1, FieldKeyType.NormalAndAlternateCourses, None)
  val Key_EC = new FieldKey(2, FieldKeyType.ExceptionCourses, None)

  val T1 = StepState("X2", "T1", List(StepState("X3", "T2", Nil), StepState("X4", "T3", Nil)))
  val T4 = StepState("X5", "T4", List(StepState("X6", "T5", Nil), StepState("X7", "T6", Nil)))
  val Tree1 = StepState("X1", "Root", List(T1, T4)) :: StepState("X8", "Other", Nil) :: Nil
  val Tree2 = StepState("X1", "Root [D.800]", List(T1, T4)) :: StepState("X8", "Other", Nil) :: Nil

  describe("CourseFieldState") {
    it("should build a step map") {
      val x = CourseFieldState(Tree1)
      x.stepMap.size should be(8)
      x.stepMap("X2") should be(T1)
      x.stepMap("X4") should be(StepState("X4", "T3", Nil))
      x.stepMap("X1") should be(Tree1.head)
    }
  }

  describe("Building nodes from state") {
    it("should build a matching tree (NC/AC)") {
      val cf = new NormalAndAlternateCourseFields(mockUseCaseCtx, Key_NC)
      cf.setState(CourseFieldState(Tree1))()
      cf.courses should matchTree(parseStepTree( """
        1.0. Root
          1. T1
            a. T2
            b. T3
          2. T4
            a. T5
            b. T6
        1.1. Other """))
      cf.stepLabelMap("X1") should be("1.0")
      cf.stepLabelMap("X2") should be("1.0.1")
      cf.stepLabelMap("X7") should be("1.0.2.b")
    }

    it("should build a matching tree (EC)") {
      val cf = new ExceptionCourseFields(mockUseCaseCtx, Key_EC)
      cf.setState(CourseFieldState(Tree1))()
      cf.courses should matchTree(parseStepTree( """
        1.E.1. Root
          1. T1
            a. T2
            b. T3
          2. T4
            a. T5
            b. T6
        1.E.2. Other """))
      cf.stepLabelMap("X1") should be("1.E.1")
      cf.stepLabelMap("X2") should be("1.E.1.1")
      cf.stepLabelMap("X7") should be("1.E.1.2.b")
    }

    it("should sync text fields (without refs)") {
      val cf = new NormalAndAlternateCourseFields(mockUseCaseCtx, Key_NC)
      cf.setState(CourseFieldState(Tree1))()
      val tf = cf.test__textFields
      tf.keySet should be(Set("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8"))
      tf("X2").text should be("T1")
    }

    it("should sync text fields and realise normalised refs") {
      val ucCtx = mockUseCaseCtx
      val cf = new NormalAndAlternateCourseFields(ucCtx, Key_NC)
      val fn = cf.setState(CourseFieldState(Tree2))
      when(ucCtx.savedSteps).thenReturn(Map(800.tag[StepDataId] -> "X8".asLocalStepId))
      when(ucCtx.stepLabelMap).thenReturn(cf.stepLabelMap)
      fn()
      val tf = cf.test__textFields
      tf.keySet should be(Set("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8"))
      tf("X1").text should be("Root [1.1]")
    }
  }

  /*
  describe("Loading") {
    val Value_NC = new FieldValue(10, Key_NC.valueId, None)
    val Value_EC = new FieldValue(20, Key_EC.valueId, None)
    val FieldValueMap = Map(1L -> Value_NC, 2L -> Value_EC)
    val StepValueMap = Map(100L -> "Root NC", 201L -> "EC 1E1", 202L -> "EC 1E2", 211L -> "EC 1E11")
    val Relations = Map((RelationType.Has: RelationType) -> Map(
      10L -> List(100L)
      , 20L -> List(201L, 202L)
      , 201L -> List(211L)
    ))
    val LoadCtx = new FieldLoadCtx(FieldValueMap, Relations, StepValueMap)

    def NodeId(id: Long) = "v" + id.toString

    it("should clear courses when no field value exists") {
      val ec = new ExceptionCourseFields(mockUCES, Key_EC)
      ec.courses = StepNode("id", 0, 0, NewStep, Nil) :: Nil
      ec.load(new FieldLoadCtx(Map(1L -> Value_NC), Relations, StepValueMap))
      ec.courses should be('empty)
    }

    it("should change its value to the loaded field value") {
      val ec = new ExceptionCourseFields(mockUCES, Key_EC)
      ec.load(LoadCtx)
      val expected =
        StepNode(NodeId(201), 0, 1, Step("EC 1E1"), List(new StepNode(NodeId(211), 1, 1, Step("EC 1E11")))) ::
          StepNode(NodeId(202), 0, 2, Step("EC 1E2"), Nil) ::
          Nil
      ec.courses should matchTree(expected)
      ec.courses should be(expected)
    }
  }
  */
}