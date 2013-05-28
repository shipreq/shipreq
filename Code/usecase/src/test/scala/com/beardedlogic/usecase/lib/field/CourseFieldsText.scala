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

class CourseFieldsText extends FunSpec with ShouldMatchers with MockitoSugar with TestHelpers {

  describe("load()") {
    val Key_NC = new FieldKey(1, FieldKeyType.NormalAndAlternateCourses, None)
    val Key_EC = new FieldKey(2, FieldKeyType.ExceptionCourses, None)
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

    def mockUCES = {
      val msg = mock[MessageCentre]
      val u = mock[UseCaseCtx]
      when(u.msgCentre).thenReturn(msg)
      when(u.number).thenReturn(1: Short)
      u
    }

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
}