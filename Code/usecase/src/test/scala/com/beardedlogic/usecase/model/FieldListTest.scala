package com.beardedlogic.usecase
package model

import org.scalatest.FunSpec
import lib.field._
import test.TestDatabaseSupport

class FieldListTest extends FunSpec with TestDatabaseSupport {

  describe("FieldList") {
    it("should save") {

      val fieldList =
        NormalAndAlternateCourseFields ::
          TextFieldDef("Opeth") ::
          TextFieldDef("Heritage") ::
          TextFieldDef("Haxprocess") ::
          ExceptionCourseFields ::
          Nil

      val newValues = fieldList.size + 1
      val saved = assertTableDiffs("data" -> newValues, "value" -> newValues, "relation" -> fieldList.size) {
        FieldList.save(fieldList)
      }

      FieldList.load(saved.id) should be(fieldList)
    }
  }
}