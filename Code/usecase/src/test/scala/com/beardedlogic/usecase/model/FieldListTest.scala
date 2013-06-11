package com.beardedlogic.usecase
package model

import org.scalatest.FunSpec
import lib.field._
import test.TestDatabaseSupport

class FieldListTest extends FunSpec with TestDatabaseSupport {

  lazy val fl1 =
    NormalAndAlternateCourseFields ::
      TextFieldDef("Opeth") ::
      TextFieldDef("Heritage") ::
      TextFieldDef("Haxprocess") ::
      ExceptionCourseFields ::
      Nil

  lazy val fl2 =
      TextFieldDef("Opeth") ::
      TextFieldDef("Heritage") ::
      TextFieldDef("CHANGED") ::
      NormalAndAlternateCourseFields ::
      ExceptionCourseFields ::
      Nil


  describe("FieldList") {
    it("should save") {
      truncate('field_key)
      val fieldList = fl1
      val newValues = fieldList.size + 1
      val saved = assertTableDiffs(
        'data -> newValues,
        'value -> newValues,
        'relation -> fieldList.size,
        'field_key -> fieldList.size) {
        db.createInitialFieldList(fieldList)
      }
      val loaded = db.findFieldList(saved.data, LatestRev)
      loaded.get.fieldDefs should be(fieldList)
    }

    describe("ensureSavedAndLatest") {
      it("should save when never saved before") {
        val id = randomId
        val r = db.syncFieldList(id, fl1)
        r.data should be(Data(id,DataType.FieldList))
        r.fieldDefs should be(fl1)
      }

      it("should use existing when already saved") {
        val save1 = db.createInitialFieldList(fl1)
        assertTableDiffs('data -> 0, 'value -> 0, 'relation -> 0) {
          val save2 = db.syncFieldList(save1.dataId, fl1)
          save2 should be(save1)
        }
      }

      it("should save differences when it differs") {
        val save1 = db.createInitialFieldList(fl1)
        // expect: 1 new field key (data+value)
        // expect: 1 new field list value (value)
        // expect: 5 new relations
        val save2 = assertTableDiffs('data -> 1, 'value -> 2, 'relation -> 5, 'field_key -> 1) {
          db.syncFieldList(save1.dataId, fl2)
        }
        save2.data should be(save1.data)
        save2.fieldDefs should be(fl2)
      }
    }
  }
}