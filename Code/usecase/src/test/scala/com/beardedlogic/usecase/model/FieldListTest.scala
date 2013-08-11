package com.beardedlogic.usecase
package model

import org.scalatest.FunSpec
import lib.field._
import test.TestDatabaseSupport
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation

class FieldListTest extends FunSpec with TestDatabaseSupport {

  lazy val fl1 =
    NormalCourseFieldDefinition ::
      TextFieldDefinition("Opeth") ::
      TextFieldDefinition("Heritage") ::
      TextFieldDefinition("Haxprocess") ::
      ExceptionCourseFieldDefinition ::
      Nil

  lazy val fl2 =
    TextFieldDefinition("Opeth") ::
      TextFieldDefinition("Heritage") ::
      TextFieldDefinition("CHANGED") ::
      NormalCourseFieldDefinition ::
      ExceptionCourseFieldDefinition ::
      Nil

  describe("syncFieldList") {
    it("should save when never saved before") {
      sqlu" UPDATE field_key SET data='hehe' WHERE data IS NULL ".execute
      val fl = assertTableDiffs2(Tables.FieldKey -> fl1.size) {db.syncFieldList(fl1)}
      fl.fieldDefns ==== fl1
    }

    it("should use existing when already saved") {
      val save1 = db.syncFieldList(fl1)
      assertTableDiffs2() {
        val save2 = db.syncFieldList(fl1)
        save2 ==== save1
      }
    }

    it("should save differences when it differs") {
      val save1 = db.syncFieldList(fl1)
      val save2 = assertTableDiffs2(Tables.FieldKey -> 1) {db.syncFieldList(fl2)}
      save2.fieldDefns ==== fl2
    }
  }
}