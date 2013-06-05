package com.beardedlogic.usecase
package model

import org.scalatest.FunSpec
import test.{TestHelpers, TestDatabaseSupport}
import lib.Defaults
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation

class UseCaseTest extends FunSpec with TestDatabaseSupport with TestHelpers {

  describe("findUseCaseWithValue") {
    it("should load when found") {
      val value = db.createInitialValue(DataType.UseCase)
      val vid = value.valueId
      sqlu"INSERT INTO usecase VALUES(${vid}, 'ah', 7, ${Defaults.FieldList.get.valueId})".execute

      val uc = db.findUseCaseWithValue(vid).get
      uc should be(UseCaseWithValue(value, "ah", 7.toShort, Defaults.FieldList.get.valueId))
    }
  }
}