package com.beardedlogic.usecase
package lib.db

import org.scalatest.FunSpec
import test.TestDatabaseSupport
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation
import model.DataType

class DatabaseEnumTest extends FunSpec with TestDatabaseSupport {

  describe("DatabaseEnum.init()") {

    def count = countRowsIn("data_type")

    def verifyAllThere() {
      count should be(DataType.Values.size)
      val found = sql"SELECT id,name FROM data_type".as[(Short,String)].list
      for ((id,name) <- found) DataType(id).name should be(name)
    }

    it("should insert new items") {
      truncate("data_type")
      count should be(0)
      DatabaseEnum.init(DataType)
      verifyAllThere
    }

    it("should update existing") {
      truncate("data_type")
      val v = DataType.UseCase
      sqlu"INSERT INTO data_type(id,name) VALUES(${v.ordinal},'bullshit')".execute
      count should be(1)
      DatabaseEnum.init(DataType)
      verifyAllThere
    }
  }
}