package com.beardedlogic.usecase
package lib.db

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.fixture.FunSpec
import scala.slick.session.Session
import scala.slick.jdbc.{StaticQuery => Q}
import Q.interpolation
import model.DataType

class DatabaseEnumTest extends FunSpec with ShouldMatchers {

  type FixtureParam = Session

  override protected def withFixture(test: OneArgTest) = {
    DB.Slick.withTransaction { implicit db: Session =>
      try withFixture(test.toNoArgTest(db))
      finally db.rollback()
    }
  }

  describe("DatabaseEnum.init()") {

    def count(implicit db: Session) =  sql"SELECT COUNT(*) FROM data_type".as[Int].first

    def verifyAllThere(implicit db: Session) {
      count should be(DataType.Values.size)
      val found = sql"SELECT id,name FROM data_type".as[(Short,String)].list
      for ((id,name) <- found) DataType(id).name should be(name)
    }

    it("should insert new items") { implicit db =>
      sqlu"DELETE FROM data_type".execute
      count should be(0)
      DatabaseEnum.init(DataType)
      verifyAllThere
    }

    it("should update existing") { implicit db =>
      sqlu"DELETE FROM data_type".execute
      val v = DataType.UseCase
      sqlu"INSERT INTO data_type(id,name) VALUES(${v.ordinal},'bullshit')".execute
      count should be(1)
      DatabaseEnum.init(DataType)
      verifyAllThere
    }
  }
}