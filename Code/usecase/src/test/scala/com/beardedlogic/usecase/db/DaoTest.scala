package com.beardedlogic.usecase.db

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import com.beardedlogic.usecase.test.TestDB
import org.scalatest.Matchers

class DaoTest extends FunSuite with Matchers with BeforeAndAfterAll {

  def Dao = DB.DaoProvider

  override def beforeAll() {
    TestDB.init()
  }

  test("New session should not be in a transaction") {
    Dao.withSession(_.session.conn.getAutoCommit shouldBe true)
  }
  test("New transaction should be in a transaction") {
    Dao.withTransaction(_.session.conn.getAutoCommit shouldBe false)
  }
  test("Session -> transaction") {
    Dao.withSession(d => {
      d.session.conn.getAutoCommit shouldBe true
      d.withTransaction(d.session.conn.getAutoCommit shouldBe false)
      d.session.conn.getAutoCommit shouldBe true
    })
  }
}
