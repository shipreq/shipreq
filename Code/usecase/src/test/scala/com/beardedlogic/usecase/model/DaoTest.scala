package com.beardedlogic.usecase.model

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import com.beardedlogic.usecase.test.TestDatabaseSupport
import org.scalatest.matchers.ShouldMatchers

class DaoTest extends FunSuite with ShouldMatchers with BeforeAndAfterAll {

  override def beforeAll() {
    TestDatabaseSupport.init()
  }

  test("New session should not be in a transaction") {
    DAO.withSession(_.conn.getAutoCommit should be(true))
  }
  test("New transaction should be in a transaction") {
    DAO.withTransaction(_.conn.getAutoCommit should be(false))
  }
  test("Session -> transaction") {
    DAO.withSession(d => {
      d.conn.getAutoCommit should be(true)
      d.withTransaction(d.conn.getAutoCommit should be(false))
      d.conn.getAutoCommit should be(true)
    })
  }
}
