package shipreq.webapp.server.db

import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.server.test.TestDb

object DaoProviderTest extends TestSuite {

  TestDb.init()

  def dp = DB.DaoProvider

  override def tests = TestSuite {
    'withSession - assertEq(dp.withSession(_.session.conn.getAutoCommit), true)
    'withTransaction - assertEq(dp.withTransaction(_.session.conn.getAutoCommit), false)
  }
}
