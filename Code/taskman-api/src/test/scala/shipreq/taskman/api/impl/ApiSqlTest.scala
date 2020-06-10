package shipreq.taskman.api.impl

import shipreq.base.test.db.TestDb
import utest._

object ApiSqlTest extends TestSuite {

  private lazy val dao = new ApiDao(TestDb.db.schemaAsPrefix)
  import dao._

  override def tests = Tests {

    "CreateMsg" - TestDb.check(createMsgQuery)
    // "CfgPut" - TestDb.check(cfgPutQuery)
    "QueryMsgStatus" - TestDb.check(queryMsgStatusQuery)

  }
}
