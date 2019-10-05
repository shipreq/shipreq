package shipreq.taskman.api.impl

import shipreq.base.test.db.SqlTester.test
import shipreq.base.test.db.TestDb

import utest._

object ApiSqlTest extends TestSuite {
  private val dao = new ApiDao(TestDb.dbAccess.schemaAsPrefix)
  import dao._

  override def tests = Tests {

    "CreateMsg" - test(createMsgQuery)
    //  "CfgPut" - test(CfgPut)
    "QueryMsgStatus" - test(queryMsgStatusQuery)

  }
}
