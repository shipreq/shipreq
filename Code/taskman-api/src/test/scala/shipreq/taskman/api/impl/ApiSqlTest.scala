package shipreq.taskman.api.impl

import org.specs2.mutable.Specification
import shipreq.base.test.db.SqlTester.test
import shipreq.base.test.db.TestDb
import shipreq.base.test.specs2.AllowUnitAsResult._
import shipreq.base.test.specs2.db.DatabaseTest

class ApiSqlTest extends Specification with DatabaseTest {
  sequential

  val dao = new ApiDao(TestDb.dbAccess.schemaAsPrefix)
  import dao._

  "CreateMsg" in test(CreateMsg)
//  "CfgPut" in test(CfgPut)
  "QueryMsgStatus" in test(QueryMsgStatus)
}
