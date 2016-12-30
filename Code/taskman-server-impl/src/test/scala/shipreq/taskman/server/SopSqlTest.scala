package shipreq.taskman.server

import org.specs2.mutable.Specification
import shipreq.base.test.db.SqlTester.test
import shipreq.base.test.specs2.AllowUnitAsResult._
import shipreq.base.test.specs2.db.DatabaseTest
import shipreq.taskman.server.SopImpl.Sql._

class SopSqlTest extends Specification with DatabaseTest {
  sequential

//  "getNextNodeIdQ" in test(getNextNodeIdQ)
  "cfgGetQ" in test(cfgGetQ)
//  "getMsgsAssignNodeZ" in test(getMsgsAssignNodeZ)
//  "getMsgsAssignNodeF" in test(getMsgsAssignNodeF)
//  "getMsgsAssignNodeP" in test(getMsgsAssignNodeP)
//  "getMsgAssignWorkerQ" in test(getMsgAssignWorkerQ)
  "reassignWorkerQ" in test(reassignWorkerQ)
  "failAndRetryQ" in test(failAndRetryQ)
//  "archiveMsgQ" in test(archiveMsgQ)
}
