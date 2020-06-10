package shipreq.taskman.server

import shipreq.base.test.db._
import shipreq.taskman.server.ServerOpFx.Sql._
import utest._

object ServerOpSqlTest extends TestSuite {

  override def tests = Tests {

//    "getNextNodeIdQ"      - TestDb.checkOutput(getNextNodeIdQ)
    "cfgGetQ"             - TestDb.check(cfgGetQ)
//    "getMsgsAssignNodeZ"  - TestDb.check(getMsgsAssignNodeZ)
//    "getMsgsAssignNodeF"  - TestDb.check(getMsgsAssignNodeF)
//    "getMsgsAssignNodeP"  - TestDb.check(getMsgsAssignNodeP)
    "getMsgAssignWorkerQ" - TestDb.checkOutput(getMsgAssignWorkerQ)
    "reassignWorkerQ"     - TestDb.check(reassignWorkerQ)
    "failAndRetryQ"       - TestDb.check(failAndRetryQ)
    "archiveMsgQ"         - TestDb.checkOutput(archiveMsgQ)

  }
}
