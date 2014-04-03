package shipreq.taskman.api.impl

import shipreq.base.test.db.specs2.DatabaseTest
import shipreq.taskman.api.ApiOp

trait ApiImplTestHelpers {
  this: DatabaseTest =>

  lazy val apiOpReifier = new TaskmanApi(TaskmanApi.Context(None), db)

  def runApiOp(ops: ApiOp[_]*): Unit =
    for (op <- ops) apiOpReifier(op).unsafePerformIO()

}
