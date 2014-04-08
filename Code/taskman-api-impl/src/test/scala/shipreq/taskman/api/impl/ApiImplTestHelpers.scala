package shipreq.taskman.api.impl

import shipreq.base.test.db.specs2.DatabaseTest
import shipreq.taskman.api.ApiOp

trait ApiImplTestHelpers {
  this: DatabaseTest =>

  lazy val apiOpReifier = new TaskmanApi(TaskmanApi.Context(None), db)

  def run[A](op: ApiOp[A]): A =
    apiOpReifier(op).unsafePerformIO()
  
  def run_(ops: ApiOp[_]*): Unit =
    for (op <- ops) apiOpReifier(op).unsafePerformIO()

}
