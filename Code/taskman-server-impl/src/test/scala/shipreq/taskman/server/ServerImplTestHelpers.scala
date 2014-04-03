package shipreq.taskman.server

import shipreq.base.test.db.specs2.DatabaseTest
import shipreq.taskman.api.impl.TaskmanApi
import shipreq.taskman.api.ApiOp
import scalaz.effect.IO

trait ServerImplTestHelpers {
  this: DatabaseTest =>

  def apiOpReifier = new TaskmanApi(TaskmanApi.Context(None), db)
  def sopReifier = new SopImpl(db)

  def reify[A](op: ApiOp[A]): IO[A] = apiOpReifier(op)
  def reify[A](op: Sop[A]): IO[A] = sopReifier(op)

  def run[A](op: ApiOp[A]): A = reify(op).unsafePerformIO()
  def run[A](op: Sop[A]): A = reify(op).unsafePerformIO()

}

