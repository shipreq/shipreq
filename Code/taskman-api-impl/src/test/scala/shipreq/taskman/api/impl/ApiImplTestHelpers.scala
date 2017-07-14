package shipreq.taskman.api.impl

import scalaz.effect.IO
import shipreq.base.test.specs2.db.DatabaseTest
import shipreq.taskman.api.TaskmanApi

trait ApiImplTestHelpers {
  this: DatabaseTest =>

  lazy val apiImpl: TaskmanApi[IO] =
    TaskmanApiImpl(TaskmanApiImpl.Context(None), xa.trans)

  def run[A](f: TaskmanApi[IO] => IO[A]): A =
    f(apiImpl).unsafePerformIO()

  def run_(ops: (TaskmanApi[IO] => IO[_])*): Unit =
    ops.foreach(_(apiImpl).unsafePerformIO())

}
