package shipreq.taskman.api.impl

import shipreq.base.util.FxModule._
import shipreq.base.test.specs2.db.DatabaseTest
import shipreq.taskman.api.TaskmanApi

trait ApiImplTestHelpers {
  this: DatabaseTest =>

  lazy val apiImpl: TaskmanApi[Fx] =
    TaskmanApiImpl(TaskmanApiImpl.Context(None), xa.trans)

  def run[A](f: TaskmanApi[Fx] => Fx[A]): A =
    f(apiImpl).unsafeRun()

  def run_(ops: (TaskmanApi[Fx] => Fx[_])*): Unit =
    ops.foreach(_(apiImpl).unsafeRun())

}
