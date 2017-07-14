package shipreq.taskman.server

import java.util.concurrent.locks.ReentrantReadWriteLock
import scalaz.effect.IO
import shipreq.base.test.db.{SingleConnectionXA, TestDb}
import shipreq.base.util.Props
import shipreq.taskman.api.TaskmanApi
import shipreq.taskman.server.ServerImplTestHelpers._

trait ServerImplTestHelpers {
  def xa: SingleConnectionXA

  final def dbMutexR = ServerImplTestHelpers.dbMutexR
  final def dbMutexW = ServerImplTestHelpers.dbMutexW

  lazy val ctx = TaskmanCtx(
    xa dbAccess TestDb.dbAccess,
    taskmanConfig,
    cfgSrc)

  lazy val taskmanApi = ctx.taskmanApi
  import ctx._

  def reify[A](op: Sop[A]): IO[A] = sopReifier(op)

  def runApi[A](f: TaskmanApi[IO] => IO[A]): A = f(taskmanApi).unsafePerformIO()
  def run[A](op: Sop[A]): A = reify(op).unsafePerformIO()
}

object ServerImplTestHelpers {

  val dbLockRW = new ReentrantReadWriteLock
  val dbMutexR = Some(dbLockRW.readLock)
  val dbMutexW = Some(dbLockRW.writeLock)

  private[server] def cfgSrc = Props.sources

  lazy val (taskmanConfig, taskmanConfigReport) =
    TaskmanConfig.config.withReport.run(cfgSrc).unsafePerformIO().getOrDie()

  def apply(_xa: SingleConnectionXA): ServerImplTestHelpers =
    new ServerImplTestHelpers {
      override def xa = _xa
    }
}