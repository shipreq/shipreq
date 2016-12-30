package shipreq.taskman.server

import doobie.imports._
import java.util.Properties
import java.util.concurrent.locks.ReentrantReadWriteLock
import scalaz.effect.IO
import shipreq.base.util.{JPropertiesValueReader, Props, RunMode}
import shipreq.taskman.api.ApiOp
import ServerImplTestHelpers._

trait ServerImplTestHelpers {
  def xa: Transactor[IO]

  final def dbMutexR = ServerImplTestHelpers.dbMutexR
  final def dbMutexW = ServerImplTestHelpers.dbMutexW

  lazy val ctx: TaskmanCtx = new TaskmanCtx(xa, props, propsR) {
    override def cfgFromApiReader = propsR
  }
  import ctx._

  def reify[A](op: ApiOp[A]): IO[A] = aopReifier(op)
  def reify[A](op: Sop[A]): IO[A] = sopReifier(op)

  def run[A](op: ApiOp[A]): A = reify(op).unsafePerformIO()
  def run[A](op: Sop[A]): A = reify(op).unsafePerformIO()
}

object ServerImplTestHelpers {

  def props = Props.loadUsingStandardStrategy(RunMode.Test)(new Properties)
  val propsR = JPropertiesValueReader(props)

  val dbLockRW = new ReentrantReadWriteLock
  val dbMutexR = Some(dbLockRW.readLock)
  val dbMutexW = Some(dbLockRW.writeLock)

  def apply(_xa: Transactor[IO]): ServerImplTestHelpers =
    new ServerImplTestHelpers {
      override def xa = _xa
    }
}