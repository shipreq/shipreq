package shipreq.webapp.server.test

import scalaz.effect.IO
import shipreq.base.test.db.SingleConnectionXA
import shipreq.webapp.server.app.DI

object TestDb extends shipreq.base.test.db.TestDb {

  override protected def unsafeInit(): Unit = {
    super.unsafeInit()
    dbAccess.io.trans(DbTable.validate(dbAccess.absoluteSchema)).unsafePerformIO()
    useInLift()
  }

  def useInLift(): Unit =
    DI.dbAccess = TestDb.dbAccess

  lazy val truncate: IO[Unit] =
    dbAccess.io trans DbTable.truncateAll

  override protected def unsafeClean(): Unit = {
    super.unsafeClean()
    truncate.unsafePerformIO()
  }

  override def wrapTransaction[A](xa: SingleConnectionXA, io: IO[A]): IO[A] =
    for {
      orig <- IO(DI.dbAccess)
      _ <- IO(DI.dbAccess = xa.dbAccess(dbAccess))
      a <- io ensuring IO(DI.dbAccess = orig)
    } yield a

}
