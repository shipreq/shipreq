package shipreq.webapp.server.test

import scalaz.effect.IO
import shipreq.base.test.db.SingleConnectionXA
import shipreq.webapp.server.app.Global

object TestDb extends shipreq.base.test.db.TestDb {

  override protected def unsafeInit(): Unit = {
    super.unsafeInit()
    dbAccess.io.trans(DbTable.validate(dbAccess.absoluteSchema)).unsafePerformIO()
    useInLift()
  }

  def useInLift(): Unit = {
    val g1 = PrepareEnv.global()
    val g2 = Global.default(TestDb.dbAccess, g1.config)
    val g3 = Global.modify(_.copy(
      db       = g2.db,
      logic    = g2.logic,
      security = g2.security,
      taskman  = g2.taskman))
    g3
  }

  lazy val truncate: IO[Unit] =
    dbAccess.io trans DbTable.truncateAll

  override protected def unsafeClean(): Unit = {
    super.unsafeClean()
    truncate.unsafePerformIO()
  }

  override def wrapTransaction[A](xa: SingleConnectionXA, io: IO[A]): IO[A] =
    for {
      orig <- IO(Global.db)
      _ <- IO(Global.modify(_.copy(db = xa.dbAccess(dbAccess))))
      a <- io ensuring IO(Global.modify(_.copy(db = orig)))
    } yield a

}
