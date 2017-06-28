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
    implicit val cfg = Global.config
    implicit val db = TestDb.dbAccess
    val g = Global.default
    Global.modify(_.copy(
      db      = g.db,
      logic   = g.logic,
      taskman = g.taskman))
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
