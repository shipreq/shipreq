package shipreq.webapp.server.test

import shipreq.base.util.FxModule._
import shipreq.base.test.db.SingleConnectionXA
import shipreq.webapp.server.app.Global
import shipreq.webapp.ssr.SsrOff

object TestDb extends shipreq.base.test.db.TestDb {

  override protected def unsafeInit(): Unit = {
    super.unsafeInit()
    dbAccess.fx.trans(DbTable.validate(dbAccess.absoluteSchema)).unsafeRun()
    useInLift()
  }

  def useInLift(): Unit = {
    val g1 = PrepareEnv.global()
    val g2 = Global.default(TestDb.dbAccess, None, SsrOff.prepared, g1.config)
    val g3 = Global.modify(_.copy(
      db       = g2.db,
      logic    = g2.logic,
      ops      = g2.ops,
      ssr      = g2.ssr,
      security = g2.security,
      taskman  = g2.taskman))
    g3
  }

  lazy val truncate: Fx[Unit] =
    dbAccess.fx trans DbTable.truncateAll

  override protected def unsafeClean(): Unit = {
    super.unsafeClean()
    truncate.unsafeRun()
  }

  override def wrapTransaction[A](xa: SingleConnectionXA, fx: Fx[A]): Fx[A] =
    for {
      orig <- Fx(Global.db)
      _    <- Fx(Global.modify(_.copy(db = xa.dbAccess(dbAccess))))
      a    <- fx andFinally Fx(Global.modify(_.copy(db = orig)))
    } yield a

}
