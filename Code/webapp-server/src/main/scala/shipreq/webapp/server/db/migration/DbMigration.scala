package shipreq.webapp.server.db.migration

import cats.Applicative
import cats.effect.{Blocker, IO, Resource}
import doobie._
import doobie.implicits._
import doobie.util.ExecutionContexts
import doobie.util.transactor.Strategy
import org.flywaydb.core.api.migration.{BaseJavaMigration, Context}

private[migration] abstract class DbMigration extends BaseJavaMigration {

  override final def canExecuteInTransaction =
    true

  override final def migrate(context: Context): Unit = {

    val resourceXA: Resource[IO, Transactor[IO]] =
      for {
        csEC  <- ExecutionContexts.cachedThreadPool[IO]
        txnEC <- ExecutionContexts.cachedThreadPool[IO]
      } yield {
        implicit val cs = IO.contextShift(csEC)
        val txnB = Blocker.liftExecutionContext(txnEC)
        val conn = context.getConnection
        Transactor.fromConnection[IO](conn, txnB).copy(strategy0 = Strategy.void)
      }

    resourceXA
      .use(migration.transact(_))
      .unsafeRunSync()
  }

  protected def migration: ConnectionIO[_]

  protected final def point[A](a: => A): ConnectionIO[A] =
    Applicative[ConnectionIO].unit.map(_ => a)

  protected final def execute(sql: String) =
    Update0(sql, None).run
}
