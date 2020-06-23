package shipreq.base.db

import cats.effect.syntax.all._
import cats.free.Free
import cats.implicits._
import doobie._
import doobie.free.{connection => C}
import doobie.implicits._
import japgolly.univeq._
import java.sql.SQLException
import java.time.{Duration, Instant}
import scalaz.{-\/, \/, \/-}

object DoobieHelpers {

  val ConnectionIoUnit: ConnectionIO[Unit] =
    ().pure[ConnectionIO]

  private val now: ConnectionIO[Instant] =
    Free.defer(Free.pure(Instant.now()))

  implicit class ConnectionIOExt[A](private val self: ConnectionIO[A]) extends AnyVal {

    def measureDuration: ConnectionIO[(Duration, A)] = {
      for {
        start <- now
        result <- self
        end <- now
      } yield (Duration.between(start, end), result)
    }

    def collapseError[B](implicit ev: ConnectionIO[A] =:= ConnectionIO[SQLException \/ B]): ConnectionIO[B] =
      ev(self).flatMap {
        case \/-(b) => C.pure(b)
        case -\/(e) => C.raiseError(e)
      }

    @inline def inTransaction(on: Boolean): ConnectionIO[A] =
      withAutoCommit(!on)

    def withAutoCommit(ac: Boolean): ConnectionIO[A] =
      C.getAutoCommit.flatMap { orig =>
        // Yes guarantee(setAutoCommit) is called regardless of the original value cos the inner block may also
        // call setAutoCommit itself.
        (C.setAutoCommit(ac).whenA(ac != orig) *> self).guarantee(C.setAutoCommit(orig))
      }

    /** "Safe" in the sense that an error rolls back the inner transaction without aborting the outer one. */
    def inSafeTransaction: ConnectionIO[SQLException \/ A] = {
      val inner: ConnectionIO[SQLException \/ A] =
        for {
          sp     <- C.setSavepoint
          result <- self.attemptSql
          _      <- C.rollback(sp).whenA(result.isLeft)
        } yield \/.fromEither(result)
      inner.inTransaction(true)
    }

    /** @param level See java.sql.Connection */
    def withTransactionLevel(level: Int): ConnectionIO[A] =
      // This is wrapped in auto-commit=true because if it's not, the following happens:
      // 1. C.getTransactionIsolation is called
      // 2. When auto-commit is false, it starts a new transaction
      // 3. C.setTransactionIsolation is called
      // 4. Postgres throws "Cannot change transaction isolation level in the middle of a transaction"
      C.getTransactionIsolation.flatMap(orig =>
        if (orig ==* level)
          self
        else
          (
            C.setTransactionIsolation(level) *> C.setAutoCommit(false) *> self <* C.commit
          ).guarantee(
            C.setTransactionIsolation(orig)
          )
      ).inTransaction(false)

    def void: ConnectionIO[Unit] =
      self.map(_ => ())

    def attemptVoid: ConnectionIO[Option[Throwable]] =
      self.attempt.map(_.fold[Option[Throwable]](Some(_), _ => None))
  }

  implicit class ConnectionIOExtE[E, A](private val self: ConnectionIO[E \/ A]) extends AnyVal {
    def retry(times: Int): ConnectionIO[E \/ A] =
      self.flatMap {
        case r @ \/-(_) => Free.pure(r)
        case e @ -\/(_) => if (times > 1) retry(times - 1) else Free.pure(e)
      }
  }

  implicit class Update0Ext(private val self: Update0) extends AnyVal {
    def execute: ConnectionIO[Unit] =
      self.run.void
  }

  implicit class UpdateExt[A](private val self: Update[A]) extends AnyVal {
    def executeBatch(as: IterableOnce[A])(implicit c: Write[A]): ConnectionIO[Unit] =
      if (as.iterator.isEmpty) {
        // 0 rows
        ConnectionIoUnit
      } else {
        val it = as.iterator
        val first = it.next()
        if (it.isEmpty) {
          // 1 row
          self.toUpdate0(first).execute
        } else {
          // 2 or more rows
          val addBatch = (a: A) => HPS.set(a) *> FPS.addBatch
          val addBatches = it.map(addBatch).foldLeft(addBatch(first))(_ *> _)
          HC.prepareStatement(self.sql)(addBatches *> FPS.executeBatch).void
        }
      }
  }

  def sequentially[A](cmds: IterableOnce[ConnectionIO[_]], ret: A): ConnectionIO[A] =
    if (cmds.iterator.isEmpty)
      ret.pure[ConnectionIO]
    else
      cmds.iterator
        .asInstanceOf[IterableOnce[ConnectionIO[Any]]]
        .iterator
        .reduce((a, b) => a.flatMap(_ => b))
        .map(_ => ret)

  implicit class DoobieReadObjExt(private val self: Read.type) extends AnyVal {
    def apply2[A, B, Z](f: (A, B) => Z)(implicit r: Read[(A, B)]): Read[Z] =
      r.map(f.tupled)

    def apply3[A, B, C, Z](f: (A, B, C) => Z)(implicit r: Read[(A, B, C)]): Read[Z] =
      r.map(f.tupled)

    def apply4[A, B, C, D, Z](f: (A, B, C, D) => Z)(implicit r: Read[(A, B, C, D)]): Read[Z] =
      r.map(f.tupled)

    def apply5[A, B, C, D, E, Z](f: (A, B, C, D, E) => Z)(implicit r: Read[(A, B, C, D, E)]): Read[Z] =
      r.map(f.tupled)
  }

  implicit class DoobieWriteObjExt(private val self: Write.type) extends AnyVal {
    def apply2[A, B, Z](f: Z => (A, B))(implicit r: Write[(A, B)]): Write[Z] =
      r.contramap(f)

    def apply3[A, B, C, Z](f: Z => (A, B, C))(implicit r: Write[(A, B, C)]): Write[Z] =
      r.contramap(f)

    def apply4[A, B, C, D, Z](f: Z => (A, B, C, D))(implicit r: Write[(A, B, C, D)]): Write[Z] =
      r.contramap(f)

    def apply5[A, B, C, D, E, Z](f: Z => (A, B, C, D, E))(implicit r: Write[(A, B, C, D, E)]): Write[Z] =
      r.contramap(f)
  }

}
