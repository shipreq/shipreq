package shipreq.base.db

import doobie.free.connection.{ConnectionIO => _, _}
import doobie.imports._
import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.univeq._
import java.sql.SQLException
import java.time.{Duration, Instant}
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._

object DoobieHelpers {

  val ConnectionIoUnit: ConnectionIO[Unit] =
    Free.pure(())

  implicit class ConnectionIOExt[A](private val self: ConnectionIO[A]) extends AnyVal {

    def measureDuration: ConnectionIO[(Duration, A)] = {
      val now: ConnectionIO[Instant] = delay(Instant.now())
      for {
        start <- now
        result <- self
        end <- now
      } yield (Duration.between(start, end), result)
    }

    def inTransaction: ConnectionIO[A] =
      getAutoCommit.flatMap(ac =>
        if (ac) runInTransaction else self)

    private def runInTransaction: ConnectionIO[A] =
      for {
        _      <- setAutoCommit(false)
        result <- self
        _      <- commit
        _      <- setAutoCommit(true)
      } yield result

    /** "Safe" in the sense that an error rolls back the inner transaction without aborting the outer one. */
    def inSafeTransaction: ConnectionIO[SQLException \/ A] =
      for {
        ac     <- getAutoCommit
        _      <- setAutoCommit(false).whenM(ac)
        sp     <- setSavepoint
        result <- (if (ac) self <* commit else self).attemptSql
        _      <- rollback(sp).whenM(result.isLeft)
        _      <- setAutoCommit(true).whenM(ac)
      } yield result

    /** @param level See java.sql.Connection */
    def withTransactionLevel(level: Int): ConnectionIO[A] =
      getTransactionIsolation.flatMap(orig =>
        if (orig ==* level)
          self
        else
          (setTransactionIsolation(level) *> self) ensuring setTransactionIsolation(orig))

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
    def executeBatch(as: TraversableOnce[A])(implicit c: Composite[A]): ConnectionIO[Unit] =
      if (as.isEmpty) {
        // 0 rows
        ConnectionIoUnit
      } else {
        val it = as.toIterator
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

  implicit class DoobieMetaExt[A](private val self: Meta[A]) extends AnyVal {
    def readOnlyAnyVal[B](f: A => B)(implicit tt: TypeTag[B]): Meta[B] =
      self.xmap[B](f, _ => sys error s"Writing $tt not supported.")

    def readOnly[B >: Null](f: A => B)(implicit tt: TypeTag[B], ev: Null <:< A): Meta[B] =
      self.nxmap[B](f, _ => sys error s"Writing $tt not supported.")

    def writeOnlyAnyVal[B](f: B => A)(implicit tt: TypeTag[B]): Meta[B] =
      self.xmap[B](_ => sys error s"Reading $tt not supported.", f)

    def writeOnly[B >: Null](f: B => A)(implicit tt: TypeTag[B], ev: Null <:< A): Meta[B] =
      self.nxmap[B](_ => sys error s"Reading $tt not supported.", f)
  }

  implicit class DoobieCompositeExt[A](private val self: Composite[A]) extends AnyVal {
    def readOnly[B](f: A => B)(implicit tt: TypeTag[B]): Composite[B] =
      self.xmap(f, _ => sys error s"Writing $tt not supported.")

    def writeOnly[B](f: B => A)(implicit tt: TypeTag[B]): Composite[B] =
      self.xmap(_ => sys error s"Reading $tt not supported.", f)
  }

  def meta1[A: Meta, B: TypeTag](f: A => B)(g: B => A): Meta[B] =
    Meta[A].xmap(f, g)

  def composite3[Z, A, B, C](f: (A, B, C) => Z)(g: Z => (A, B, C))(implicit c: Composite[(A, B, C)]): Composite[Z] =
    c.xmap(f.tupled, g)

  def selectByNonEmptySet[A, B](as: NonEmptySet[A], groupSize: Int = 100)
                               (f: Seq[A] => ConnectionIO[B]): ConnectionIO[List[B]] = {
    val it = as.iterator.grouped(groupSize).map(f)
    val h = it.next().map(_ :: Nil)
    it.foldLeft(h)(Apply[ConnectionIO].apply2(_, _)((bs, b) => b :: bs))
  }

  def sequentially[A](cmds: TraversableOnce[ConnectionIO[_]], ret: A): ConnectionIO[A] =
    if (cmds.isEmpty)
      ret.pure[ConnectionIO]
    else
      cmds.asInstanceOf[TraversableOnce[ConnectionIO[Any]]].reduce((a, b) => a.flatMap(_ => b)).map(_ => ret)
}
