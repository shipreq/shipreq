package doobie.japgolly

import doobie.imports._
import doobie.util.transactor.Transactor
import java.sql.Connection
import scalaz.{Catchable, Free, Monad}

object DoobieHack {

  // Doobie wraps everything in transactions by default
  def disableAutoTransactions[M[_]: Catchable : Capture : Monad](orig: Transactor[M]): Transactor[M] =
    new Transactor[M] {
      override val before: ConnectionIO[Unit] = Free.pure(())
      override val after: ConnectionIO[Unit] = before
      override val oops: ConnectionIO[Unit] = before
      override val connect: M[Connection] = orig.connect
    }

  def connect[M[_]](t: Transactor[M]) = t.connect
}
