package shipreq.base

import doobie.ConnectionIO
import scalaz.Monad

package object db {

  implicit object scalazDoobieConnectionIO extends Monad[ConnectionIO] {
    override def point[A](a: => A): ConnectionIO[A] =
      cats.free.Free.defer(cats.free.Free.pure(a))

    override def bind[A, B](fa: ConnectionIO[A])(f: A => ConnectionIO[B]): ConnectionIO[B] =
      fa.flatMap(f)

    override def map[A, B](fa: ConnectionIO[A])(f: A => B): ConnectionIO[B] =
      fa.map(f)
  }

}
