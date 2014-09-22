package shipreq.webapp.client.ui

import scalaz.{Applicative, Bind, StateT}
import scalaz.Scalaz.Id
import scalaz.effect.IO
import scalaz.syntax.bind._
import japgolly.scalajs.react.ScalazReact._

object Implicits {

  implicit class StateExt[S, A](val u: StateT[Id, S, A]) extends AnyVal {
    def liftIO: StateT[IO, S, A] = u.lift[IO]
  }

  implicit def autoLiftStateIntoIO[S, A](s: StateT[Id, S, A]) = s.liftIO

  // Use Scalaz's Optional after upgrading to 7.2
  trait Optional2[M[_]] {
    def getOrElse[A](m: M[A], d: => A): A
    def toOption[A](m: M[A]): Option[A]
  }

  implicit object OptionalOption extends Optional2[Option] {
    override def getOrElse[A](m: Option[A], d: => A) = m getOrElse d
    override def toOption[A](m: Option[A]) = m
  }

  implicit object OptionalId extends Optional2[Id] {
    override def getOrElse[A](m: Id[A], d: => A) = m
    override def toOption[A](m: Id[A]) = Some(m)
  }

  implicit final class OptionalOptionOps[M[_], A](val m: M[A]) extends AnyVal {
    def getOrElse(d: => A)(implicit M: Optional2[M]) =
      M.getOrElse(m, d)

    def toOption(implicit M: Optional2[M]) =
      M toOption m

    def mapOrElse[B](d: => B)(f: A => B)(implicit M: Optional2[M], B: Bind[M]): B =
      (m map f) getOrElse d

    def mapReactS[S](f: A => ReactS[S, Unit])(implicit M: Optional2[M], B: Bind[M]): ReactS[S, Unit] =
      m.mapOrElse(ReactS.ret[S, Unit](()))(f)

    def mapReactST[S, N[_]](f: A => ReactST[N, S, Unit])(implicit M: Optional2[M], B: Bind[M], N: Applicative[N]): ReactST[N, S, Unit] =
      m.mapOrElse(ReactS.retT[N, S, Unit](()))(f)
  }
}
