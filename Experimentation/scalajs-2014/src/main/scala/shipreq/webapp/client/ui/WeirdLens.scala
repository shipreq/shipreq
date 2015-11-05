package shipreq.webapp.client.ui

import monocle._
import scalaz.Bind
import scalaz.Scalaz._
import Implicits._

case class WeirdLens[M[_] : Bind : Optional2, S, T, A](get: S => M[A], set: (S, A) => M[T]) {

  def mod(s: S, f: A => A): M[T] =
    get(s).flatMap(a => set(s, f(a)))

  def map[B](l: SimpleLens[A, B]) = WeirdLens[M, S, T, B](
    s => get(s).map(l.get),
    (s,b) => get(s).flatMap(a => set(s, l.set(a, b))))

  def mapF[B](f: A => B)(g: (A, B) => A) = WeirdLens[M, S, T, B](
    s => get(s).map(f),
    (s,b) => get(s).flatMap(a => set(s, g(a, b))))

  def dimap[F, G](f: F => S, g: T=> G) =
    WeirdLens[M, F, G, A](get compose f, (b, a) => set(f(b), a) map g)

  def getO: S => Option[A] =
    s => get(s).toOption

  def setO: (S,A) => Option[T] =
    (s,a) => set(s,a).toOption
}

object WeirdLens {
  def from[S, A](l: SimpleLens[S, A]) = WeirdLens[Id, S, S, A](l.get, l.set)
}