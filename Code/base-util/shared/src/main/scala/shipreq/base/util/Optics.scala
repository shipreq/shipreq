package shipreq.base.util

import monocle._
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scalaz.Applicative

object Optics {

  def cbfTraversal[M[x] <: Traversable[x], A, N[_], B](implicit cbf: CanBuildFrom[M[A], B, N[B]]): PTraversal[M[A], N[B], A, B] =
    new PTraversal[M[A], N[B], A, B] {
      override def modifyF[F[_]](f: A => F[B])(ma: M[A])(implicit F: Applicative[F]): F[N[B]] = {
        type C = Builder[B, N[B]]
        val add: F[B => C => C] = F.pure(b => _ += b)
        var fc: F[C] = F.point(cbf(ma))
        for (a <- ma)
          fc = F.ap(fc)(F.ap(f(a))(add))
        F.map(fc)(_.result())
      }
    }

  def cbfIterable[From, A](implicit cbf: CanBuildFrom[Nothing, A, List[A]]): CanBuildFrom[From, A, Iterable[A]] =
    collection.breakOut(cbf)

  def listPTraversal[A, B]: PTraversal[List[A], List[B], A, B] =
    cbfTraversal[List, A, List, B]

  def listTraversal[A]: Traversal[List[A], A] =
    listPTraversal[A, A]

  def vectorPTraversal[A, B]: PTraversal[Vector[A], Vector[B], A, B] =
    cbfTraversal[Vector, A, Vector, B]

  def vectorTraversal[A]: Traversal[Vector[A], A] =
    vectorPTraversal[A, A]

  def setPTraversal[A, B]: PTraversal[Set[A], Set[B], A, B] =
    cbfTraversal[Set, A, Set, B]

  def setTraversal[A]: Traversal[Set[A], A] =
    setPTraversal[A, A]

  def iterablePTraversal[A, B]: PTraversal[Iterable[A], Iterable[B], A, B] =
    cbfTraversal[Iterable, A, Iterable, B](cbfIterable[Iterable[A], B])

  def iterableTraversal[A]: Traversal[Iterable[A], A] =
    iterablePTraversal[A, A]

  def nonEmptyMapIso[K, V]: Iso[Option[Map[K, V]], Map[K, V]] =
    Iso[Option[Map[K, V]], Map[K, V]](_ getOrElse Map.empty)(m => if (m.isEmpty) None else Some(m))

  def nonEmptyMapValueLens[K, V, O, I](k: K, iso: PIso[Option[V], Option[V], O, I]): PLens[Map[K, V], Map[K, V], O, I] =
    PLens[Map[K, V], Map[K, V], O, I](m => iso.get(m get k))(i => m =>
      iso.reverseGet(i) match {
        case Some(v) => m.updated(k, v)
        case None    => m - k
      })
}
