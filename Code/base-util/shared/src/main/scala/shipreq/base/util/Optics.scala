package shipreq.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.BiMap
import monocle._
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.reflect.ClassTag
import scalaz.{-\/, Applicative, Functor, \/, \/-}

object Optics {

  object Implicits {
    implicit class LensOps[S, T, A, B](private val t: PLens[S, T, A, B]) extends AnyVal {
      def setF[F[_] : Functor](f: F[B])(s: S): F[T] =
        t.modifyF(_ => f)(s)
    }

    implicit class TraversalOps[S, T, A, B](private val t: PTraversal[S, T, A, B]) extends AnyVal {
      def setF[F[_] : Applicative](f: F[B])(s: S): F[T] =
        t.modifyF(_ => f)(s)
    }
  }

  /** Isomorphism between A and B.
    *
    * If the bimap doesn't contain ALL possible `A` and all possible `B`s, then usage may result in an exception
    * (via `Map#apply`).
    */
  def biMapIso_![A, B](m: BiMap[A, B]): Iso[A, B] =
    Iso(m.forward.apply)(m.backward.apply)

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

  def mapValue[K, V](k: K): Lens[Map[K, V], Option[V]] =
    Lens[Map[K, V], Option[V]](_.get(k))(ov => _.setValueOption(k, ov))

  def mapValueEmpty[K, V](k: K, empty: V)(isEmpty: V => Boolean): Lens[Map[K, V], V] =
    Lens[Map[K, V], V](_.getOrElse(k, empty))(v => if (isEmpty(v)) _ - k else _.updated(k, v))

  def innerMap[A, B, C](a: A): Lens[Map[A, Map[B, C]], Map[B, C]] =
    mapValueEmpty[A, Map[B, C]](a, Map.empty)(_.isEmpty)

  def innerMapValue[A, B, C](a: A, b: B): Lens[Map[A, Map[B, C]], Option[C]] =
    innerMap[A, B, C](a) ^|-> mapValue(b)

  def subtypeLens[C, A <: C: ClassTag](default: => A): Lens[C, A] =
    Lens[C, A]({
      case a: A => a
      case _    => default
    })(a => _ => a)

  def coproductLens[C, A](attempt: PartialFunction[C, A],
                          lift   : A => C,
                          default: => A): Lens[C, A] =
    Lens[C, A](attempt.applyOrElse(_, (_: C) => default))(a => _ => lift(a))

  def disjunctionLensLeft[L, R](default: => L): Lens[L \/ R, L] =
    coproductLens[L \/ R, L]({ case -\/(a) => a }, -\/(_), default)

  def disjunctionLensRight[L, R](default: => R): Lens[L \/ R, R] =
    coproductLens[L \/ R, R]({ case \/-(a) => a }, \/-(_), default)
}
