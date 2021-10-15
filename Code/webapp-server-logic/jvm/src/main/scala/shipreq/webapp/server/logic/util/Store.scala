package shipreq.webapp.server.logic.util

import cats.syntax.all._
import cats.{Applicative, Monad}
import java.util.concurrent.ConcurrentHashMap
import monocle.macros.Lenses
import shipreq.base.util.CatsExtra.ApplicativeDelay
import shipreq.base.util.FreeOption.Implicits._
import shipreq.base.util._

/**
  * Stores data in memory, indexed by key, with atomic operations.
  */
object Store {

  /** All ops are atomic */
  trait Algebra[F[_], K, V >: Null] {

    /** Number of keys currently in the store */
    val storeKeyCount: F[Int]

    def storeGet(key: K): F[Option[V]]
    def storeMod(key: K)(f: FreeOption[V] => FreeOption[V]): F[Option[V]]
    def storeModSet(key: K)(f: FreeOption[V] => V): F[V]

    def storeModIfPresent(key: K)(f: V => V): F[Option[V]] =
      storeMod(key)(_.map(f))

    final def storeModO(key: K)(f: Option[V] => Option[V]): F[Option[V]] =
      storeMod(key)(o => f(o.toOption).free)

    final def storeModT[OptionV <: Option[V]](key: K)(f: FreeOption[V] => OptionV): F[OptionV] = {
      val x: F[Option[V]] = storeMod(key)(f(_).free)
      x.asInstanceOf[F[OptionV]]
    }

    final def storeUpdateQuickOrLong[E, A, OK <: Option[V]](key        : K,
                                                            tryQuick   : FreeOption[V] => FreeOption[V],
                                                            quickWorked: Option[V] => F[E \/ A] \/ OK)
                                                           (applyLong  : (Option[V], A) => OK)
                                                           (implicit F : Monad[F]): F[E \/ OK] =
      storeMod(key)(tryQuick)
        .flatMap(quickWorked(_) match {
          case ok @ \/-(_) => F pure ok
          case -\/(fea) => fea flatMap {
            case \/-(a) =>
              storeModT(key)(fo => {
                val o = fo.toOption
                quickWorked(o).getOrElse(applyLong(o, a))
              }).map(\/-(_))
            case e@ -\/(_) =>
              F pure e
          }
        })

    final def storeUpdateQuickOrSetLong[E, A](key        : K,
                                              tryQuick   : FreeOption[V] => FreeOption[V],
                                              quickWorked: Option[V] => F[E \/ A] \/ V)
                                             (applyLong  : (Option[V], A) => V)
                                             (implicit F : Monad[F]): F[E \/ V] =
      storeUpdateQuickOrLong[E, A, Some[V]](
        key,
        tryQuick,
        quickWorked(_).map(Some(_)))(
        (o, a) => Some(applyLong(o, a))).map(_.map(_.value))

    final def storeModOrTryInit[E](key: K, mod: V => V, tryInit: => F[E \/ V])(implicit F: Monad[F]): F[E \/ V] = {
      lazy val tryInit2 = tryInit
      storeUpdateQuickOrSetLong[E, V](key, _.map(mod), _ toRight tryInit2)(_ getOrElse _)
    }
  }

  object Algebra {
    def concurrentHashMap[F[_], K: UnivEq, V >: Null](map: ConcurrentHashMap[K, V] = new ConcurrentHashMap[K, V])
                                                     (implicit F: Applicative[F]): Algebra[F, K, V] =
      new Algebra[F, K, V] {
        override val storeKeyCount: F[Int] =
          F delay map.size()

        override def storeGet(key: K): F[Option[V]] =
          F delay Option(map.get(key))

        override def storeMod(key: K)(f: FreeOption[V] => FreeOption[V]): F[Option[V]] =
          F delay Option(map.compute(key, (_, v) => f(FreeOption(v)).getOrNull))

        override def storeModSet(key: K)(f: FreeOption[V] => V): F[V] =
          F delay map.compute(key, (_, v) => f(FreeOption(v)))

        override def storeModIfPresent(key: K)(f: V => V): F[Option[V]] =
          F delay Option(map.computeIfPresent(key, (_, v) => f(v)))
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** Values are shared between registered clients, and removed when all clients have unregistered. */
  object Register {

    type Algebra[F[_], K, V, A] = Store.Algebra[F, K, Node[V, A]]

    final case class RegId[K](key: K, id: Long)
    implicit def univEqRegId[K: UnivEq]: UnivEq[RegId[K]] = UnivEq.derive

    @Lenses
    final case class Node[V, A](value: V, registrants: List[(Long, A)], maxRegId: Long) {
      def modValue(f: V => V): Node[V, A] =
        copy(value = f(value))

      /** Use maxRegId to get registrationId */
      def register(registrantData: A): Node[V, A] = {
        val id = maxRegId + 1
        Node(value, (id, registrantData) :: registrants, id)
      }

      def unregister(id: Long): Node[V, A] = {
        val newRegistrants = registrants.filter(_._1 !=* id)
        if (newRegistrants eq registrants)
          this
        else {
          val newMaxId: Long =
            if (id < maxRegId)
              maxRegId
            else {
              // A little boilerplate for a little server capacity increase
              var m = Long.MinValue
              var i = newRegistrants
              while (i.nonEmpty) {
                val h = i.head._1
                if (h > m) m = h
                i = i.tail
              }
              m
            }
          Node(value, newRegistrants, newMaxId)
        }
      }
    }

    object Node {
      def init[V, A](value: V, registrantData: A): Node[V, A] = {
        val id = Long.MinValue
        apply(value, (id, registrantData) :: Nil, id)
      }
    }

    final class Dsl[F[_], K, V >: Null, A](implicit alg: Algebra[F, K, V, A], F: Monad[F]) {

      def get(key: K): F[Option[V]] =
        alg.storeGet(key).map(_.map(_.value))

      def valueMod(key: K)(f: V => V): F[Option[V]] =
        alg.storeModIfPresent(key)(_.modValue(f)).map(_.map(_.value))

      def registerAttempt[E](key: K, registrantData: A, init: => F[E \/ V], verify: V => Option[E]): F[E \/ RegId[K]] =
        alg.storeModOrTryInit(key, _.register(registrantData), init.map(_.map(v => Node.init(v, registrantData))))
          .map(_.flatMap(n => verify(n.value) toLeft RegId(key, n.maxRegId)))

      def unregister(r: RegId[K]): F[Unit] =
        alg.storeMod(r.key)(_.map(_.unregister(r.id)).filter(_.registrants.nonEmpty)).void
    }
  }

}
