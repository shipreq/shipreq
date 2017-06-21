package shipreq.webapp.server.logic

import japgolly.univeq._
import java.util.concurrent.ConcurrentHashMap
import scalaz.syntax.monad._
import scalaz.syntax.std.option._
import scalaz.{-\/, Applicative, Monad, \/, \/-}
import shipreq.base.util._

/**
  * Stores data in memory, indexed by key, with atomic operations.
  */
object Store {

  /** All ops are atomic */
  trait Algebra[F[_], K, V >: Null] {
    def storeGet(key: K): F[FreeOption[V]]
    def storeEntryMod(key: K)(f: FreeOption[V] => FreeOption[V]): F[FreeOption[V]]
    def storeEntrySet(key: K)(f: FreeOption[V] => V): F[V]
    def storeValueMod(key: K)(f: V => V): F[FreeOption[V]]
  }

  object Algebra {
    def concurrentHashMap[F[_], K: UnivEq, V >: Null](map: ConcurrentHashMap[K, V] = new ConcurrentHashMap[K, V])
                                                     (implicit F: Applicative[F]): Algebra[F, K, V] =
      new Algebra[F, K, V] {
        override def storeGet(key: K): F[FreeOption[V]] =
          F point FreeOption(map.get(key))

        override def storeEntryMod(key: K)(f: FreeOption[V] => FreeOption[V]): F[FreeOption[V]] =
          F point FreeOption(map.compute(key, (_, v) => f(FreeOption(v)).getOrNull))

        override def storeEntrySet(key: K)(f: FreeOption[V] => V): F[V] =
          F point map.compute(key, (_, v) => f(FreeOption(v)))

        override def storeValueMod(key: K)(f: V => V): F[FreeOption[V]] =
          F point FreeOption(map.computeIfPresent(key, (_, v) => f(v)))
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** Values are shared between registered clients, and removed when all clients have unregistered. */
  object Register {

    type Algebra[F[_], K, V, A] = Store.Algebra[F, K, Node[V, A]]

    final case class RegId[K](key: K, id: Long)
    implicit def univEqRegId[K: UnivEq]: UnivEq[RegId[K]] = UnivEq.derive

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

    // TODO F[FreeOption gets boxed right?

    final class Dsl[F[_], K, V >: Null, A](implicit alg: Algebra[F, K, V, A], F: Monad[F]) {

      def get(key: K): F[FreeOption[V]] =
        alg.storeGet(key).map(_.map(_.value))

      def valueMod(key: K)(f: V => V): F[FreeOption[V]] =
        alg.storeValueMod(key)(_.modValue(f)).map(_.map(_.value))

      def registerAttempt[E](key: K, registrantData: A, init: => F[E \/ V], verify: V => Option[E]): F[E \/ RegId[K]] = {
        def success(node: Node[V, A]): E \/ RegId[K] =
          verify(node.value) <\/ RegId(key, node.maxRegId)

        def tryGet: F[FreeOption[Node[V, A]]] =
          alg.storeValueMod(key)(_.register(registrantData))

        def getOrSet: F[E \/ RegId[K]] =
          init.flatMap {
            case    \/-(v) => alg.storeEntrySet(key)(_.fold(Node.init(v, registrantData), _.register(registrantData))).map(success)
            case e@ -\/(_) => F pure e
          }

        tryGet.flatMap(_.fold(getOrSet, F pure success(_)))
      }

      def unregister(r: RegId[K]): F[Unit] =
        alg.storeEntryMod(r.key)(_.map(_.unregister(r.id)).filter(_.registrants.nonEmpty)).void
    }
  }

}
