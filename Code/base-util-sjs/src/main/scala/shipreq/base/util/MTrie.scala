package shipreq.base.util

import scala.annotation.tailrec
import scalaz.{Order, Equal}
import scalaz.std.map.mapEqual

/**
 * A Trie where each level is a Map of keys to nodes.
 */
object MTrie {
  type Trie[K, V] = Map[K, Node[K, V]]

  sealed abstract class Node[K, V] {
    def fold[A](b: Branch[K, V] => A, t: Value[K, V] => A): A
  }

  final case class Branch[K, V](value: Option[Value[K, V]], next: Trie[K, V]) extends Node[K, V] {
    override def fold[A](b: Branch[K, V] => A, t: Value[K, V] => A) = b(this)
  }

  final case class Value[K, V](value: V) extends Node[K, V] {
    override def fold[A](b: Branch[K, V] => A, t: Value[K, V] => A) = t(this)
  }

  // ===================================================================================================================

  def empty[K: UnivEq, V]: Trie[K, V] = UnivEq.emptyMap

  implicit def equality[K: Order, V: Equal]: Equal[Trie[K, V]] =
    Equal[Map[NonEmptyVector[K], V]] contramap (_.flattenTrie)

  // ===================================================================================================================

  implicit class Ops[K, V](val trie: MTrie.Trie[K, V]) extends AnyVal {
    type Trie   = MTrie.Trie[K, V]
    type Node   = MTrie.Node[K, V]
    type Branch = MTrie.Branch[K, V]
    type Value  = MTrie.Value[K, V]
    type Path   = NonEmptyVector[K]
    @inline implicit private[this] def keyUnivEq: UnivEq[K] = UnivEq.force // evident from existence of trie

    /**
     * Flat left fold. Sugar to expand the key-value tuple.
     */
    @inline def foldl[A](z: A)(f: (A, K, Node) => A): A =
      trie.foldLeft(z)((a, kv) => f(a, kv._1, kv._2))

    def cataN[A](z: A)(f: (A, Node) => A): A =
      foldl(z)((q, k, n) =>
        n.fold(
          b => b.next.cataN(f(q, n))(f),
          _ => f(q, n)))

    /**
     * A fold that builds up a "path" representing a node's location from the root.
     *
     * @param pz Empty/root path.
     * @param p1 Append a trie key to the path.
     * @tparam P0 A possibly-empty path.
     * @tparam P1 A non-empty path.
     */
    def cataP[A, P0, P1](z: A, pz: => P0, p0: P1 => P0)
                        (p1: (P0, K) => P1, f: (A, P1, Option[Value]) => A): A = {

      def traverseT(a: A, path: P0, t: Trie): A =
        t.foldl(a)((q, k, n) => traverseN(q, p1(path, k), n))

      @inline def traverseN(a: A, path: P1, node: Node): A =
        node.fold(
          b => traverseT(f(a, path, b.value), p0(path), b.next),
          t => f(a, path, Some(t)))

      traverseT(z, pz, trie)
    }

    def cata[A](z: A)(f: (A, Path, Option[Value]) => A): A =
      cataP[A, Vector[K], Path](z, Vector.empty, _.whole)(NonEmptyVector.end, f)

    def cataV[A](z: A)(f: (A, Path, V) => A): A =
      cata(z)((a, p, o) => o.fold(a)(v => f(a, p, v.value)))

    def flattenTrie: Map[Path, V] =
      flattenTrieP(identity)

    def flattenTrieP[P: UnivEq](f: Path => P): Map[P, V] =
      cata(UnivEq.emptyMap[P, V])((m, p, ot) =>
        ot.fold(m)(t => m.updated(f(p), t.value)))

    def flatStream: Stream[(Path, V)] = {
      def go(trie: Trie, p: Vector[K]): Stream[(Path, V)] =
        trie.toStream.flatMap { kv =>
          val k = kv._1
          @inline def result(t: Value) = (NonEmptyVector.end(p, k), t.value)
          kv._2.fold(
            b => b.value.map(result).toStream append go(b.next, p :+ k),
            v => Stream(result(v)))
        }
      go(trie, Vector.empty)
    }

    def put(loc: Path, value: V): Trie = {
      val v = Value[K, V](value)
      @inline def empty = MTrie.empty[K, V]

      @tailrec def go(t: Trie, locH: K, locT: Vector[K], unwind: Trie => Trie): Trie =
        if (locT.isEmpty) {

          // At target-path's end
          val newNode: Node =
            t.get(locH) match {
              case Some(Branch(_, next)) => Branch(Some(v), next)
              case Some(_: Value)
                 | None                  => v
            }
          unwind(t.updated(locH, newNode))

        } else {

          // Still traversing target-path
          val a = locT.head
          val b = locT.tail
          t.get(locH) match {
            case Some(Branch(ot, onext)) => go(onext, a, b, n ⇒ unwind(t.updated(locH, Branch(ot, n))))
            case ot @ Some(_: Value)     => go(empty, a, b, n ⇒ unwind(t.updated(locH, Branch(ot.asInstanceOf[Option[Value]], n))))
            case None                    => go(empty, a, b, n ⇒ unwind(t.updated(locH, Branch(None, n))))
          }
        }

      go(trie, loc.head, loc.tail, identity)
    }

    def pathSet: Set[Path] =
      pathSetP(identity)

    def pathSetP[P: UnivEq](f: Path => P): Set[P] =
      cataV(UnivEq.emptySet[P])((q, path, _) => q + f(path))
  }
}
