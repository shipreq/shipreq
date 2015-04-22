package shipreq.base.util

import japgolly.nyaya.util.Multimap
import scala.annotation.tailrec
import scalaz.{Order, Equal}
import scalaz.std.map.mapEqual

/**
 * A Trie where each level is a Map of keys to nodes.
 */
object MTrie {
  type Trie[K, V] = Map[K, Node[K, V]]

  sealed abstract class Node[K, V] {
    def fold[A](b: Branch[K, V] => A, t: Target[K, V] => A): A
  }

  final case class Branch[K, V](target: Option[Target[K, V]], next: Trie[K, V]) extends Node[K, V] {
    override def fold[A](b: Branch[K, V] => A, t: Target[K, V] => A) = b(this)
  }

  final case class Target[K, V](value: V) extends Node[K, V] {
    override def fold[A](b: Branch[K, V] => A, t: Target[K, V] => A) = t(this)
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
    type Target = MTrie.Target[K, V]
    type Path   = NonEmptyVector[K]
    @inline implicit private[this] def keyUnivEq: UnivEq[K] = UnivEq.force // evident from existence of trie

    @inline def foldl[A](z: A)(f: (A, K, Node) => A): A =
      trie.foldLeft(z)((a, kv) => f(a, kv._1, kv._2))

    def foldValues[A](z: A)(f: (A, Node) => A): A =
      foldl(z)((q, k, v) =>
        v.fold(
          b => b.next.foldValues(f(q, v))(f),
          _ => f(q, v)))

    /**
     * A fold that builds up a "path" representing a node's location from the root.
     *
     * @param pz Empty/root path.
     * @param p1 Append a trie key to the path.
     * @tparam P0 A possibly-empty path.
     * @tparam P1 A non-empty path.
     */
    def foldP[A, P0, P1](z: A, pz: => P0, p0: P1 => P0)
                        (p1: (P0, K) => P1, f: (A, P1, Option[Target]) => A): A = {

      def traverseT(a: A, path: P0, t: Trie): A =
        t.foldl(a)((q, k, v) => traverseN(q, p1(path, k), v))

      @inline def traverseN(a: A, path: P1, node: Node): A =
        node.fold(
          b => traverseT(f(a, path, b.target), p0(path), b.next),
          t => f(a, path, Some(t)))

      traverseT(z, pz, trie)
    }

    def fold[A](z: A)(f: (A, Path, Option[Target]) => A): A =
      foldP[A, Vector[K], Path](z, Vector.empty, _.whole)(NonEmptyVector.end, f)

    def flattenTrie: Map[Path, V] =
      flattenTrieP(identity)

    def flattenTrieP[P: UnivEq](f: Path => P): Map[P, V] =
      fold(UnivEq.emptyMap[P, V])((m, p, ot) =>
        ot.fold(m)(t => m.updated(f(p), t.value)))

    def flatStream: Stream[(Path, V)] = {
      def go(trie: Trie, p: Vector[K]): Stream[(Path, V)] =
        trie.toStream.flatMap { kv =>
          val k = kv._1
          @inline def result(t: Target) = (NonEmptyVector.end(p, k), t.value)
          kv._2.fold(
            b => b.target.map(result).toStream append go(b.next, p :+ k),
            t => Stream(result(t)))
        }
      go(trie, Vector.empty)
    }

    def put(loc: Path, value: V): Trie = {
      val target = Target[K, V](value)
      @inline def empty = MTrie.empty[K, V]

      @tailrec def go(t: Trie, locH: K, locT: Vector[K], unwind: Trie => Trie): Trie =
        if (locT.isEmpty) {

          // At target-path's end
          val newNode: Node =
            t.get(locH) match {
              case Some(Branch(_, next)) => Branch(Some(target), next)
              case Some(_: Target)
                 | None                  => target
            }
          unwind(t.updated(locH, newNode))

        } else {

          // Still traversing target-path
          val a = locT.head
          val b = locT.tail
          t.get(locH) match {
            case Some(Branch(ot, onext)) => go(onext, a, b, n ⇒ unwind(t.updated(locH, Branch(ot, n))))
            case ot @ Some(_: Target)    => go(empty, a, b, n ⇒ unwind(t.updated(locH, Branch(ot.asInstanceOf[Option[Target]], n))))
            case None                    => go(empty, a, b, n ⇒ unwind(t.updated(locH, Branch(None, n))))
          }
        }

      go(trie, loc.head, loc.tail, identity)
    }

    def byTarget(implicit ev: UnivEq[V]): Multimap[V, Set, Path] =
      byTargetP(identity)

    def byTargetP[P](f: Path => P)(implicit ev: UnivEq[V]): Multimap[V, Set, P] =
      fold(UnivEq.emptyMultimap[V, Set, P])((q, path, ot) =>
        ot.fold(q)(t => q.add(t.value, f(path))))

    def pathSet: Set[Path] =
      pathSetP(identity)

    def pathSetP[P: UnivEq](f: Path => P): Set[P] =
      fold(UnivEq.emptySet[P])((q, path, ot) =>
        ot.fold(q)(_ => q + f(path)))

  }
}
