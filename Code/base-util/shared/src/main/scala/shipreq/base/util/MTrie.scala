package shipreq.base.util

import scalaz.std.map.mapEqual
import scalaz.std.option.optionInstance
import scalaz.syntax.traverse._
import scalaz.{Applicative, Equal, Order, Traverse}

/**
 * A Trie where each level is a Map of keys to nodes.
 */
object MTrie {
  type Trie[K, V] = Map[K, Node[K, V]]

  sealed abstract class Node[K, V] {
    def fold[A](b: Branch[K, V] => A, t: Value[K, V] => A): A
    def exists(b: Branch[K, V] => Boolean, v: Value[K, V] => Boolean): Boolean
    def valueIterator(): Iterator[V]
    def valueIteratorExcludingSelf(): Iterator[V]

    final def existsV(f: V => Boolean): Boolean = {
      val g = (v: Value[K, V]) => f(v.value)
      exists(_.value exists g, g)
    }

    final def getValueOrElse[A >: V](default: => A): A =
      fold(_.value.fold(default)(_.value), _.value)

    final def foldValue[A](z: => A, f: V => A): A =
      fold(_.value.fold(z)(x => f(x.value)), x => f(x.value))
  }

  final case class Branch[K, V](value: Option[Value[K, V]], next: Trie[K, V]) extends Node[K, V] {
    override def fold[A](b: Branch[K, V] => A, t: Value[K, V] => A) =
      b(this)

    override def exists(b: Branch[K, V] => Boolean, v: Value[K, V] => Boolean) =
      b(this) || next.values.exists(_.exists(b, v))

    override def valueIterator() =
      value match {
        case None    => valueIteratorExcludingSelf()
        case Some(v) => v.valueIterator() ++ valueIteratorExcludingSelf()
      }

    override def valueIteratorExcludingSelf() =
      next.valuesIterator.flatMap(_.valueIterator())
  }

  final case class Value[K, V](value: V) extends Node[K, V] {
    override def fold[A](b: Branch[K, V] => A, t: Value[K, V] => A) = t(this)
    override def exists(b: Branch[K, V] => Boolean, v: Value[K, V] => Boolean) = v(this)
    override def valueIterator() = Iterator.single(value)
    override def valueIteratorExcludingSelf() = Iterator.empty
  }

  // ===================================================================================================================

  class Types[K: UnivEq, V] {
    type Trie   = MTrie.Trie[K, V]
    type Node   = MTrie.Node[K, V]
    type Branch = MTrie.Branch[K, V]
    type Value  = MTrie.Value[K, V]
    type Entry  = (K, Node)
    type Path   = NonEmptyVector[K]
    val  Branch = MTrie.Branch.apply[K, V] _
    val  Value  = MTrie.Value.apply[K, V] _
    def  empty  = MTrie.empty[K, V]

    def fixk = new FixK[K]
  }

  def empty[K: UnivEq, V]: Trie[K, V] = UnivEq.emptyMap

  implicit def equality[K: Order, V: Equal]: Equal[Trie[K, V]] =
    Equal[Map[NonEmptyVector[K], V]] contramap (_.flattenTrie)

  class FixK[K: UnivEq] {
    type Trie  [V] = MTrie.Trie[K, V]
    type Node  [V] = MTrie.Node[K, V]
    type Branch[V] = MTrie.Branch[K, V]
    type Value [V] = MTrie.Value[K, V]

    implicit val traverseValue: Traverse[Value] =
      new Traverse[Value] {
        override def traverseImpl[G[_], A, B](fa: Value[A])(f: A => G[B])(implicit G: Applicative[G]): G[Value[B]] =
          G.map(f(fa.value))(Value(_))
      }

    implicit lazy val traverseBranch: Traverse[Branch] =
      new Traverse[Branch] {
        override def traverseImpl[G[_], A, B](fa: Branch[A])(f: A => G[B])(implicit G: Applicative[G]): G[Branch[B]] = {
          def v: G[Option[Value[B]]] = fa.value.map(_ traverse f).sequence
          def n: G[Trie[B]]          = fa.next traverse f
          G.apply2(v, n)(Branch.apply)
        }
      }

    implicit lazy val traverseNode: Traverse[Node] =
      new Traverse[Node] {
        override def traverseImpl[G[_], A, B](fa: Node[A])(f: A => G[B])(implicit G: Applicative[G]): G[Node[B]] =
          fa match {
            case n: Value[A]  => G.map(n traverse f)(x => x)
            case n: Branch[A] => G.map(n traverse f)(x => x)
          }
      }

    implicit lazy val traverseTrie: Traverse[Trie] =
      new Traverse[Trie] {
        override def traverseImpl[G[_], A, B](fa: Trie[A])(f: A => G[B])(implicit G: Applicative[G]): G[Trie[B]] = {
          val z: G[Trie[B]] = G.point(Map.empty)
          fa.foldLeft(z) { case (gtb, (k, n)) =>
            G.apply2(gtb, n traverse f)(_.updated(k, _))
          }
        }
      }
  }

  // ===================================================================================================================

  implicit class Ops[K, V](val trie: MTrie.Trie[K, V]) extends AnyVal {
    type Trie   = MTrie.Trie[K, V]
    type Node   = MTrie.Node[K, V]
    type Branch = MTrie.Branch[K, V]
    type Value  = MTrie.Value[K, V]
    type Path   = NonEmptyVector[K]
    @inline implicit private[this] def keyUnivEq: UnivEq[K] = UnivEq.force // evident from existence of trie

    def foreachValue[U](f: V => U): Unit = {
      type It = Iterator[(K, Node)]
      var more = List.empty[It]
      var it = trie.iterator
      while({
        while (it.hasNext)
          it.next()._2 match {
            case b: MTrie.Branch[K, V] =>
              for (v <- b.value)
                f(v.value)
              more ::= b.next.iterator

            case v: MTrie.Value[K, V] =>
              f(v.value)
          }
        !more.isEmpty
      }) {
        it = more.head
        more = more.tail
      }
    }

    def foreachPathAndValue[U](f: (Path, V) => U): Unit = {
      // This is a *very* hot path. Measure affect on ApplyEventBM.

      type It   = Iterator[(K, Node)]
      type Work = (It, Vector[K])

      var workQueue  = List.empty[Work]
      var it         = trie.iterator
      var pathPrefix = Vector.empty[K]

      while({
        while (it.hasNext) {
          val cur = it.next()
          val key = cur._1
          cur._2 match {
            case b: MTrie.Branch[K, V] =>
              for (v <- b.value) {
                val path = NonEmptyVector.end(pathPrefix, key)
                f(path, v.value)
              }
              val newWork = (b.next.iterator, pathPrefix :+ key)
              workQueue ::= newWork

            case v: MTrie.Value[K, V] =>
              val path = NonEmptyVector.end(pathPrefix, key)
              f(path, v.value)
          }
        }
        !workQueue.isEmpty
      }) {
        val w = workQueue.head
        it = w._1
        pathPrefix = w._2
        workQueue = workQueue.tail
      }
    }

    /**
     * Flat left fold. Sugar to expand the key-value tuple.
     */
    def foldl[A](z: A)(f: (A, K, Node) => A): A =
      trie.foldLeft(z)((a, kv) => f(a, kv._1, kv._2))

    def cataN[A](z: A)(f: (A, MTrie.Node[K, V]) => A): A =
      foldl(z)((q, _, n) =>
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

    def flatIterator(): Iterator[(Path, V)] = {
      def go(trie: Trie, p: Vector[K]): Iterator[(Path, V)] =
        trie.iterator.flatMap { kv =>
          val k = kv._1
          @inline def result(t: Value) = (NonEmptyVector.end(p, k), t.value)
          kv._2.fold(
            b => b.value.map(result).iterator ++ go(b.next, p :+ k),
            v => result(v) :: Nil)
        }
      go(trie, Vector.empty)
    }

    def atPath[A](path: Path, fail: => A)(f: Branch => A, g: Value => A): A = {
      @tailrec def go(t: Trie, pathH: K, pathT: Vector[K]): A =
        t.get(pathH) match {
          case Some(b@ Branch(_, n)) =>
            NonEmptyVector.option(pathT) match {
              case None    => f(b)
              case Some(p) => go(n, p.head, p.tail)
            }
          case Some(n@ Value(_))     => if (pathT.isEmpty) g(n) else fail
          case None                  => fail
        }
      go(trie, path.head, path.tail)
    }

    def valueAtPath[A](path: Path, fail: => A)(f: V => A): A = {
      val g: Value => A = v => f(v.value)
      atPath(path, fail)(_.value.fold(fail)(g), g)
    }

    def getNode[A](path: Path): Option[Node] =
      atPath(path, None: Option[Node])(Some.apply, Some.apply)

    def lookup(path: Path): Option[V] =
      atPath(path, None: Option[V])(_.value.map(_.value), t => Some(t.value))

    def lookupK(k: K): Option[V] =
      trie.get(k).flatMap(_.fold(_.value.map(_.value), t => Some(t.value)))

    def modify(path: Path)(f: Option[V] => V): Trie =
      valueAtPath(path, put(path, f(None)))(v => put(path, f(Some(v))))

    def modifyIfExists(path: Path)(f: V => V): Option[Trie] =
      valueAtPath[Option[Trie]](path, None)(v => Some(put(path, f(v))))

    def remove(path: Path): Trie = {

      def notLast(t: Trie, ki: Vector[K], kl: K): Trie =
        if (ki.isEmpty)
          last(t, kl)
        else {
          val k = ki.head
          t.get(k) match {
            case Some(b: Branch) =>
              val next0 = b.next
              val next = notLast(next0, ki.tail, kl)
              if (next eq next0)
                t
              else if (next.nonEmpty)
                t.updated(k, b.copy(next = next))
              else b.value match {
                case Some(v) => t.updated(k, v)
                case None    => t - k
              }
            case Some(_: Value) | None => t
          }
        }

      def last(t: Trie, k: K): Trie =
        t.get(k) match {
          case Some(Branch(None, _))       => t
          case Some(Branch(Some(_), next)) => t.updated(k, Branch(None, next))
          case Some(_: Value)              => t - k
          case None                        => t
        }

      notLast(trie, path.init, path.last)
    }

    def removeAll(paths: IterableOnce[Path]): Trie =
      paths.iterator.foldLeft(trie)(_ remove _)

    /**
     * @return A sub-trie beginning at the given path.
     */
    def dropPath(path: Path): Trie =
      atPath(path, Map.empty: Trie)(_.next, _ => Map.empty)

    def put(path: Path, value: V): Trie = {
      val v = Value[K, V](value)
      @inline def empty = MTrie.empty[K, V]

      @tailrec def go(t: Trie, pathH: K, pathT: Vector[K], unwind: Trie => Trie): Trie =
        if (pathT.isEmpty) {

          // At target-path's end
          val newNode: Node =
            t.get(pathH) match {
              case Some(Branch(_, next)) => Branch(Some(v), next)
              case Some(_: Value)
                 | None                  => v
            }
          unwind(t.updated(pathH, newNode))

        } else {

          // Still traversing target-path
          val a = pathT.head
          val b = pathT.tail
          t.get(pathH) match {
            case Some(Branch(ot, onext)) => go(onext, a, b, n => unwind(t.updated(pathH, Branch(ot, n))))
            case ot @ Some(_: Value)     => go(empty, a, b, n => unwind(t.updated(pathH, Branch(ot.asInstanceOf[Option[Value]], n))))
            case None                    => go(empty, a, b, n => unwind(t.updated(pathH, Branch(None, n))))
          }
        }

      go(trie, path.head, path.tail, identity)
    }

    def pathSet: Set[Path] =
      pathSetP(identity)

    def pathSetP[P: UnivEq](f: Path => P): Set[P] =
      cataV(UnivEq.emptySet[P])((q, path, _) => q + f(path))

    def nodeExists(b: Branch => Boolean, v: Value => Boolean): Boolean =
      trie.values.exists(_.exists(b, v))

    def nodeExistsV(f: V => Boolean): Boolean =
      trie.values.exists(_ existsV f)

    def hasValueK(k: K): Boolean =
      trie.get(k) match {
        case None               => false
        case Some(Value(_))     => true
        case Some(Branch(v, _)) => v.isDefined
      }

    def allValues: Iterator[V] =
      trie.values.iterator.flatMap {
        case b: Branch => b.value.iterator.map(_.value) ++ b.next.allValues
        case v: Value  => v.value :: Nil
      }

    @inline def add(path: Path)(implicit ev: Unit =:= V): Trie =
      put(path, ())

    def addAll(paths: IterableOnce[Path])(implicit ev: Unit =:= V): Trie =
      paths.iterator.foldLeft(trie)(_ add _)

    @inline def @+ (path: Path)(implicit ev: Unit =:= V)               : Trie = add(path)
    @inline def @++(paths: IterableOnce[Path])(implicit ev: Unit =:= V): Trie = addAll(paths)
    @inline def @- (path: Path)                                        : Trie = remove(path)
    @inline def @--(paths: IterableOnce[Path])                         : Trie = removeAll(paths)
  }
}
