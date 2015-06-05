package shipreq.webapp.base.util

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag
import scalaz.Need
import shipreq.base.util.UnivEq

object TransitiveClosure {
  def auto[A: UnivEq: ClassTag](as: TraversableOnce[A])(directChildren: A => Iterable[A], follow: A => Boolean) = {
    val map = as.toStream.zipWithIndex.toMap
    val array = new Array[A](map.size)
    for ((k,v) <- map)
      array(v) = k
    new TransitiveClosure(map.apply, array.apply, array.length, directChildren, follow)
  }
}

/**
 * Only works with acyclic digraphs.
 * Closure is also reflexive.
 *
 * Laws
 * ====
 * i ∈ [0,size)
 * a2i.i2a = id
 *
 * @param a2i Global index of a node.
 * @param i2a Node at global index.
 * @param directChildren Each direct child of a given node. Non-transitive; don't return self.
 * @param follow Whether or not transitivity holds for a given node.
 *               When false, the node is added but its children are not followed.
 */
final class TransitiveClosure[A: UnivEq](a2i           : A => Int,
                                         i2a           : Int => A,
                                         size          : Int,
                                         directChildren: A => Iterable[A],
                                         follow        : A => Boolean) {

  private val closure: Array[Need[BitSet]] =
    new Array(size)

  // Init
  for (i <- 0 until size) {
    closure(i) = Need[BitSet] {
      val a = i2a(i)
      val z = BitSet.empty + i
      directChildren(a).foldLeft(z){ (q, c) =>
        val j = a2i(c)
        if (follow(c))
          q ++ tc(j)
        else
          q + j
      }
    }
  }

  @inline private def tc(i: Int): BitSet =
    closure(i).value

//  private def a2io(a: A): Option[Int] =
//    try Some(a2i(a)) catch {
//      case _: Throwable => None
//    }

  private def a2is(a: A)(f: Int => Set[A]): Set[A] =
//    a2io(a).fold(empty)(f)
    f(a2i(a))

  @inline private def empty = UnivEq.emptySet[A]

  /** Reflexive */
  def apply(a: A): Set[A] =
    a2is(a)(i =>
      tc(i).foldLeft(empty)(_ + i2a(_)))

  /** Non-reflexive */
  def nonRefl(a: A): Set[A] =
    a2is(a)(i =>
      tc(i).foldLeft(empty)((q, j) =>
        if (i == j) q else q + i2a(j)))
}
