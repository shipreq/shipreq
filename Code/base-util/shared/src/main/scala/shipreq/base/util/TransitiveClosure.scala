package shipreq.base.util

import cats.Eval
import scala.collection.immutable.BitSet
import scala.reflect.ClassTag
import shipreq.base.util.TransitiveClosure.Filter

object TransitiveClosure {
  def auto[A: UnivEq: ClassTag](as            : IterableOnce[A])
                               (directChildren: A => Iterable[A],
                                filter        : A => Filter = Filter.followAll) = {

    val map = as.iterator.zipWithIndex.toMap
    val array = new Array[A](map.size)
    for ((k,v) <- map)
      array(v) = k
    new TransitiveClosure(map.apply, array.apply, array.length, directChildren, filter)
  }

  sealed abstract class Filter
  object Filter {

    /** Include the subject node and follow its vertices. */
    case object Follow extends Filter

    /** Include the subject node but do not follow its vertices. */
    case object Terminal extends Filter

    /** Exclude the subject node and its vertices. */
    case object Exclude extends Filter

    val followAll: Any => Filter =
      _ => Follow

    def terminalSet[A](terminals: Set[A]): A => Filter =
      a => if (terminals.contains(a)) Terminal else Follow
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
 */
final class TransitiveClosure[A: UnivEq](a2i           : A => Int,
                                         i2a           : Int => A,
                                         size          : Int,
                                         directChildren: A => Iterable[A],
                                         filter        : A => Filter) {

  private var traversing: BitSet =
    BitSet.empty

  private val closure: Array[Eval[BitSet]] =
    new Array(size)

  // Init
  for (i <- 0 until size) {
    closure(i) = Eval.later[BitSet] {
      val a = i2a(i)
      val z = BitSet.empty + i
      traversing += i
      try
        directChildren(a).foldLeft(z) { (q, c) =>
          filter(c) match {
            case Filter.Follow =>
              val ci = a2i(c)
              if (traversing(ci))
                q // cycle found
              else
                q ++ tc(ci)
            case Filter.Terminal => q + a2i(c)
            case Filter.Exclude  => q
          }
        }
      finally
        traversing -= i
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
