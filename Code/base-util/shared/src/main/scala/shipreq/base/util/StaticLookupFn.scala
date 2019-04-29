package shipreq.base.util

import japgolly.univeq.UnivEq
import scala.reflect.ClassTag

/** Fast, efficient lookup functions for static data.
  *
  * Creation verifies key uniqueness and throws runtime exceptions on failure.
  */
object StaticLookupFn {

  def array[A: ClassTag](as: Traversable[(Int, A)]): Int => Option[A] =
    if (as.isEmpty)
      _ => None
    else {
      val len = as.toIterator.map(_._1).max + 1
      val array = Array.fill[Option[A]](len)(None)
      for ((i, a) <- as) {
        assert(i >= 0, s"Indices can't be negative. Found: $i")
        for (a2 <- array(i))
          fail(s"Duplicates for index $i: $a and $a2")
        array(i) = Some(a)
      }
      i => if (i >= 0 && i < len) array(i) else None
    }

  def arrayBy[A: ClassTag](as: Traversable[A])(key: A => Int): Int => Option[A] =
    array(as.map(a => (key(a), a)))

  def unsafeArray[A >: Null : ClassTag](as: Traversable[(Int, A)]): Int => A = {
    val len = as.toIterator.map(_._1).max + 1
    val array = Array.fill[A](len)(null)
    for ((i, a) <- as) {
      assert(i >= 0, s"Indices can't be negative. Found: $i")
      val a2 = array(i)
      assert(null == a2, s"Duplicates for index $i: $a and $a2")
      array(i) = a
    }
    array.apply
  }

  def unsafeArrayBy[A >: Null : ClassTag](as: Traversable[A])(key: A => Int): Int => A =
    unsafeArray(as.map(a => (key(a), a)))

  // ===================================================================================================================

  /** Call .get or .array yourself on the result. */
  def map[K: UnivEq, V](kvs: TraversableOnce[(K, V)]): Map[K, V] = {
    var m = Map.empty[K, V]
    for ((k, v) <- kvs)
      m.get(k) match {
        case None     => m = m.updated(k, v)
        case Some(v0) => fail(s"Duplicates for $k: $v0 and $v")
      }
    m
  }

  /** Call .get or .array yourself on the result. */
  def mapBy[K: UnivEq, V](vs: TraversableOnce[V])(k: V => K): Map[K, V] =
    map(vs.map(v => k(v) -> v))

  // ===================================================================================================================

  private def assert(t: Boolean, e: => String): Unit =
    if (!t) fail(e)

  private def fail(e: String): Nothing =
    throw new ExceptionInInitializerError(e)
}
