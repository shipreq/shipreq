package shipreq.base.util

import scala.reflect.ClassTag

/** Fast, efficient lookup functions for static data.
  *
  * Creation verifies key uniqueness and throws runtime exceptions on failure.
  */
object StaticLookupFn {

  def arrayBy[A: ClassTag](as: Traversable[A])(key: A => Int): Int => Option[A] =
    array(as.map(a => (key(a), a)))

  def array[A: ClassTag](as: Traversable[(Int, A)]): Int => Option[A] =
    if (as.isEmpty)
      _ => None
    else {
      val len = as.toIterator.map(_._1).max
      val array = Array.fill[Option[A]](len)(None)
      for ((i, a) <- as) {
        assert(i >= 0, s"Indices can't be negative. Found: $i")
        for (a2 <- array(i))
          assert(false, s"Duplicates for index $i: $a and $a2")
        array(i) = Some(a)
      }
      i => if (i >= 0 && i < len) array(i) else None
    }

}
