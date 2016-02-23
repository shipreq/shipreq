package shipreq.webapp.client.feature

import org.scalajs.dom.console
import scala.annotation.elidable
import shipreq.base.util.Intersection

object Dimensions {

  @elidable(elidable.ASSERTION)
  def warnDiscard(key: Any): Unit =
    console.warn(s"Discarding set($key, …) because key is outside intersection.")

  def set1[A, B, V](i: Intersection[A, B])(values: Map[A, V], key: B, o: Option[V]): Map[A, V] =
    i.reverse.fold(key, k => {
      val m = o match {
        case Some(v) => values.updated(k, v)
        case None    => values - k
      }
      m
    }) {
      warnDiscard(key)
      values
    }

  def set2[A, B, V](i: Intersection[A, B])(values: Map[A, V])(key: B, merge: Option[V] => V, isEmpty: V => Boolean): Map[A, V] =
    i.reverse.fold(key, k => {
      val oldSeg = values.get(k)
      val newSeg = merge(oldSeg)
      if (isEmpty(newSeg))
        values - k
      else
        values.updated(k, newSeg)
    }) {
      warnDiscard(key)
      values
    }

  def merge[A, B, VA >: VB, VB](p: A => Option[B])(parent: Map[A, VA], child: Map[A, VB]): Map[A, VA] = {

    // Include everything from small
    var m: Map[A, VA] = child

    // Include everything from big which isn't covered by small
    for ((a, v) <- parent)
      if (p(a).isEmpty)
        m = m.updated(a, v)

    m
  }

  def iterator[A, B, V, W](p: A => Option[B], m: Map[A, V])(f: V => W): Iterator[(B, W)] =
    m.iterator
      .map(x => p(x._1) match {
        case Some(b) => (b, f(x._2))
        case None    => null
      })
      .filter(_ ne null)

}
