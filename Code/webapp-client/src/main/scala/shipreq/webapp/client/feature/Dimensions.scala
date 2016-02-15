package shipreq.webapp.client.feature

import monocle.Prism

object Dimensions {

  def set1[A, B, V](p: Prism[A, B])(values: Map[A, V], key: B, o: Option[V]): Map[A, V] = {
    val k = p reverseGet key
    val m = o match {
      case Some(v) => values.updated(k, v)
      case None    => values - k
    }
    m
  }

  def set2[K, S](values: Map[K, S])(k: K, merge: Option[S] => S)(isEmpty: S => Boolean): Map[K, S] = {
    val oldSeg = values.get(k)
    val newSeg = merge(oldSeg)
    if (isEmpty(newSeg))
      values - k
    else
      values.updated(k, newSeg)
  }

  def merge[A, B, VA >: VB, VB](p: Prism[A, B])(parent: Map[A, VA], child: Map[A, VB]): Map[A, VA] = {

    // Include everything from small
    var m: Map[A, VA] = child

    // Include everything from big which isn't covered by small
    for ((a, v) <- parent)
      if (p.getOption(a).isEmpty)
        m = m.updated(a, v)

    m
  }

  def iterator[A, B, V, W](p: Prism[A, B], m: Map[A, V])(f: V => W): Iterator[(B, W)] =
    m.iterator
      .map(x => p.getOption(x._1) match {
        case Some(b) => (b, f(x._2))
        case None    => null
      })
      .filter(_ ne null)

}
