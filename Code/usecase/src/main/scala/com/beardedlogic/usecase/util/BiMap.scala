package com.beardedlogic.usecase.util

object BiMap {

  private val empty_ = BiMap(Map.empty[Any, Any], Map.empty[Any, Any])

  def empty[A, B] = empty_.asInstanceOf[BiMap[A, B]]

  def apply[A, B](ab: Map[A, B]): BiMap[A, B] = BiMap(ab, ab.map(e => e._2 -> e._1))

  def apply[A, B](pairs: Tuple2[A, B]*): BiMap[A, B] = BiMap(Map(pairs: _*))

  def flatten[T](pairs: Tuple2[T, T]*): Map[T, T] = Map((pairs ++ pairs.map(t => (t._2, t._1))): _*)

  def swapped[A, B](ba: Map[B, A]): BiMap[A, B] = BiMap(ba.map(e => e._2 -> e._1), ba)
}

/**
 * Simplest and quickest implementation of a bidirectional map on Earth.
 *
 * @since 31/05/2013
 */
case class BiMap[A, B](final val ab: Map[A, B], final val ba: Map[B, A]) {
  def as = ab.keySet
  def bs = ba.keySet
  def size = ab.size
  assume(ab.size == ba.size, "A->B and B->A have differing sizes. There must be a non-unique element.")
  override def toString = s"BiMap($ab)"
}

/*{
  def apply(a:A): B = ab(a)
  def apply(b:B): A = ba(b)
  def get(a:A): Option[B] = ab.get(a)
  def get(b:B): Option[A] = ba.get(b)
}*/

/**
 * Builds BiMaps.
 *
 * @since 31/05/2013
 */
final class BiMapBuilder[A, B] {
  final val ab = Map.newBuilder[A, B]
  final val ba = Map.newBuilder[B, A]

  @inline final def +=(ab: (A, B)) {
    this.ab += ab
    ba += ab.swap
  }

  @inline def update(a: A, b: B): Unit = this += (a -> b)

  def result = new BiMap(ab.result, ba.result)
}
