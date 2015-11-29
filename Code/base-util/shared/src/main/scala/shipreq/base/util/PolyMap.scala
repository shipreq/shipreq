package shipreq.base.util

import monocle.Lens

/**
 * A map where keys are polymorphic and the value type depends on the key type.
 */
object PolyMap {

  def empty[A: UnivEq]: PolyMap[A] =
    UnivEq.emptyMap

  def lens[A, K <: A](k: K) =
    new LensPendingValueType[A, K](k)

  final class LensPendingValueType[A, K <: A](val k: K) extends AnyVal {
    def apply[V]: Lens[PolyMap[A], Option[V]] =
      Lens[PolyMap[A], Option[V]](
        _.get(k).asInstanceOf[Option[V]]
      )({
        case Some(v) => _.updated(k, v)
        case None    => _ - k
      })
  }

  def Fix[A] = new Fix[A]
  final class Fix[A] {
    type PolyMap = shipreq.base.util.PolyMap[A]
    def empty(implicit ev: UnivEq[A]): PolyMap = PolyMap.empty
    def lens[K <: A](k: K) = PolyMap.lens[A, K](k)
  }

  // ===================================================================================================================

  type Table[Row, Columns] = Map[Row, PolyMap[Columns]]

  object Table {

    def empty[R: UnivEq, C: UnivEq]: Table[R, C] =
      UnivEq.emptyMap

    final class ColLensPendingValueType[R, C, K <: C](val col: K) extends AnyVal {
      def apply[V]: R => Lens[Table[R, C], Option[V]] = {
        val kl = lens[C, K](col)[V]
        r => Lens[Table[R, C], Option[V]](
          s => s.get(r).flatMap(kl.get))(
          n => s => s.updated(r, kl.set(n)(s.getOrElse(r, Map.empty))))
      }
    }

    def colLens[R, C, K <: C](col: K) =
      new ColLensPendingValueType[R, C, K](col)

    def Fix[R, C] = new Fix[R, C]
    final class Fix[R, C] {
      type Table = PolyMap.Table[R, C]
      def empty(implicit r: UnivEq[R], c: UnivEq[C]): Table = PolyMap.Table.empty
      def colLens[K <: C](col: K) = PolyMap.Table.colLens[R, C, K](col)
    }
  }
}
