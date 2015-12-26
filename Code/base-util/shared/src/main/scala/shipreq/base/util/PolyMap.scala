package shipreq.base.util

/*
import monocle.{Iso, Lens}
import PolyMap.Value

/**
 * A map where keys are polymorphic and the value type depends on the key type.
 * You are trusted to ensure you don't overlap keys.
 *
 * To use, fix the super types, then create subtype access keys.
 *
 * @tparam K The super class of all key types.
 * @tparam V The super class of all value types.
 */
final class PolyMap[K, V] private[PolyMap] (private[PolyMap] val raw: Map[K, Value[Any]]) extends AnyVal {
  // .raw is    Map[K, Value[Any]]
  // instead of Map[K, Value[V]]
  // because it avoids boxing.

  @inline def isEmpty = raw.isEmpty
  @inline def nonEmpty = raw.nonEmpty
  @inline def size =  raw.size

  def apply(k: K): Option[V] =
    get(k).map(_.value)

  def get(k: K): Option[Value[V]] =
    raw.get(k).asInstanceOf[Option[Value[V]]]
}

object PolyMap {
  final case class Value[+A](value: A) extends AnyVal

  def empty[K: UnivEq, V]: PolyMap[K, V] =
    new PolyMap(UnivEq.emptyMap)

  def accessKey[KB, VB, K <: KB](k: K) =
    new AccessKeyPendingValueType[KB, VB, K](k)

  final class AccessKeyPendingValueType[KB, VB, K <: KB](private val k: K) extends AnyVal {
    def apply[V <: VB] = new AccessKey(
      Lens[PolyMap[KB, VB], Option[Value[V]]](
        _.raw.get(k).asInstanceOf[Option[Value[V]]]
      )({
        case Some(v) => m => new PolyMap(m.raw.updated(k, v))
        case None    => m => new PolyMap(m.raw - k)
      }))
  }

  class AccessKey[KB, VB, K, V](val node: Lens[PolyMap[KB, VB], Option[Value[V]]]) {
    val value: Lens[PolyMap[KB, VB], Option[V]] =
      node ^<-> Iso[Option[Value[V]], Option[V]](_.map(_.value))(_.map(Value(_)))
  }

  def Fix[K: UnivEq, V] = new Fix[K, V]
  final class Fix[K: UnivEq, V] {
    type PolyMap = shipreq.base.util.PolyMap[K, V]
    def empty: PolyMap = PolyMap.empty
    def accessKey[KK <: K](k: KK) = PolyMap.accessKey[K, V, KK](k)
  }

  // ===================================================================================================================

//  type Table[Row, Col, V] = Map[Row, PolyMap[Col, V]]
  final class Table[Row, Col, V] private[PolyMap] (private[PolyMap] val raw: Map[Row, PolyMap[Col, V]]) extends AnyVal {
    def apply(r: Row): PolyMap[Col, V] =
      raw.getOrElse(r, PolyMap.empty(UnivEq.force)) // UnivEq[Col] proven externally
  }

  object Table {

    def empty[R: UnivEq, C: UnivEq, V]: Table[R, C, V] =
      new Table(Map.empty)

//    final class ColLensPendingValueType[R, C, VB, K <: C](val col: K) extends AnyVal {
//      def apply[V <: VB]: R => Lens[Table[R, C, VB], Option[V]] = {
//        val kl = accessKey[C, VB, K](col)[V]
//        r => Lens[Table[R, C, VB], Option[V]](
//          s => kl get s(r))(
//          n => s => s.updated(r, kl.set(n)(s.getOrElse(r, Map.empty))))
//      }
//    }
//
//    def colLens[R, C, K <: C](col: K) =
//      new ColLensPendingValueType[R, C, K](col)

    final class AccessCol[R, C, VB, K <: C, V <: VB](col: K) {
      type T = Table[R, C, VB]

      private val kl = accessKey[C, VB, K](col)[V]

      private def atRow(r: R) =
        Lens[T, PolyMap[C, VB]](
          _(r))(
          m => t => new Table(t.raw.updated(r, m)))

      def node(r: R): Lens[T, Option[Value[V]]] =
        atRow(r) ^|-> kl.node

      def value(r: R): Lens[T, Option[V]] =
        node(r) ^<-> Iso[Option[Value[V]], Option[V]](_.map(_.value))(_.map(Value(_)))

      def getDirect(v: Value[Any]): V =
        v.value.asInstanceOf[V]
    }

    final class AccessColPendingValueType[R, C, VB, K <: C](private val col: K) extends AnyVal {
      def apply[V <: VB] = new AccessCol[R, C, VB, K, V](col)
    }
    def accessCol[R, C, VB, K <: C](col: K) =
      new AccessColPendingValueType[R, C, VB, K](col)

    def Fix[R: UnivEq, C: UnivEq, V] = new Fix[R, C, V]
    final class Fix[R: UnivEq, C: UnivEq, V] {
      type Table = PolyMap.Table[R, C, V]
      type AtRow = PolyMap[C, V]
      type AccessCol[VV <: V] = PolyMap.Table.AccessCol[R, C, V, _, VV]

      def empty: Table = PolyMap.Table.empty
      def accessCol[K <: C](col: K) = PolyMap.Table.accessCol[R, C, V, K](col)
    }
  }
}
*/