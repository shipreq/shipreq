package shipreq.webapp.base.data.derivation

import scala.collection.mutable
import scala.collection.mutable.Builder
import shipreq.base.util.LazyVal
import shipreq.base.util.fp.Monoid
import shipreq.base.util.fp.Monoid.Implicits._
import shipreq.base.util.univeq._

/**
 * A collection of stats mapped by a key.
 */
final case class LiveDeadStatMap[Key, @specialized(Int) A] private[derivation](raw: Map[Key, LiveDeadStat[A]],
                                                                               allLazy: LazyVal[LiveDeadStat[A]])
                                                                              (implicit A: Monoid[A]) {
  def isEmpty = raw.isEmpty

  def all: LiveDeadStat[A] =
    allLazy.value

  def apply(key: Key): LiveDeadStat[A] =
    raw.getOrElse(key, LiveDeadStat.empty[A])

//  def map[B: Monoid](f: A => B): LiveDeadStatMap[Key, B] =
//    LiveDeadStatMap(raw.mapValuesNow(_.map(f)))

  def ++(that: LiveDeadStatMap[Key, A]): LiveDeadStatMap[Key, A] =
    if (that.isEmpty)
      this
    else if (this.isEmpty)
      that
    else {
      var m = raw
      for ((key, c) <- that.raw) {
        val n = m.get(key).fold(c)(_ + c)
        m = m.updated(key, n)
      }
      val a =
        for {
          x <- this.allLazy
          y <- that.allLazy
        } yield x + y
      LiveDeadStatMap(m, a)
    }

  def countByValues[B: UnivEq](f: A => IterableOnce[B]): LiveDeadStatMap[B, Int] = {
    val r = LiveDeadStatMap.Builder.ofInts[B]()
    countByValues(r, f)
    r.result()
  }

  def countByValues[B: UnivEq](r: LiveDeadStatMap.Builder.OfInts[B], f: A => IterableOnce[B]): Unit =
    for (stat <- raw.values) {
      f(stat.live).iterator.foreach(r(_).live += 1)
      f(stat.dead).iterator.foreach(r(_).dead += 1)
    }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object LiveDeadStatMap {

  implicit def univEq[K: UnivEq, V: UnivEq]: UnivEq[LiveDeadStatMap[K, V]] =
    UnivEq.derive

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Builder {

    def ofInts[Key: UnivEq]() = new OfInts[Key]

    final class OfInts[Key: UnivEq] {
      private val map = mutable.Map.empty[Key, LiveDeadStat.Builder.OfInts]

      def apply(key: Key): LiveDeadStat.Builder.OfInts =
        map.getOrElseUpdate(key, LiveDeadStat.Builder.ofInts())

      def result(): LiveDeadStatMap[Key, Int] = {
        var m: Map[Key, LiveDeadStat[Int]] = UnivEq.emptyMap
        val all = LiveDeadStat.Builder.ofInts()
        for (kv <- map) {
          val v = kv._2.result()
          all += v
          m = m.updated(kv._1, v)
        }
        LiveDeadStatMap(m, LazyVal.pure(all.result()))
      }
    }

    // -----------------------------------------------------------------------------------------------------------------

    def set[Key: UnivEq, A](): OfBuilders[Key, Set, A] =
      ofBuilders(Set.newBuilder)

    def vec[Key: UnivEq, A](): OfBuilders[Key, Vector, A] =
      ofBuilders(Vector.newBuilder)

    def ofBuilders[Key: UnivEq, F[x] <: Iterable[x], A](newBuilder: => Builder[A, F[A]]): OfBuilders[Key, F, A] =
      new OfBuilders(newBuilder)

    final class OfBuilders[Key: UnivEq, F[x] <: Iterable[x], A](newBuilder: => Builder[A, F[A]]) {
      private[this] val map = mutable.Map.empty[Key, LiveDeadStat.Builder.OfBuilders[A, F[A]]]

      def apply(key: Key): LiveDeadStat.Builder.OfBuilders[A, F[A]] =
        map.getOrElseUpdate(key, LiveDeadStat.Builder.ofBuilders(newBuilder))

      def result()(implicit B: Monoid[F[A]]): LiveDeadStatMap[Key, F[A]] = {
        var m: Map[Key, LiveDeadStat[F[A]]] = UnivEq.emptyMap
        for (kv <- map) {
          val v = kv._2.result()
          m = m.updated(kv._1, v)
        }
        val all = LazyVal {
          val b = LiveDeadStat.Builder.ofBuilders(newBuilder)
          m.values.foreach(b ++= _)
          b.result()
        }
        LiveDeadStatMap(m, all)
      }
    }
  }
}