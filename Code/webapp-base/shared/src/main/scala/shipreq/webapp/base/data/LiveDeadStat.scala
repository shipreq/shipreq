package shipreq.webapp.base.data

import scala.collection.{mutable, GenTraversable}
import scalaz.{Monoid, Semigroup}
import scalaz.std.anyVal.intInstance
import scalaz.syntax.semigroup._
import shipreq.base.util.univeq._

/**
 * Stats partitioned into Live & Dead.
 */
final class LiveDeadStat[A](val live: A,
                            val dead: A,
                            val all : A) {

  def +(c: LiveDeadStat[A])(implicit a: Semigroup[A]): LiveDeadStat[A] =
    LiveDeadStat(live |+| c.live, dead |+| c.dead)
}

object LiveDeadStat {
  def apply[A: Semigroup](live: A, dead: A): LiveDeadStat[A] =
    new LiveDeadStat(live, dead, live |+| dead)

  def empty[A](implicit m: Monoid[A]): LiveDeadStat[A] =
    LiveDeadStat(m.zero, m.zero)

  /**
   * Mutable [[LiveDeadStat]] builder.
   */
  final class Builder[A](implicit m: Monoid[A]) {
    var live = m.zero
    var dead = m.zero

    def mod(l: Live)(f: A => A): Unit =
      l match {
        case Live => live = f(live)
        case Dead => dead = f(dead)
      }

    def +=(u: LiveDeadStat[A]): Unit = {
      live = live |+| u.live
      dead = dead |+| u.dead
    }

    def result(): LiveDeadStat[A] =
      LiveDeadStat(live, dead)
  }

  def sum[A: Monoid](cs: GenTraversable[LiveDeadStat[A]]): LiveDeadStat[A] = {
    val b = new Builder[A]
    cs foreach (b += _)
    b.result()
  }
}

// =====================================================================================================================

/**
 * A collection of stats mapped by a key.
 */
final class LiveDeadStatMap[Key: UnivEq, A: Monoid] private[LiveDeadStatMap](val raw: Map[Key, LiveDeadStat[A]]) {
  val all: LiveDeadStat[A] =
    LiveDeadStat sum raw.values

  def apply(key: Key): LiveDeadStat[A] =
    raw.getOrElse(key, LiveDeadStat.empty[A])

  def +(that: LiveDeadStatMap[Key, A]): LiveDeadStatMap[Key, A] =
    if (that.raw.size > this.raw.size)
      that + this
    else {
      var m = raw
      for ((key, c) <- that.raw) {
        val n = m.get(key).fold(c)(_ + c)
        m = m.updated(key, n)
      }
      LiveDeadStatMap(m)
    }

  def countByValues[B: UnivEq](f: A => GenTraversable[B]): LiveDeadStatMap[B, Int] = {
    val r = new LiveDeadStatMap.Builder[B, Int]
    for (stat <- raw.values) {
      f(stat.live) foreach (r(_).live += 1)
      f(stat.dead) foreach (r(_).dead += 1)
    }
    r.result()
  }
}

object LiveDeadStatMap {
  def apply[Key: UnivEq, A: Monoid](byKey: Map[Key, LiveDeadStat[A]]): LiveDeadStatMap[Key, A] =
    new LiveDeadStatMap(byKey)

  /**
   * Mutable [[LiveDeadStatMap]] builder.
   */
  final class Builder[Key: UnivEq, A: Monoid] {
    private val map = mutable.Map.empty[Key, LiveDeadStat.Builder[A]]

    def apply(key: Key): LiveDeadStat.Builder[A] =
      map.getOrElseUpdate(key, new LiveDeadStat.Builder)

    def result(): LiveDeadStatMap[Key, A] = {
      var m: Map[Key, LiveDeadStat[A]] = UnivEq.emptyMap
      map foreach (t => m = m.updated(t._1, t._2.result()))
      LiveDeadStatMap(m)
    }
  }
}