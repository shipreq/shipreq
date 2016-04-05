package shipreq.webapp.base.data

import scala.collection.{mutable, GenTraversable}
import scalaz.{Monoid, Semigroup}
import scalaz.std.anyVal.intInstance
import scalaz.syntax.semigroup._
import shipreq.base.util.univeq._

/**
 * Stats partitioned into Live & Dead.
 */
final class LDStat[A](val live: A,
                      val dead: A,
                      val all : A) {

  def +(c: LDStat[A])(implicit a: Semigroup[A]): LDStat[A] =
    LDStat(live |+| c.live, dead |+| c.dead)
}

object LDStat {
  def apply[A: Semigroup](live: A, dead: A): LDStat[A] =
    new LDStat(live, dead, live |+| dead)

  def empty[A](implicit m: Monoid[A]): LDStat[A] =
    LDStat(m.zero, m.zero)

  /**
   * Mutable [[LDStat]] builder.
   */
  final class Builder[A](implicit m: Monoid[A]) {
    var live = m.zero
    var dead = m.zero

    def mod(l: Live)(f: A => A): Unit =
      l match {
        case Live => live = f(live)
        case Dead => dead = f(dead)
      }

    def +=(u: LDStat[A]): Unit = {
      live = live |+| u.live
      dead = dead |+| u.dead
    }

    def result(): LDStat[A] =
      LDStat(live, dead)
  }

  def sum[A: Monoid](cs: GenTraversable[LDStat[A]]): LDStat[A] = {
    val b = new Builder[A]
    cs foreach (b += _)
    b.result()
  }
}

// =====================================================================================================================

/**
 * A collection of stats mapped by a key.
 */
final class LDStats[Key: UnivEq, A: Monoid] private[LDStats](val raw: Map[Key, LDStat[A]]) {
  val all: LDStat[A] =
    LDStat sum raw.values

  def apply(key: Key): LDStat[A] =
    raw.getOrElse(key, LDStat.empty[A])

  def +(that: LDStats[Key, A]): LDStats[Key, A] =
    if (that.raw.size > this.raw.size)
      that + this
    else {
      var m = raw
      for ((key, c) <- that.raw) {
        val n = m.get(key).fold(c)(_ + c)
        m = m.updated(key, n)
      }
      LDStats(m)
    }

  def countByValues[B: UnivEq](f: A => GenTraversable[B]): LDStats[B, Int] = {
    val r = new LDStats.Builder[B, Int]
    for (stat <- raw.values) {
      f(stat.live) foreach (r(_).live += 1)
      f(stat.dead) foreach (r(_).dead += 1)
    }
    r.result()
  }
}

object LDStats {
  def apply[Key: UnivEq, A: Monoid](byKey: Map[Key, LDStat[A]]): LDStats[Key, A] =
    new LDStats(byKey)

  /**
   * Mutable [[LDStats]] builder.
   */
  final class Builder[Key: UnivEq, A: Monoid] {
    private val map = mutable.Map.empty[Key, LDStat.Builder[A]]

    def apply(key: Key): LDStat.Builder[A] =
      map.getOrElseUpdate(key, new LDStat.Builder)

    def result(): LDStats[Key, A] = {
      var m: Map[Key, LDStat[A]] = UnivEq.emptyMap
      map foreach (t => m = m.updated(t._1, t._2.result()))
      LDStats(m)
    }
  }
}