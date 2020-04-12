package shipreq.webapp.base.data

import japgolly.microlibs.stdlib_ext.StdlibExt._
import scala.collection.{mutable, Iterable}
import scalaz.{Monoid, Semigroup}
import scalaz.std.anyVal.intInstance
import scalaz.syntax.semigroup._
import shipreq.base.util.univeq._

/**
 * Stats partitioned into Live & Dead.
 */
final case class LiveDeadStat[A] private[LiveDeadStat](live: A, dead: A, all: A) {

  def +(c: LiveDeadStat[A])(implicit a: Semigroup[A]): LiveDeadStat[A] =
    LiveDeadStat(live |+| c.live, dead |+| c.dead)

  def apply(fd: FilterDead): A =
    fd match {
      case HideDead => live
      case ShowDead => all
    }

  def map[B](f: A => B): LiveDeadStat[B] =
    LiveDeadStat(f(live), f(dead), f(all))

  def clearDead(implicit a: Monoid[A]): LiveDeadStat[A] =
    new LiveDeadStat(live, a.zero, live)
}

object LiveDeadStat {
  def apply[A: Semigroup](live: A, dead: A): LiveDeadStat[A] =
    new LiveDeadStat(live, dead, live |+| dead)

  def empty[A](implicit m: Monoid[A]): LiveDeadStat[A] =
    LiveDeadStat(m.zero, m.zero)

  implicit def univEq[A: UnivEq]: UnivEq[LiveDeadStat[A]] =
    UnivEq.derive

  def newBuilder[A: Monoid]: Builder[A] =
    new Builder

  /**
   * Mutable [[LiveDeadStat]] builder.
   */
  final class Builder[A](implicit m: Monoid[A]) {
    var live = m.zero
    var dead = m.zero

    def add(l: Live, a: A): Unit =
      mod(l)(_ |+| a)

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

  def sum[A: Monoid](cs: Iterable[LiveDeadStat[A]]): LiveDeadStat[A] = {
    val b = new Builder[A]
    cs foreach (b += _)
    b.result()
  }
}

// =====================================================================================================================

/**
 * A collection of stats mapped by a key.
 */
final case class LiveDeadStatMap[Key: UnivEq, A: Monoid] private[LiveDeadStatMap](raw: Map[Key, LiveDeadStat[A]]) {
  def isEmpty = raw.isEmpty

  val all: LiveDeadStat[A] =
    LiveDeadStat sum raw.values

  def apply(key: Key): LiveDeadStat[A] =
    raw.getOrElse(key, LiveDeadStat.empty[A])

  def map[B: Monoid](f: A => B): LiveDeadStatMap[Key, B] =
    LiveDeadStatMap(raw.mapValuesNow(_.map(f)))

  def +(that: LiveDeadStatMap[Key, A]): LiveDeadStatMap[Key, A] =
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
      LiveDeadStatMap(m)
    }

  def countByValues[B: UnivEq](f: A => IterableOnce[B]): LiveDeadStatMap[B, Int] = {
    val r = new LiveDeadStatMap.Builder[B, Int]
    countByValues(r, f)
    r.result()
  }

  def countByValues[B: UnivEq](r: LiveDeadStatMap.Builder[B, Int], f: A => IterableOnce[B]): Unit = {
    for (stat <- raw.values) {
      f(stat.live).foreach(r(_).live += 1)
      f(stat.dead).foreach(r(_).dead += 1)
    }
  }
}

object LiveDeadStatMap {

  implicit def univEq[K: UnivEq, V: UnivEq]: UnivEq[LiveDeadStatMap[K, V]] =
    UnivEq.derive

  def newBuilder[Key: UnivEq, A: Monoid]: Builder[Key, A] =
    new Builder

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