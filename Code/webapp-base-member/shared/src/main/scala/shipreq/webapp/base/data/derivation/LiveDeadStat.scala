package shipreq.webapp.base.data.derivation

import scala.collection.mutable.Builder
import shipreq.base.util.LazyVal
import shipreq.base.util.fp.Semigroup.Syntax._
import shipreq.base.util.fp.{Monoid, Semigroup}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._

/**
 * Stats partitioned into Live & Dead.
 */
final case class LiveDeadStat[@specialized(Int) A] private[data](live: A,
                                                                 dead: A,
                                                                 allLazy: LazyVal[A]) {

  @inline def apply(l: Live): A =
    if (l eq Live) live else dead

  def all: A =
    allLazy.value

  def +(c: LiveDeadStat[A])(implicit a: Semigroup[A]): LiveDeadStat[A] =
    LiveDeadStat(
      a.append(live, c.live),
      a.append(dead, c.dead))

  def apply(fd: FilterDead): A =
    fd match {
      case HideDead => live
      case ShowDead => all
    }

  def map[@specialized(Int) B](f: A => B): LiveDeadStat[B] =
    LiveDeadStat(f(live), f(dead), allLazy.map(f))

  def clearDead(implicit a: Monoid[A]): LiveDeadStat[A] =
    new LiveDeadStat(live, a.zero, LazyVal.pure(live))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object LiveDeadStat {

  def apply[@specialized(Int) A: Semigroup](live: A, dead: A): LiveDeadStat[A] =
    new LiveDeadStat(live, dead, LazyVal(live |+| dead))

  def empty[@specialized(Int) A](implicit m: Monoid[A]): LiveDeadStat[A] = {
    val z = m.zero
    LiveDeadStat(z, z, LazyVal.pure(z))
  }

  implicit def univEq[A: UnivEq]: UnivEq[LiveDeadStat[A]] =
    UnivEq.derive

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Builder {

    def ofInts() = new OfInts

    final class OfInts {
      var live = 0
      var dead = 0

      def add(l: Live, n: Int): Unit =
        if (l.is(Live))
          live += n
        else
          dead += n

      def +=(u: LiveDeadStat[Int]): Unit = {
        live += u.live
        dead += u.dead
      }

      def result(): LiveDeadStat[Int] =
        new LiveDeadStat(live, dead, LazyVal.pure(live + dead))
    }

    // -----------------------------------------------------------------------------------------------------------------

    def set[A](): OfBuilders[A, Set[A]] =
      ofBuilders(Set.newBuilder)

    def vec[A](): OfBuilders[A, Vector[A]] =
      ofBuilders(Vector.newBuilder)

    def ofBuilders[A, B](newBuilder: => Builder[A, B]): OfBuilders[A, B] =
      new OfBuilders(newBuilder, newBuilder)

    final class OfBuilders[A, B](val live: Builder[A, B], val dead: Builder[A, B]) {

      @inline def add(l: Live, a: A): Unit = {
        val b = if (l eq Live) live else dead
        b += a
      }

      def ++=[F[x] <: Iterable[x]](i: LiveDeadStat[B])(implicit ev: LiveDeadStat[B] <:< LiveDeadStat[F[A]]): Unit = {
        val j = ev(i)
        live ++= j.live
        dead ++= j.dead
      }

      def result()(implicit B: Semigroup[B]): LiveDeadStat[B] = {
        val l = live.result()
        val d = dead.result()
        val a = LazyVal(B.append(l, d))
        new LiveDeadStat(l, d, a)
      }
    }

  }
}
