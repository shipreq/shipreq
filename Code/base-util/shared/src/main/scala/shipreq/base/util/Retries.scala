package shipreq.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import scala.collection.View

final case class Retries(waitTimes: Iterable[Duration]) {

  def apply(attemptsSoFar: Int): Option[Duration] =
    waitTimes.drop(attemptsSoFar).headOption

  def isEmpty: Boolean =
    waitTimes.isEmpty

  def take(n: Int): Retries =
    Retries(waitTimes.take(n))

  def takeWhile(f: Duration => Boolean): Retries =
    Retries(waitTimes.takeWhile(f))

  def pop: Option[(Duration, Retries)] =
    if (isEmpty)
      None
    else
      Some((waitTimes.head, Retries(waitTimes.tail)))

  def ++(r: Retries): Retries =
    Retries(waitTimes ++ r.waitTimes)
}

object Retries {
  private def expStream(d: Duration, factor: Double): LazyList[Duration] =
    d #:: expStream((d.toMillis * factor).millis, factor)

  def exponentially(d: Duration, factor: Double = 2): Retries =
    apply(expStream(d, factor))

  def continually(d: Duration): Retries =
    apply(View.from(Iterator.continually(d)))

  def none: Retries =
    Retries(Nil)
}
