package shipreq.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration

final case class Retries(waitTimes: Stream[Duration]) {

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
  private def expStream(d: Duration, factor: Double = 2): Stream[Duration] =
    d #:: expStream((d.toMillis * factor).millis, factor)

  def exponentially(d: Duration, factor: Double = 2): Retries =
    apply(expStream(d, factor))

  def continually(d: Duration): Retries =
    apply(Stream.continually(d))

  def none: Retries =
    Retries(Stream.empty)
}
