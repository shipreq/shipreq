package shipreq.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration

final case class Retries(waitTimes: Stream[Duration]) extends AnyVal {

  def apply(attemptsSoFar: Int): Option[Duration] =
    waitTimes.drop(attemptsSoFar).headOption

  def isEmpty: Boolean =
    waitTimes.isEmpty

  def take(n: Int): Retries =
    Retries(waitTimes.take(n))

  def ++(r: Retries): Retries =
    Retries(waitTimes ++ r.waitTimes)
}

object Retries {
  private def expStream(d: Duration, factor: Double = 2): Stream[Duration] =
    d #:: expStream((d.toMillis * factor).millis, factor)

  def exponentiallyFrom(d: Duration, factor: Double = 2)(take: Duration => Boolean): Retries =
    apply(expStream(d, factor).takeWhile(take))

  def continually(d: Duration): Retries =
    apply(Stream.continually(d))

  def none: Retries =
    Retries(Stream.empty)
}
