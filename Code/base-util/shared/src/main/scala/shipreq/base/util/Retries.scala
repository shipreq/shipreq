package shipreq.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import shipreq.base.util.ScalaExt._

final case class Retries(waitTimes: List[Duration]) extends AnyVal {

  def isEmpty: Boolean =
    waitTimes.isEmpty

  def totalTime: Duration =
    waitTimes.foldLeft(Duration.ZERO)(_ plus _)

  override def toString: String =
    if (waitTimes.isEmpty)
      "Retries(Nil)"
    else
      waitTimes.mkString(totalTime + " = ", " + ", "").replace("PT", "").toLowerCase

  def pop: Option[(Duration, Retries)] =
    Option.when(waitTimes.nonEmpty)((waitTimes.head, Retries(waitTimes.tail)))
}

object Retries {
  private def expStream(d: Duration, factor: Double = 2): Stream[Duration] =
    d #:: expStream((d.toMillis * factor).millis, factor)

  def exponentiallyFrom(d: Duration, factor: Double = 2)(take: Duration => Boolean): Retries =
    apply(expStream(d, factor).takeWhile(take).toList)

  def none: Retries =
    Retries(Nil)
}
