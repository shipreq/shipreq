package shipreq.base.util

import scala.scalajs.js.timers.{SetIntervalHandle, SetTimeoutHandle}

trait JsTimers {

  def setTimeout(interval: Double)(body: => Unit): SetTimeoutHandle

  def clearTimeout(handle: SetTimeoutHandle): Unit

  def setInterval(interval: Double)(body: => Unit): SetIntervalHandle

  def clearInterval(handle: SetIntervalHandle): Unit

}

object JsTimers {

  val real: JsTimers =
    new JsTimers {
      import scala.scalajs.js.timers.RawTimers

      def setTimeout(interval: Double)(body: => Unit) =
        RawTimers.setTimeout(() => body, interval)

      override def clearTimeout(handle: SetTimeoutHandle) =
        RawTimers.clearTimeout(handle)

      def setInterval(interval: Double)(body: => Unit) =
        RawTimers.setInterval(() => body, interval)

      override def clearInterval(handle: SetIntervalHandle) =
        RawTimers.clearInterval(handle)
    }
}