package shipreq.webapp.base.util

import japgolly.scalajs.react.{AsyncCallback, Callback}

final class AsyncVar[A] {

  private var value: A = _

  private val barrier = AsyncCallback.barrier.runNow()

  def set(a: => A): Callback =
    Callback {
      value = a
    } >> barrier.complete

  val get: AsyncCallback[A] =
    barrier.waitForCompletion >> AsyncCallback.delay(value)

}

object AsyncVar {
  def apply[A](): AsyncVar[A] =
    new AsyncVar
}
