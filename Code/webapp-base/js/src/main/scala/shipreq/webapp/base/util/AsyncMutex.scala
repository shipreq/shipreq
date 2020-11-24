package shipreq.webapp.base.util

import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}

final class AsyncMutex private() {

  private var mutex: Option[AsyncCallback.Barrier] =
    None

  private val release: Callback =
    CallbackTo {
      val old = mutex
      mutex = None
      old
    }.flatMap(Callback.traverseOption(_)(_.complete))

  /** not re-entrant */
  def apply[A](ac: AsyncCallback[A]): AsyncCallback[A] =
    AsyncCallback.byName {

      mutex match {
        case None =>
          // Mutex empty
          val b = AsyncCallback.barrier.runNow()
          mutex = Some(b)
          ac.finallyRunSync(release)

        case Some(b) =>
          // Mutex in use
          b.waitForCompletion >> apply(ac)
      }
    }
}

object AsyncMutex {
  def newMutex: CallbackTo[AsyncMutex] =
    CallbackTo(new AsyncMutex)

  def apply(): AsyncMutex =
    newMutex.runNow()
}