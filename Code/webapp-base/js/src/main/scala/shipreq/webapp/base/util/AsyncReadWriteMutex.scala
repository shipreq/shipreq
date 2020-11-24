package shipreq.webapp.base.util

import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}

final class AsyncReadWriteMutex private() {

  // Whether it's a read or write mutex is determined by readers being > 0 or not
  private var mutex: Option[AsyncCallback.Barrier] =
    None

  private var readers =
    0

  private val releaseMutex: Callback =
    CallbackTo {
      if (readers == 0) {
        val old = mutex
        mutex = None
        old
      } else
        None
    }.flatMap(Callback.traverseOption(_)(_.complete))

  private val releaseReader: Callback =
    CallbackTo {
      readers -= 1
    } >> releaseMutex

  /** not re-entrant */
  def write[A](ac: AsyncCallback[A]): AsyncCallback[A] =
    AsyncCallback.byName {

      mutex match {
        case None =>
          // Mutex empty
          val b = AsyncCallback.barrier.runNow()
          mutex = Some(b)
          ac.finallyRunSync(releaseMutex)

        case Some(b) =>
          // Mutex in use
          b.waitForCompletion >> write(ac)
      }
    }

  def read[A](ac: AsyncCallback[A]): AsyncCallback[A] =
    AsyncCallback.byName {

      mutex match {
        case None =>
          // Mutex empty
          val b = AsyncCallback.barrier.runNow()
          mutex = Some(b)
          assert(readers == 0)
          readers = 1
          ac.finallyRunSync(releaseReader)

        case Some(b) =>
          if (readers > 0) {
            // Read-mutex in use
            readers += 1
            ac.finallyRunSync(releaseReader)

          } else {
            // Write-mutex in use
            b.waitForCompletion >> read(ac)
          }
      }
    }
}

object AsyncReadWriteMutex {
  def newMutex: CallbackTo[AsyncReadWriteMutex] =
    CallbackTo(new AsyncReadWriteMutex)

  def apply(): AsyncReadWriteMutex =
    newMutex.runNow()
}