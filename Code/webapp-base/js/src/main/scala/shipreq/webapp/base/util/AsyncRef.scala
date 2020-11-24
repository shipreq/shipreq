package shipreq.webapp.base.util

import japgolly.scalajs.react.{AsyncCallback, CallbackTo}

final class AsyncRef[A] private(allowStaleReads: Boolean) {

  private val mutex       = AsyncReadWriteMutex()
  private val initialised = AsyncCallback.barrier.runNow()
  private var _value: A   = _
  private var _hasValue   = false // TODO Remove after https://github.com/japgolly/scalajs-react/issues/818

  private val markInitialised =
    initialised.complete.asAsyncCallback

  val get: AsyncCallback[A] = {
    var readValue: AsyncCallback[A] =
      AsyncCallback.delay(_value)

    if (!allowStaleReads)
      readValue = mutex.read(readValue)

    initialised.waitForCompletion >> readValue
  }

  private def _setInMutex(c: AsyncCallback[A]): AsyncCallback[Unit] =
    for {
      a <- c
      _ <- AsyncCallback.delay { _value = a; _hasValue = true }
      _ <- markInitialised
    } yield ()

  def set     (a: => A)            : AsyncCallback[Unit] = setAsync(AsyncCallback.delay(a))
  def setSync (c: CallbackTo[A])   : AsyncCallback[Unit] = setAsync(c.asAsyncCallback)
  def setAsync(c: AsyncCallback[A]): AsyncCallback[Unit] =
    mutex.write(_setInMutex(c))

  def setIfUnset     (a: => A)            : AsyncCallback[Boolean] = setIfUnsetAsync(AsyncCallback.delay(a))
  def setIfUnsetSync (c: CallbackTo[A])   : AsyncCallback[Boolean] = setIfUnsetAsync(c.asAsyncCallback)
  def setIfUnsetAsync(c: AsyncCallback[A]): AsyncCallback[Boolean] =
    mutex.write {
      AsyncCallback.byName {
        if (_hasValue)
          AsyncCallback.pure(false)
        else
          _setInMutex(c).ret(true)
      }
    }
}

object AsyncRef {

  def newRef[A](allowStaleReads: Boolean = false): CallbackTo[AsyncRef[A]] =
    CallbackTo(new AsyncRef(allowStaleReads))

  def apply[A](allowStaleReads: Boolean = false): AsyncRef[A] =
    newRef(allowStaleReads).runNow()
}