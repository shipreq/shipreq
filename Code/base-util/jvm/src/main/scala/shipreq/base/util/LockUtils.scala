package shipreq.base.util

import java.util.concurrent.locks.Lock
import FxModule._

object LockUtils {

  def inMutex[A](lock: Lock)(a: => A): A = {
    lock.lockInterruptibly()
    try a finally lock.unlock()
  }

  def maybeInMutex[A](mutex: Option[Lock])(a: => A): A =
    mutex.fold(a)(inMutex(_)(a))

  def inMutexFx[A](lock: Lock)(fx: Fx[A]): Fx[A] =
    Fx(lock.lockInterruptibly()).bracketFx_(
      use = fx,
      release = Fx(lock.unlock()))

  def maybeInMutexFx[A](mutex: Option[Lock])(fx: Fx[A]): Fx[A] =
    mutex.fold(fx)(inMutexFx(_)(fx))
}
