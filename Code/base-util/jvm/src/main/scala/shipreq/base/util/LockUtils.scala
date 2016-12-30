package shipreq.base.util

import java.util.concurrent.locks.Lock
import scalaz.effect.IO
import scalaz.syntax.bind.ToBindOps

object LockUtils {

  def inMutex[A](lock: Lock)(a: => A): A = {
    lock.lockInterruptibly()
    try a finally lock.unlock()
  }

  def inMutexIO[A](lock: Lock)(io: IO[A]): IO[A] =
    IO(lock.lockInterruptibly()) >> io.ensuring(IO(lock.unlock()))

  def maybeInMutex[A](mutex: Option[Lock])(a: => A): A =
    mutex.fold(a)(inMutex(_)(a))

  def maybeInMutexIO[A](mutex: Option[Lock])(io: IO[A]): IO[A] =
    mutex.fold(io)(inMutexIO(_)(io))
}
