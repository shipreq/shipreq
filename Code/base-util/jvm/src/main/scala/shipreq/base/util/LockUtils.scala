package shipreq.base.util

import java.util.concurrent.locks.Lock
import java.util.concurrent.{TimeUnit, TimeoutException}
import shipreq.base.util.FxModule._

object LockUtils {

  sealed trait LockMechanism {
    def lock(l: Lock): Unit
  }

  object LockMechanism {

    implicit val default: LockMechanism =
      Lock

    case object Lock extends LockMechanism {
      override def lock(l: Lock): Unit =
        l.lockInterruptibly()
    }

    final case class LimitWaitTime(time: Long, unit: TimeUnit, lockName: String = null) extends LockMechanism {
      override def lock(l: Lock): Unit =
        if (!l.tryLock(time, unit)) {
          val name = Option(lockName).fold("lock")(_ + " lock")
          throw new TimeoutException(s"Failed to aquire $name in $time $unit")
        }
    }
  }

  def inMutex[A](lock: Lock)(a: => A)(implicit m: LockMechanism): A = {
    m.lock(lock)
    try a finally lock.unlock()
  }

  def maybeInMutex[A](mutex: Option[Lock])(a: => A)(implicit m: LockMechanism): A =
    mutex.fold(a)(inMutex(_)(a))

  def inMutexFx[A](lock: Lock)(fx: Fx[A])(implicit m: LockMechanism): Fx[A] =
    Fx(m.lock(lock)).bracketFx_(
      use = fx,
      release = Fx(lock.unlock()))

  def maybeInMutexFx[A](mutex: Option[Lock])(fx: Fx[A])(implicit m: LockMechanism): Fx[A] =
    mutex.fold(fx)(inMutexFx(_)(fx))
}
