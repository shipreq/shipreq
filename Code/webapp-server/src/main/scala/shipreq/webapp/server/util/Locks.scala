package shipreq.webapp.server.util

import java.util.concurrent.locks.{Lock, ReentrantLock}
import java.util.concurrent.{ConcurrentHashMap, TimeUnit, TimeoutException}
import java.util.function.{Function => J8Fn}

trait LockUsage {
  def apply[A](lock: Lock, a: => A): A
}

object LockUsage {
  final case class MaxWait(time: Long, unit: TimeUnit) extends LockUsage {
    override def apply[A](lock: Lock, a: => A): A =
      if (lock.tryLock(time, unit)) {
        try a
        finally lock.unlock()
      } else
        throw new TimeoutException()
  }

  val Default = MaxWait(20, TimeUnit.SECONDS)
}

trait Mutex {
  def apply[A](a: => A): A
}

/** Won't scale long-term.
  * Ignoring time efficiency, there's a space leak in that mutexes are never removed or GC'd.
  */
final case class KeyedMutexes[K](usage: LockUsage) {
  private type V = Mutex

  private[this] val newLockFn =
    new J8Fn[K, V] {
      override def apply(a: K): V =
        new Mutex {
          val lock = new ReentrantLock()
          override def apply[A](a: => A): A =
            usage(lock, a)
        }
    }

  private[this] val mutexes =
    new ConcurrentHashMap[K, V]()

  def apply(key: K): Mutex =
    mutexes.computeIfAbsent(key, newLockFn)
}

