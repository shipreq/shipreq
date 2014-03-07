package shipreq.webapp.util

import com.google.common.cache.{CacheBuilder, CacheLoader}
import java.util.concurrent.locks.{Lock => JLock, ReentrantReadWriteLock}
import java.util.concurrent.{TimeoutException, TimeUnit}
import net.liftweb.common.Logger

/**
 * Polymorphic locks must get coarser-grained (ie. larger in scope) as they get lower.
 * The most fine-grained will be the closest to ⊤.
 *
 * Example:
 * {{{
 * trait Finger extends LockSubject
 * trait Hand   extends Finger
 * trait Arm    extends Hand
 * }}}
 *
 * Different families are combined using `with`.
 * Example: `Lock[Arm with Leg]`
 */
trait LockToken

// =====================================================================================================================

sealed trait Lock[+R <: LockToken, +W <: LockToken]

object Lock {
  type Read[+R <: LockToken] = Lock[R, _ <: LockToken]
  type Write[+W <: LockToken] = Lock[_ <: LockToken, W]
}

// =====================================================================================================================

trait PreparedLock[+R <: LockToken, +W <: LockToken] {
  def apply[T](block: Lock[R,W] => T): T
}

object PreparedLock {
  type Read[+R <: LockToken] = PreparedLock[R, _ <: LockToken]
  type Write[+W <: LockToken] = PreparedLock[_ <: LockToken, W]
}

// =====================================================================================================================

object LockProvider {
  sealed trait Ø extends LockToken

  def merge[AR <: LockToken, AW <: LockToken, BR <: LockToken, BW <: LockToken](a: Lock[AR, AW], b: Lock[BR, BW]) = a.asInstanceOf[Lock[AR with BR, AW with BW]]

  private[this] val lockInstance_ = new Lock[Ø, Ø] {}
  @inline private[util] def lockInstance[R <: LockToken, W <: LockToken] = lockInstance_.asInstanceOf[Lock[R, W]]
}

// =====================================================================================================================

import LockProvider._

trait LockProvider[LockKey <: AnyRef
, RLR <: LockToken, RLW <: LockToken
, WLR <: LockToken, WLW <: LockToken] {

  final type RL = Lock[RLR, RLW]
  final type WL = Lock[WLR, WLW]

  def read[R](id: LockKey)(block: RL => R): R

  def write[R](id: LockKey)(block: WL => R): R

  final def readM[M[_]](id: LockKey): ResourceLeaseMonad1[RL, M] =
    new ResourceLeaseMonad1[RL, M] {protected override def exec[T](f: RL => T): T = read(id)(f(_))}

  final def writeM[M[_]](id: LockKey): ResourceLeaseMonad1[WL, M] =
    new ResourceLeaseMonad1[WL, M] {protected override def exec[T](f: WL => T): T = write(id)(f(_))}

  def readP(id: LockKey): PreparedLock[RLR, RLW] = new PreparedLock[RLR, RLW] {
    override def apply[T](block: RL => T): T = read(id)(block)
  }

  def writeP(id: LockKey): PreparedLock[WLR, WLW] = new PreparedLock[WLR, WLW] {
    override def apply[T](block: WL => T): T = write(id)(block)
  }
}

// =====================================================================================================================

object DefaultLockProvider {
  def simple[LockKey <: AnyRef, T <: LockToken] = new DefaultLockProvider[LockKey, T, Ø, Ø, T]
}

class DefaultLockProvider[LockKey <: AnyRef
, RLR <: LockToken, RLW <: LockToken
, WLR <: LockToken, WLW <: LockToken]
  extends LockProvider[LockKey, RLR, RLW, WLR, WLW] with Logger {

  private class LockCreator extends CacheLoader[LockKey, ReentrantReadWriteLock] {
    override def load(key: LockKey) = new ReentrantReadWriteLock
  }

  private val lockCache = CacheBuilder.newBuilder()
                          .concurrencyLevel(32)
                          .initialCapacity(0x1000)
                          .weakValues()
                          .build(new LockCreator)

  @inline protected def getRealLock(id: LockKey): ReentrantReadWriteLock = lockCache.get(id)

  @inline private def acquireLock[T, R <: LockToken, W <: LockToken](lock: JLock, block: Lock[R, W] => T): T = {
    if (!lock.tryLock(30, TimeUnit.SECONDS)) throw new TimeoutException()
    try block(lockInstance[R, W]) finally lock.unlock
  }

  override def read[U](id: LockKey)(block: RL => U): U = acquireLock(getRealLock(id).readLock, block)

  override def write[U](id: LockKey)(block: WL => U): U = acquireLock(getRealLock(id).writeLock, block)

  /** Alias for read. Makes more sense in some situations. */
  final def shareAccess[R](id: LockKey)(block: RL => R): R = read(id)(block)

  /** Alias for write. Makes more sense in some situations. */
  final def exclusiveAccess[R](id: LockKey)(block: WL => R): R = write(id)(block)
}
