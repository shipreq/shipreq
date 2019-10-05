package shipreq.base.util

import java.time._

trait CachePolicy[-T] {
  /** Type of state this policy uses. */
  type S
  /** Called when the cache is written to, ie. a new value is installed. */
  def write(value: T): S
  /** Determines whether a cached item has expired. */
  def expired(state: S): Boolean
}

/**
 * Values are cached for a maximum time duration.
 */
case class ExpireAfter(duration: Duration, now: () => Instant = () => Instant.now()) extends CachePolicy[Any] {
  override type S                     = Instant
  override def write(value: Any)      = now() plus duration
  override def expired(expiryTime: S) = now() isAfter expiryTime
}

/**
 * Cached values never expire.
 */
object NeverExpire extends CachePolicy[Any] {
  override type S                     = Unit
  override def write(value: Any)      = ()
  override def expired(expiryTime: S) = false
}

/**
 * Values are never cached.
 */
object DisableCache extends CachePolicy[Any] {
  override type S                     = Unit
  override def write(value: Any)      = ()
  override def expired(expiryTime: S) = true
}

// =====================================================================================================================

/**
 * Caches a value.
 */
class CacheVar[T](val policy: CachePolicy[T]) {
  private[this] val lock = new Object
  private var state: Option[(Some[T], policy.S)] = None

  def get: Option[T] =
    lock.synchronized {
      state match {
        case None                              => None
        case Some((_, p)) if policy.expired(p) => state = None; None
        case Some((v, _))                      => v
      }
    }

  def getOrSet(f: => T): T =
    lock.synchronized {
      get getOrElse {
        val v = f
        set(v)
        v
      }
    }

  def set(value: T): Unit =
    lock.synchronized {
      state = Some((Some(value), policy.write(value)))
    }
}
object CacheVar {
  def apply[T](policy: CachePolicy[T]) = new CacheVar[T](policy)
}

// =====================================================================================================================

/**
 * Caches the result of a function. When the cached value expires, the function is simply re-evaluated.
 */
class CacheFn[T](f: => T)(val policy: CachePolicy[T]) {
  private[this] val lock = new Object
  private var state: Option[(T, policy.S)] = None

  def value: T =
    lock.synchronized {
      state match {
        case None                              => refresh()
        case Some((_, p)) if policy.expired(p) => refresh()
        case Some((v, _))                      => v
      }
    }

  /** Ignore cache policy and force a refresh of the underlying cache value. */
  def refresh(): T =
    lock.synchronized {
      val value = f
      state = Some((value, policy.write(value)))
      value
    }
}
object CacheFn {
  def apply[T](f: => T)(policy: CachePolicy[T]) = new CacheFn[T](f)(policy)
}