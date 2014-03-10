package shipreq.webapp.test

import net.liftweb.common.Logger

object SharedGlobal {
  def apply[T >: Null](releaseDelay: Option[Long], startFn: () => T)(shutdownFn: (T) => Unit) =
    new SharedGlobal[T](releaseDelay, startFn, shutdownFn)
}

/**
 * Manages a shared resource, performing startup on-demand, and shutdown when all leases are returned.
 *
 * @param startFn Creates and starts a new instance of the resource.
 * @param shutdownFn Shuts down the resource.
 * @param releaseDelay An optional duration in milliseconds to leave the resource alive with no references before
 *                     shutting down.
 * @tparam T The resource type.
 * @since 13/05/2013
 */
class SharedGlobal[T >: Null] private(releaseDelay: Option[Long], startFn: () => T, shutdownFn: (T) => Unit) extends Logger {

  private val serverLock = new Object()
  private var refCount = 0
  private var instance: T = null

  Runtime.getRuntime.addShutdownHook(new Thread {
    new Runnable {
      def run() {
        serverLock.synchronized {
          if (refCount != 0) {
            warn(s"Force-shutting down: $instance ($refCount)")
            refCount = 0
            shutdown
          }
        }
      }
    }
  })

  /**
   * Acquires a resource reference.
   */
  def acquire(): T = {
    serverLock.synchronized {
      if (refCount == 0) {
        instance = startFn()
        debug(s"Created new: $instance")
      }
      debug(s"Acquired $refCount -> ${refCount + 1}: $instance")
      refCount += 1
      instance
    }
  }

  /**
   * Releases an acquired reference.
   *
   * @return Always returns null. Allows the caller to release and nullify their reference on one-line.
   */
  def release(): T = {
    serverLock.synchronized { debug(s"Received release request: $instance ($refCount)") }
    if (releaseDelay.isEmpty)
      releaseNow
    else
      new Thread(new Runnable {
        def run() {
          Thread.sleep(releaseDelay.get)
          releaseNow
        }
      }).start()
    null
  }

  /**
   * Releases a reference without delay. If no more references held, resource is shutdown.
   */
  private[this] def releaseNow {
    serverLock.synchronized {
      debug(s"Releasing: $instance ($refCount -> ${refCount - 1})")
      refCount -= 1
      if (refCount == 0) shutdown
    }
  }

  /**
   * Shuts down the resource.
   */
  private[this] def shutdown {
    debug("Stopping NOW: " + instance)
    try shutdownFn(instance)
    catch {
      case e: Throwable => warn(s"Error shutting down $instance", e)
    }
    instance = null
  }
}
