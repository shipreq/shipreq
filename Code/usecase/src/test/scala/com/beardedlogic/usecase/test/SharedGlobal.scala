package com.beardedlogic.usecase.test

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
class SharedGlobal[T >: Null] private(releaseDelay: Option[Long], startFn: () => T, shutdownFn: (T) => Unit) {

  private val serverLock = new Object()
  private var refCount = 0
  private var instance: T = null

  Runtime.getRuntime.addShutdownHook(new Thread {
    new Runnable {
      def run() {
        serverLock.synchronized {
          if (refCount != 0) {
            println(s"Force-shutting down: $instance ($refCount)")
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
      // println(s"Started: $refCount $instance")
      if (refCount == 0) {
        instance = startFn()
        // println(s"Started: $instance")
      }
      // println(s"Acquired $refCount -> ${refCount + 1}: $instance")
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
    // serverLock.synchronized { println(s"release() for $instance ($refCount)") }
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
      // println(s"Releasing $refCount -> ${refCount - 1}: $instance")
      refCount -= 1
      if (refCount == 0) shutdown
    }
  }

  /**
   * Shuts down the resource.
   */
  private[this] def shutdown {
    // println("Stopping NOW: " + instance)
    try shutdownFn(instance)
    catch {
      case e: Throwable => println(s"Error shutting down $instance"); e.printStackTrace
    }
    instance = null
  }
}
