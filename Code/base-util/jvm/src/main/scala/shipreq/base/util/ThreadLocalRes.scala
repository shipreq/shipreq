package shipreq.base.util

/**
 * Thread-local resources.
 */
final class ThreadLocalRes[A](create: () => A, cleanupFn: Vector[A] => Unit) {
  private[this] val lock = new AnyRef

  private var as = Vector.empty[A]
  private var tl = newTL
  private def newTL: ThreadLocal[A] =
    new ThreadLocal[A] {
      override protected def initialValue: A = {
        val a = create()
        lock.synchronized(as :+= a)
        a
      }
    }

  def get(): A =
    tl.get()

  def cleanup(): Unit = {
    val as2 = lock.synchronized {
      val x = as
      as = Vector.empty
      tl = newTL
      x
    }
    cleanupFn(as2)
  }

  def xmap[B](f: A => B)(g: B => A): ThreadLocalRes[B] =
    ThreadLocalRes[B](f(create()))(vs => cleanupFn(vs map g))

  def strength[B](f: A => B): ThreadLocalRes[(A, B)] =
    xmap(a => (a, f(a)))(_._1)

  def onCreate(f: A => Unit): ThreadLocalRes[A] =
    xmap(a => {f(a); a})(identity)

  def around[B](f: => B): B =
    try f finally cleanup()
}

object ThreadLocalRes {
  def apply[A](create: => A)(cleanupFn: Vector[A] => Unit): ThreadLocalRes[A] =
    new ThreadLocalRes(() => create, cleanupFn)
}
