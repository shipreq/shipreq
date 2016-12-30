package shipreq.base.util

import ThreadLocalRes.Around

// TODO Delete this
abstract class ThreadLocalRes[A] {
  def apply[B](f: A => B): B

  def xmap[B](f: A => B)(g: B => A): ThreadLocalRes[B]
  def onLend(onLend: A => Unit): ThreadLocalRes[A]
  def onReturn(onReturn: A => Unit): ThreadLocalRes[A]
  def wrapUse(around: Around[A]): ThreadLocalRes[A]
  def scoped[B](f: ThreadLocalRes[A] => B): B

  def scope[B, C](f: ThreadLocalRes[A] => ThreadLocalRes[B])(g: ThreadLocalRes[B] => C): C =
    scoped(s => g(f(s)))

  def map[B](f: A => B): ThreadLocalRes[(A, B)] =
    xmap(a => (a, f(a)))(_._1)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object ThreadLocalRes {

  trait Around[-R] {
    def apply[A](r: R)(a: => A): A

    def cmap[X](f: X => R): Around[X] = {
      val self = this
      new Around[X] {
        override def apply[A](x: X)(a: => A): A =
          self(f(x))(a)
      }
    }

    def compose[RR <: R](outer: Around[RR]): Around[RR] = {
      val inner = this
      new Around[RR] {
        override def apply[A](r: RR)(a: => A): A =
          outer(r)(inner(r)(a))
      }
    }
  }

  object Around {
    val id: Around[Any] =
      new Around[Any] {
        override def apply[A](x: Any)(a: => A): A = a
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class Store[A](create: () => A, release: A => Unit, val lock: AnyRef) {

    private var all = Vector.empty[A]

    private var tl = newTL

    private def newTL: ThreadLocal[A] =
      new ThreadLocal[A] {
        override protected def initialValue: A =
          lock.synchronized {
            val a = create()
            all :+= a
            a
          }
      }

    def get(): A =
      tl.get()

    def releaseAll(): Unit = {
      val as = lock.synchronized {
        val x = all
        all = Vector.empty
        tl = newTL
        x
      }
      as foreach release
    }
  }

  def apply[A](create: => A): ThreadLocalRes[A] =
    new Impl[A, A](new Store(() => create, _ => (), new AnyRef), identity, _ => (), Around.id)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private final class Impl[S, A](store: Store[S], onLend: S => A, onReturn: A => Unit, around: Around[A]) extends ThreadLocalRes[A] {
    override def apply[B](f: A => B): B = {
      val s = store.get()
      val a = onLend(s)
      try around(a)(f(a)) finally onReturn(a)
    }

    override def xmap[B](f: A => B)(g: B => A) =
      new Impl[S, B](store, f compose onLend, onReturn compose g, around cmap g)

    override def onLend(f: A => Unit) =
      new Impl[S, A](store, s => {
        val a = onLend(s)
        f(a)
        a
      }, onReturn, around)

    override def onReturn(f: A => Unit) =
      new Impl[S, A](store, onLend, a => {f(a); onReturn(a)}, around)

    override def wrapUse(outer: Around[A]) =
      new Impl[S, A](store, onLend, onReturn, around compose outer)

    override def scoped[B](f: ThreadLocalRes[A] => B): B = {
      val store2 = new Store(() => onLend(store.get()), onReturn, store.lock)
      val t = new Impl[A, A](store2, identity, _ => (), around)
      try f(t) finally store2.releaseAll()
    }
  }
}

