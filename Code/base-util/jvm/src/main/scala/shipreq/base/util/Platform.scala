package shipreq.base.util

import japgolly.univeq.UnivEq
import java.util.concurrent.ConcurrentHashMap
import java.util.function.{Function => J8Fn}
import scala.collection.immutable.IntMap

// =================
// ====         ====
// ====   JVM   ====
// ====         ====
// =================

object Platform extends PlatformShared {

  override def memo[A: UnivEq, B](f: A => B): A => B = {
    val cache = new ConcurrentHashMap[A, B](128)
    val mf    = new J8Fn[A, B] { override def apply(a: A): B = f(a) }
    a => cache.computeIfAbsent(a, mf)
  }

  override def looseMemo[A: UnivEq, B](): LooseMemo[A, B] = {
    val cache = new ConcurrentHashMap[A, B](128)
    (a, b) => cache.computeIfAbsent(a, new J8Fn[A, B] { override def apply(a: A): B = b })
  }

  override def memoInt[A](f: Int => A): Int => A = {
    // This could be improved but it isn't used much so meh for now
    val lock = new AnyRef
    var m = IntMap.empty[A]
    i => lock.synchronized(
      m.getOrElse(i, {
        val a = f(i)
        m = m.updated(i, a)
        a
      })
    )
  }

  abstract class ScalaExt {
  }
}
