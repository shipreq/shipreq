package shipreq.base.util

import java.util.concurrent.ConcurrentHashMap
import java.util.function.{Function => J8Fn}

// =================
// ====         ====
// ====   JVM   ====
// ====         ====
// =================

object Platform {

  def memo[A <: AnyRef : UnivEq, B](f: A => B): A => B = {
    val cache = new ConcurrentHashMap[A, B](128)
    val mf    = new J8Fn[A, B] { override def apply(a: A): B = f(a) }
    a => cache.computeIfAbsent(a, mf)
  }

  abstract class ScalaExt {
  }
}
