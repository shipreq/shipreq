package shipreq.webapp.lib

/**
 * Methods to help with debugging.
 */
object Debug {

  implicit class DebugAnyExt[T](val v: T) extends AnyVal {
    def pp(): T = {println(v); v}
    def pp(title: Any): T = {println(s"$title: $v"); v}
    def pp(f: T => Any): T = {println(f(v)); v}
  }
}

trait DebugImplicits {
  import Debug._
  implicit def debugAnyExt[T](v: T) = DebugAnyExt(v)
}