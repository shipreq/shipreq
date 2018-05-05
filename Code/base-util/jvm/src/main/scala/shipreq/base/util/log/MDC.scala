package shipreq.base.util.log

import org.slf4j.{MDC => slf4jMDC}
import org.slf4j.spi.MDCAdapter

object MDC {

  @inline def apply(kvs: (String, Any)*): Ctx = apply(kvs)

  def apply(kvs: TraversableOnce[(String, Any)]): Ctx = {
    val map = new java.util.HashMap[String, String]
    for ((k,v) <- kvs)
      map.put(k, v.toString)
    new Ctx(_ setContextMap map)
  }

  final class Ctx(private val m: MDCAdapter => Unit) extends AnyVal {

    def apply[A](f: => A): A = {
      val mdc = slf4jMDC.getMDCAdapter
      val old = mdc.getCopyOfContextMap
      try {
        m(mdc)
        f
      } finally
        if (null eq old)
          mdc.clear()
        else
          mdc setContextMap old
    }

    def f[A, B](f: A => B): A => B =
      a => apply(f(a))

    def pf[A, B](f: PartialFunction[A, B]): PartialFunction[A, B] = {
      case a => apply(f(a))
    }
  }
}
