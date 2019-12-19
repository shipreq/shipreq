package shipreq.base.util.log

import org.slf4j.{MDC => M}
import scalaz.Applicative
import scalaz.syntax.applicative._
import shipreq.base.util.FxModule._

object MDC {

  final class Ctx(private val add: () => Unit, remove: () => Unit) {

    def apply[F[_], A](fa: F[A])(implicit F: Applicative[F]): F[A] = {
      F.point(add()) *> fa <* F.point(remove())
    }

    def impure[A](a: => A): A = {
      add()
      try a finally remove()
    }

    def impureWrapPF[A, B](f: PartialFunction[A, B]): PartialFunction[A, B] = {
      case a => impure(f(a))
    }
  }

  // ===================================================================================================================

  def apply[A](key: String, value: String): Ctx =
    new Ctx(() => {
      M.put(key, value)
    }, () => {
      M.remove(key)
    })

  def apply[A](key1: String, value1: String,
               key2: String, value2: String): Ctx =
    new Ctx(() => {
      M.put(key1, value1)
      M.put(key2, value2)
    }, () => {
      M.remove(key1)
      M.remove(key2)
    })

  // ===================================================================================================================

  def preserve[A](f: Fx[A]): Fx[Fx[A]] =
    Fx {
      val mdcState = M.getCopyOfContextMap
      if (mdcState eq null)
        f
      else
        Fx {
          M.setContextMap(mdcState)
          try
            f.unsafeRun()
          finally
            M.clear()
        }
    }

  def preserveUnsafe[A](f: () => A): () => A = {
    val g = preserve[A](Fx(f())).unsafeRun()
    () => g.unsafeRun()
  }
}
