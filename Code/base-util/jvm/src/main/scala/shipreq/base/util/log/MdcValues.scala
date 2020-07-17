package shipreq.base.util.log

import org.slf4j.MDC
import scalaz.{Catchable, Monad}
import shipreq.base.util.FxModule._

final class MdcValues(private val values: Map[String, String]) extends AnyVal {

  def ++(more: MdcValues): MdcValues =
    new MdcValues(values ++ more.values)

  private def unsafeAdd(): Unit =
    for ((k, v) <- values)
      MDC.put(k, v)

  private def unsafeRemove(): Unit =
    for (k <- values.keys)
      MDC.remove(k)

  def impure[A](a: => A): A =
    try {
      unsafeAdd()
      a
    } finally
      unsafeRemove()

  def impurePF[A, B](f: PartialFunction[A, B]): PartialFunction[A, B] = {
    case a if f.isDefinedAt(a) => impure(f(a))
  }

  def fx[A](body: Fx[A]): Fx[A] =
    Fx(unsafeAdd()).bracketFx_(
      release = Fx(unsafeRemove()),
      use     = body)

  def para[F[_], A](body: F[A])(implicit M: Monad[F], C: Catchable[F]): F[A] = {
    val add       = M.point(unsafeAdd())
    val remove    = M.point(unsafeRemove())
    val addAndRun = M.bind(add)(_ => body)
    M.bind(C.attempt(addAndRun)) {
      case \/-(a) => M.map(remove)(_ => a)
      case -\/(t) => M.bind(remove)(_ => C.fail(t))
    }
  }
}

object MdcValues {

  def one(key: String, value: String): MdcValues =
    new MdcValues(Map.empty[String, String].updated(key, value))
}
