package shipreq.base.util.log

import cats.effect.Sync
import org.slf4j.MDC
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

  def para[F[_], A](body: F[A])(implicit F: Sync[F]): F[A] = {
    val add       = F.delay(unsafeAdd())
    val remove    = F.delay(unsafeRemove())
    val addAndRun = F.flatMap(add)(_ => body)
    F.flatMap(F.attempt(addAndRun)) {
      case \/-(a) => F.map(remove)(_ => a)
      case -\/(t) => F.flatMap(remove)(_ => F.raiseError(t))
    }
  }
}

object MdcValues {

  def one(key: String, value: String): MdcValues =
    new MdcValues(Map.empty[String, String].updated(key, value))
}
