package shipreq.webapp.base.util

import japgolly.scalajs.react._
import scala.util.Try
import scalaz.{-\/, \/, \/-}

object CallbackHelpers {

  def mergeOptionalCallbacks(a: Option[Callback], b: Option[Callback]): Option[Callback] =
    if (a.isDefined && b.isDefined)
      Some(a.get >> b.get)
    else
      a orElse b

  final class HelperAsyncCallbackDisj[E, A](private val underlying: (Try[E \/ A] => Callback) => Callback) extends AnyVal {
    @inline private def self: AsyncCallback[E \/ A] =
      AsyncCallback(underlying)

    def leftFlatFlatMap[F](f: E => AsyncCallback[F \/ A]): AsyncCallback[F \/ A] =
      self.flatMap {
        case r@ \/-(_) => AsyncCallback.pure(r)
        case -\/(e)    => f(e)
      }

    def leftFlatMap[F](f: E => AsyncCallback[F]): AsyncCallback[F \/ A] =
      leftFlatFlatMap(f.andThen(_.map(-\/(_))))

    def leftFlatTap[F](f: E => AsyncCallback[F]): AsyncCallback[E \/ A] =
      self.flatMap {
        case l@ -\/(e) => f(e).ret(l)
        case r@ \/-(_) => AsyncCallback.pure(r)
      }

    def leftFlatTapSync[F](f: E => CallbackTo[F]): AsyncCallback[E \/ A] =
      leftFlatTap(f.andThen(_.asAsyncCallback))

    def rightFlatFlatMap[B](f: A => AsyncCallback[E \/ B]): AsyncCallback[E \/ B] =
      self.flatMap {
        case \/-(a)    => f(a)
        case l@ -\/(_) => AsyncCallback.pure(l)
      }

    def rightFlatMap[B](f: A => AsyncCallback[B]): AsyncCallback[E \/ B] =
      rightFlatFlatMap(f.andThen(_.map(\/-(_))))

    def rightFlatTap[B](f: A => AsyncCallback[B]): AsyncCallback[E \/ A] =
      self.flatMap {
        case r@ \/-(a) => f(a).ret(r)
        case l@ -\/(_) => AsyncCallback.pure(l)
      }

    def rightFlatTapSync[B](f: A => CallbackTo[B]): AsyncCallback[E \/ A] =
      rightFlatTap(f.andThen(_.asAsyncCallback))
  }

  implicit def HelperAsyncCallbackDisj[E, A](a: AsyncCallback[E \/ A]): HelperAsyncCallbackDisj[E, A] =
    new HelperAsyncCallbackDisj(a.completeWith)
}
