package shipreq.webapp.base.protocol

import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo, Reusability}
import org.scalajs.dom.ext.AjaxException
import scalaz.{\/, \/-, -\/}
import shipreq.base.util.{ErrorMsg, Identity}
import shipreq.webapp.base.util.CallbackHelpers._

final class ServerSideProcInvoker[-I, F, O](private[ServerSideProcInvoker] val run: I => AsyncCallback[F \/ O]) {

  def apply(input: I): AsyncCallback[F \/ O] =
    run(input)

  def contramapInput[A](f: A => I): ServerSideProcInvoker[A, F, O] =
    new ServerSideProcInvoker[A, F, O](run compose f)

  def contramapInputCB[A, II <: I](f: A => CallbackTo[II]): ServerSideProcInvoker[A, F, O] =
    new ServerSideProcInvoker[A, F, O](f(_).asAsyncCallback.flatMap(run))

  def mapFailure[A](f: F => A): ServerSideProcInvoker[I, A, O] =
    new ServerSideProcInvoker[I, A, O](run(_).map(_.leftMap(f)))

  def mapOutput[A](f: O => A): ServerSideProcInvoker[I, F, A] =
    new ServerSideProcInvoker[I, F, A](run(_).map(_.map(f)))

  def flatMapSuccess[A](f: O => CallbackTo[A]): ServerSideProcInvoker[I, F, A] =
    new ServerSideProcInvoker[I, F, A](run(_).rightFlatMap(f(_).asAsyncCallback))

  // TODO make these methods consistent in name and form

  def mergeFailure(implicit ev: ServerSideProcInvoker.MergeFailure[F, O]): ServerSideProcInvoker[I, F, ev.A] = {
    val run2 = ev.apply(this).run
    new ServerSideProcInvoker[I, F, ev.A](run2(_).map {
      case \/-(r@ \/-(_)) => r
      case \/-(l@ -\/(_)) => l
      case l@ -\/(_)      => l
    })
  }

  def onSuccess(f: O => Callback): ServerSideProcInvoker[I, F, O] =
    new ServerSideProcInvoker[I, F, O](run(_).rightFlatTapSync(f))
}

object ServerSideProcInvoker {

  def apply[I, F, O](run: I => AsyncCallback[F \/ O]): ServerSideProcInvoker[I, F, O] =
    new ServerSideProcInvoker(run)

  def const[F, O](result: F \/ O): ServerSideProcInvoker[Any, F, O] =
    const(AsyncCallback.pure(result))

  def const[F, O](result: AsyncCallback[F \/ O]): ServerSideProcInvoker[Any, F, O] =
    new ServerSideProcInvoker(_ => result)

  def fromSimple[I, O](f: I => CallbackTo[AsyncCallback[O]]): ServerSideProcInvoker[I, ErrorMsg, O] =
    ServerSideProcInvoker[I, ErrorMsg, O] { i =>
      f(i).asAsyncCallback.flatten.attempt.map {
        case Right(o)  => \/-(o)
        case Left(err) => -\/(throwableToErrorMsg(err))
      }
    }

  /** Working around Scalac crappy type inference as usual */
  trait MergeFailure[F, O] {
    type A
    def apply[I]: ServerSideProcInvoker[I, F, O] => ServerSideProcInvoker[I, F, F \/ A]
  }
  object MergeFailure {
    implicit def a[F, _A]: MergeFailure[F, F \/ _A] { type A = _A } =
      new MergeFailure[F, F \/ _A] {
        override type A = _A
        override def apply[I] = Identity.apply
      }
  }

  implicit def reusability[I, F, O]: Reusability[ServerSideProcInvoker[I, F, O]] =
    Reusability((a, b) => a.run eq b.run)

  implicit def variance[I, F, O, II <: I, FF >: F, OO >: O](a: ServerSideProcInvoker[I, F, O]): ServerSideProcInvoker[II, FF, OO] =
    new ServerSideProcInvoker(a.run.andThen(_.widen))

  def throwableToErrorMsg(t: Throwable): ErrorMsg =
    t match {
      case e: AjaxException if e.isTimeout     => ErrorMsg("Server didn't respond. Please check your internet connectivity.")
      case AjaxException(x) if x.status == 501 => ErrorMsg("Failed to find a compatible server. Please try again, or try reloading the page.")
      case AjaxException(_)                    => ErrorMsg("Error contacting server. Please try again.")
      case tt =>
        Option(tt.getMessage).filter(_.nonEmpty) match {
          case Some(m) => ErrorMsg("Error occurred: " + m)
          case None    => ErrorMsg("Error occurred.")
        }
    }
}