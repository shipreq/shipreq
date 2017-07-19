package shipreq.webapp.base.protocol

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.Reusability
import org.scalajs.dom.ext.AjaxException
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.{ErrorMsg, Identity}

final case class ServerSideProcInvoker[I, F, O](fn: (I, O => Callback, F => Callback) => Callback) extends AnyVal {

  @inline def apply(input    : I,
                    onSuccess: O => Callback,
                    onFailure: F => Callback): Callback =
    fn(input, onSuccess, onFailure)

  def contramapInput[A](g: A => I): ServerSideProcInvoker[A, F, O] =
    new ServerSideProcInvoker[A, F, O]((a, s, f) => fn(g(a), s, f))

  def mapFailure[A](g: F => A): ServerSideProcInvoker[I, A, O] =
    new ServerSideProcInvoker[I, A, O]((i, s, f) => fn(i, s, f compose g))

  def mapOutput[A](g: O => A): ServerSideProcInvoker[I, F, A] =
    new ServerSideProcInvoker[I, F, A]((i, s, f) => fn(i, s compose g, f))

  def mergeFailure(implicit ev: ServerSideProcInvoker.MergeFailure[F, O]): ServerSideProcInvoker[I, F, ev.A] = {
    val fn2 = ev.apply(this).fn
    new ServerSideProcInvoker[I, F, ev.A]((i, s, f) => fn2(i, _.fold(f, s), f))
  }

  def onSuccess(g: (O, Callback) => Callback): ServerSideProcInvoker[I, F, O] =
    new ServerSideProcInvoker[I, F, O]((i, s, f) => fn(i, o => g(o, s(o)), f))
}

object ServerSideProcInvoker {

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

//  def apply[I, O](cp: CPproc: ServerSideProc[I, O]): ServerSideProcInvoker[I, Throwable, O] =
//    new ServerSideProcInvoker((i, s, f) => cp.call(proc)(i, _.fold(f(_).cb, s(_).cb)))

//  final class ForClientProtocol(cp: ClientProtocol) {
//
//    def apply[I, O](proc: ServerSideProc[I, O]): ServerSideProcInvoker[I, ErrorMsg, O] =
//      applyT(proc).mapThrowableToErrorMsg
//
//    def fallible[I, O](proc: ServerSideProc[I, ErrorMsg \/ O]): ServerSideProcInvoker[I, ErrorMsg, O] =
//      fallibleT(proc).mapLeftThrowableIntoErrorMsg
//
//
//    private def fallibleT[I, F, O](proc: ServerSideProc[I, F \/ O]): ServerSideProcInvoker[I, Throwable \/ F, O] =
//      new ServerSideProcInvoker((i, s, f) => callFallible(proc)(i, s, f))
//
//    private def callFallible[I, F, O](proc     : ServerSideProc[I, F \/ O])
//                                     (input    : I,
//                                      onSuccess: O => TCB.Success,
//                                      onFailure: Throwable \/ F => TCB.Failure): Callback =
//      cp.call(proc)(input, {
//        case \/-(\/-(o)) => onSuccess(o)
//        case \/-(-\/(f)) => onFailure(\/-(f))
//        case e@ -\/(_)   => onFailure(e)
//      })
//  }

  implicit def reusability[I, F, O]: Reusability[ServerSideProcInvoker[I, F, O]] =
    Reusability((a, b) => a.fn eq b.fn)

  implicit def variance[I, F, O, II <: I, FF >: F, OO >: O](a: ServerSideProcInvoker[I, F, O]): ServerSideProcInvoker[II, FF, OO] =
    new ServerSideProcInvoker(a.fn)

  def throwableToErrorMsg(t: Throwable): ErrorMsg =
    t match {
      case e: AjaxException if e.isTimeout => ErrorMsg("Server didn't respond. Please check your internet connectivity.")
      case AjaxException(_)                => ErrorMsg("Error contacting server. Please try again.")
      case tt =>
        Option(tt.getMessage).filter(_.nonEmpty) match {
          case Some(m) => ErrorMsg("Error occurred: " + m)
          case None    => ErrorMsg("Error occurred.")
        }
    }
}