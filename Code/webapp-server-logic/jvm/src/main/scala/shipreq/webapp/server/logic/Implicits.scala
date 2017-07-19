package shipreq.webapp.server.logic

import scalaz.{-\/, Monad, \/, \/-}
import scalaz.syntax.monad._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.validation.{Composite, Simple}

private[logic] object Implicits {

  implicit class SimpleInvalidityExt(private val e: Simple.Invalidity) extends AnyVal {
    def toErrorMsg: ErrorMsg =
      ErrorMsg(Simple.Invalidity.toText(e))
  }

  implicit class CompositeInvalidityExt(private val e: Composite.Invalidity) extends AnyVal {
    def toErrorMsg: ErrorMsg =
      ErrorMsg(Composite.Invalidity.toText(e))
  }

  implicit class LeftSimpleInvalidityExt[A](private val d: Simple.Invalidity \/ A) extends AnyVal {
    def onValid[F[_], B](g: A => F[ErrorMsg \/ B])(implicit F: Monad[F]): F[ErrorMsg \/ B] =
      d match {
        case \/-(a) => g(a)
        case -\/(e) => F pure -\/(e.toErrorMsg)
      }
  }

  implicit class LeftCompositeInvalidityExt[A](private val d: Composite.Invalidity \/ A) extends AnyVal {
    def onValid[F[_], B](g: A => F[ErrorMsg \/ B])(implicit F: Monad[F]): F[ErrorMsg \/ B] =
      d match {
        case \/-(a) => g(a)
        case -\/(e) => F pure -\/(e.toErrorMsg)
      }
  }

}
