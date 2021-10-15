package shipreq.webapp.server.logic.util

import cats.Monad
import cats.effect.Sync
import com.typesafe.scalalogging.Logger
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.validation.lib.Implicits._
import shipreq.webapp.base.validation.lib.{Composite, Simple}

object LogicHelpers {

  implicit class LeftSimpleInvalidityExt[A](private val d: Simple.Invalidity \/ A) extends AnyVal {
    def onValid[F[_], B](g: A => F[ErrorMsg \/ B])(implicit F: Monad[F]): F[ErrorMsg \/ B] =
      d match {
        case \/-(a) => g(a)
        case -\/(e) => F pure -\/(e.toErrorMsg)
      }
  }

  implicit class LeftCompositeInvalidityExt[A](private val d: Composite.Invalidity \/ A) extends AnyVal {
    def onValid[F[_], B](g: A => F[ErrorMsg \/ B])(implicit F: Sync[F], logger: Logger): F[ErrorMsg \/ B] =
      d match {
        case \/-(a) => g(a)
        case -\/(e) => F.delay {
          // Client JS is supposed to prevent this
          val errMsg = e.toErrorMsg
          logger.warn(s"Validation failure: $errMsg")
          -\/(errMsg)
        }
      }
  }

  val Iso8601Format = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'").withZone(ZoneOffset.UTC)

  implicit class InstantExt(private val i: Instant) extends AnyVal {
    def toStringIso8601: String =
      Iso8601Format.format(i)
  }
}
