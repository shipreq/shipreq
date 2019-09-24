package shipreq.webapp.server.logic

import com.typesafe.scalalogging.StrictLogging
import japgolly.univeq.UnivEq
import scalaz.\/
import shipreq.webapp.base.user._
import shipreq.webapp.server.logic.dispatch.Cookie

object Security {

  trait Algebra[F[_]] {

    val db: DB.ForSecurity[F]

    /** Protects a vulnerable action from external attacks.
      *
      * A vulnerable action could be logging in, requesting a password reset, checking the validity of a security token.
      *
      * The method of protection is left to the implementation.
      * It should at the minimum provide rate limiting.
      */
    def protect[A](vulnerable: F[A]): F[A]

    final def protectFn[A, B](vulnerable: A => F[B]): A => F[B] =
      a => protect(vulnerable(a))

    def hashPassword(p: PlainTextPassword): F[PasswordAndSalt]

    def attemptLogin(user: Username \/ EmailAddr, password: PlainTextPassword): F[Option[User]]

    def sessionRestore(cookies: Cookie.LookupFn): F[Option[SessionToken]]

    def sessionPersist(token: SessionToken): F[Cookie.Update]
  }

  // ===================================================================================================================

  final case class SessionToken(authenticatedUser: Option[User])

  object SessionToken extends StrictLogging {
    val anonymous = apply(None)

    implicit def univEq: UnivEq[SessionToken] = UnivEq.derive
  }

  // ===================================================================================================================

  sealed trait Event
  object Event {
    case object Login extends Event
    case object Register1 extends Event
    case object Register2 extends Event
    case object ResetPassword1 extends Event
    case object ResetPassword2 extends Event
  }

  sealed abstract class Result(final val isSuccess: Boolean)
  object Result {
    case object Success extends Result(true)
    case object Failure extends Result(false)
  }
}
