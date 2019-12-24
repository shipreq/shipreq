package shipreq.webapp.server.logic

import com.typesafe.scalalogging.StrictLogging
import japgolly.univeq.UnivEq
import java.time.Instant
import java.util.UUID
import scalaz.{Monad, \/}
import shipreq.webapp.base.user._
import shipreq.webapp.server.logic.dispatch.Cookie

object Security {

  trait Algebra[F[_]] {

    val F: Monad[F]
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

    def sessionPersist(token: SessionToken): F[Cookie.Update]

    /** Does not create any new information (not even a session id). */
    def sessionRestore(cookies: Cookie.LookupFn): F[SessionRestoreResult]

    /** Generates a new session id if missing. */
    final def sessionRestoreOrCreate(cookies: Cookie.LookupFn): F[SessionToken] =
      F.map(sessionRestore(cookies)) {
        case SessionRestoreResult.Success(t) => t
        case SessionRestoreResult.Expired(t) => SessionToken.anonymous(t.sessionId)
        case SessionRestoreResult.None       => SessionToken.anonymous()
      }
  }

  // ===================================================================================================================

  final case class SessionId(value: String) extends AnyVal

  object SessionId {
    def random(): SessionId =
      apply(UUID.randomUUID().toString)

    implicit def univEq: UnivEq[SessionId] = UnivEq.derive
  }

  /**
    * @param requestTokenExpiration This is the JWT expiry according to the JWT read from the request.
    *                               Creating a new token in-memory will have this as None.
    */
  final case class SessionToken(sessionId             : SessionId,
                                authenticatedUser     : Option[User],
                                requestTokenExpiration: Option[Instant]) {

    def login(u: User): SessionToken =
      copy(authenticatedUser = Some(u))

    def logout: SessionToken =
      copy(authenticatedUser = None)

    def withSession(st: SessionToken): SessionToken =
      copy(sessionId = st.sessionId)

    def withoutExpiry: SessionToken =
      copy(requestTokenExpiration = None)
  }

  object SessionToken extends StrictLogging {
    def anonymous(): SessionToken =
      anonymous(SessionId.random())

    def anonymous(sessionId: SessionId): SessionToken =
      apply(sessionId, None, None)

    implicit def univEq: UnivEq[SessionToken] = UnivEq.derive
  }

  sealed trait SessionRestoreResult {
    import SessionRestoreResult._

    final def modToken(f: SessionToken => SessionToken): SessionRestoreResult =
      this match {
        case Success(t) => Success(f(t))
        case Expired(t) => Expired(f(t))
        case None       => None
      }
  }

  object SessionRestoreResult {
    sealed trait NonEmpty extends SessionRestoreResult

    case object None extends SessionRestoreResult
    final case class Success(token: SessionToken) extends NonEmpty
    final case class Expired(token: SessionToken) extends NonEmpty

    implicit def univEq: UnivEq[SessionRestoreResult] = UnivEq.derive
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
