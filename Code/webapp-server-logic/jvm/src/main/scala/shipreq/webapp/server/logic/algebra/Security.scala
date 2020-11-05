package shipreq.webapp.server.logic.algebra

import com.typesafe.scalalogging.StrictLogging
import java.time.Instant
import java.util.UUID
import scalaz.Monad
import shipreq.webapp.base.data._
import shipreq.webapp.server.logic.data.PasswordAndSalt
import shipreq.webapp.server.logic.dispatch.Cookie

object Security {

  trait Algebra[F[_]] {

    val F: Monad[F]
    val db: DB.ForSecurity[F]

    /** Protects a vulnerable action from external attacks.
      *
      * A vulnerable action could be logging in, requesting a password reset, checking the validity of a verification token.
      *
      * The method of protection is left to the implementation.
      * It should at the minimum provide rate limiting.
      */
    def protect[A](vulnerable: F[A]): F[A]

    final def protectFn[A, B](vulnerable: A => F[B]): A => F[B] =
      a => protect(vulnerable(a))

    def hashPassword(p: PlainTextPassword): F[PasswordAndSalt]

    def attemptLogin(user: Username \/ EmailAddr, password: PlainTextPassword): F[Option[User]]

    def sessionPersist(token: SessionToken[Any]): F[Cookie.Update]

    /** Does not create any new information (not even a session id). */
    def sessionRestore(cookies: Cookie.LookupFn): F[SessionRestoreResult[Instant]]

    /** Generates a new session id if missing. */
    final def sessionRestoreOrCreate(cookies: Cookie.LookupFn): F[SessionToken[Option[Instant]]] =
      F.map(sessionRestore(cookies)) {
        case SessionRestoreResult.Success(t) => t.copy(expiry = Some(t.expiry))
        case SessionRestoreResult.Expired(t) => SessionToken.anonymous(t.sessionId)
        case SessionRestoreResult.None       => SessionToken.anonymous()
      }

    final def sessionPersistIfNew(token: SessionToken[Option[Instant]]): F[Cookie.Update] =
      if (token.expiry.isDefined)
        F.pure(Cookie.Update.empty)
      else
        sessionPersist(token)
  }

  // ===================================================================================================================

  final case class SessionId(value: String) extends AnyVal

  object SessionId {
    def random(): SessionId =
      apply(UUID.randomUUID().toString)

    implicit def univEq: UnivEq[SessionId] = UnivEq.derive
  }

  /**
    * @param expiry When the JWT expires. Read from request JWT's, ignored on write.
    */
  final case class SessionToken[+E](sessionId        : SessionId,
                                    authenticatedUser: Option[User],
                                    expiry           : E) {

    def login(u: User): SessionToken[E] =
      copy(authenticatedUser = Some(u))

    def logout: SessionToken[E] =
      copy(authenticatedUser = None)

    def withSession(st: SessionToken[Any]): SessionToken[E] =
      copy(sessionId = st.sessionId)

    def withoutExpiry: SessionToken[Unit] =
      copy(expiry = ())
  }

  object SessionToken extends StrictLogging {
    def anonymous(): SessionToken[Option[Instant]] =
      anonymous(SessionId.random())

    def anonymous(sessionId: SessionId): SessionToken[Option[Instant]] =
      apply(sessionId, None, None)

    implicit def univEq[E: UnivEq]: UnivEq[SessionToken[E]] = UnivEq.derive
  }

  sealed trait SessionRestoreResult[+E] {
    import SessionRestoreResult._

    final def modToken[F](f: SessionToken[E] => SessionToken[F]): SessionRestoreResult[F] =
      this match {
        case Success(t) => Success(f(t))
        case Expired(t) => Expired(f(t))
        case None       => None
      }
  }

  object SessionRestoreResult {
    sealed trait NonEmpty[+E] extends SessionRestoreResult[E]

    case object None extends SessionRestoreResult[Nothing]
    final case class Success[+E](token: SessionToken[E]) extends NonEmpty[E]
    final case class Expired[+E](token: SessionToken[E]) extends NonEmpty[E]

    implicit def univEq[E: UnivEq]: UnivEq[SessionRestoreResult[E]] = UnivEq.derive
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
