package shipreq.webapp.server.logic

import com.typesafe.scalalogging.StrictLogging
import scalaz.\/
import shipreq.webapp.base.user._

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

    def attemptLogin(user: Username \/ EmailAddr, password: PlainTextPassword): F[Option[User]]

    def hashPassword(p: PlainTextPassword): F[PasswordAndSalt]

    val isAuthenticated: F[Boolean]

    val authenticatedUser: F[Option[User]]

    val logout: F[Unit]
  }

  trait Algebra2[F[_]] {

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

    def attemptLogin(user: Username \/ EmailAddr, password: PlainTextPassword): F[Option[SessionToken]]

    def sessionRestore(cookies: Cookie.LookupFn): F[SessionToken]

    def sessionPersist(token: SessionToken): F[Cookie.Update]
  }

  // ===================================================================================================================

  final case class SessionToken(authenticatedUser: Option[User])

  object SessionToken extends StrictLogging {
    val anonymous = apply(None)

//    val jwtWriter: Writer[SessionToken] = {
//      val empty = Js.Obj()
//      Writer[SessionToken](_.authenticatedUser match {
//        case Some(u) =>
//          var userFields: List[(String, Js.Value)] =
//            "id" -> Js.Str(Obfuscators.userId.obfuscate(u.id).value) ::
//            "un" -> Js.Str(u.username.value) ::
//            Nil
//          if (u.roles.nonEmpty)
//            userFields ::= "rl" -> Js.Arr(u.roles.iterator.map(Js.Str).toSeq: _*)
//          Js.Obj("usr" -> Js.Obj(userFields: _*))
//        case None => empty
//      })
//    }
//
//    val jwtReader: Reader[SessionToken] =
//      Reader { case Js.Obj(kvs@_*) =>
//        if (kvs.isEmpty)
//          anonymous
//        else {
//          def warnThrow(errMsg: String): Nothing = {
//            logger.warn(errMsg)
//            throw new RuntimeException(errMsg)
//          }
//          var id      : UserId = null
//          var username: Username = null
//          var roles   = Set.empty[String]
//          kvs foreach {
//            case ("id", Js.Str(v)) => Obfuscators.userId.deobfuscate(Obfuscated(v)) match {
//              case \/-(x) => id = x
//              case -\/(e) => warnThrow("Failed to deobfuscate user ID in JWT: " + e)
//            }
//            case ("un", Js.Str(v)) => username = Username(v)
//            case ("rl", Js.Arr(rs@_*)) => roles = rs.map {
//              case Js.Str(r) => r
//              case x => warnThrow(s"Unknown role in JWT: $x")
//            }.toSet
//            case (k, v) => warnThrow(s"Unknown data in JWT: $k:$v")
//          }
//          if (id eq null) warnThrow("JWT missing id field.")
//          if (username eq null) warnThrow("JWT missing username field.")
//          val user = User(id, username, roles)
//          SessionToken(Some(user))
//        }
//      }
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
