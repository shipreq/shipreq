package shipreq.webapp.server.security

import org.apache.shiro.authc.AuthenticationException
import scalaz.syntax.monad._
import scalaz.{Monad, \/}
import shipreq.webapp.base.user._
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.logic.{DB, PasswordAndSalt, Security}

final class SecurityInterpreter[F[_]](implicit F     : Monad[F],
                                               secDb : DB.ForSecurity[F],
                                               config: ServerConfig) extends Security.Algebra[F] {

  override val db = secDb

  private[this] val delay: F[Unit] =
    config.attackFrustrationDelayMs match {
      case 0 => F.point(())
      case ms => F.point(Thread.sleep(ms))
    }

  override def protect[A](vulnerable: F[A]): F[A] =
    delay >> vulnerable

  override def attemptLogin(user: Username \/ EmailAddr, password: PlainTextPassword) =
    F.point {
      val userOrEmail = user.fold(_.value, _.value)
      try {
        AppSecurityRealm.loginOrThrow(userOrEmail, password)
        AppSecurityRealm.authenticatedUser()
      } catch {
        case _: AuthenticationException => None
      }
    }

  private[this] val hashFn = AppSecurityRealm.randomHashFn

  override def hashPassword(p: PlainTextPassword): F[PasswordAndSalt] =
    F.point(hashFn(p))

  override val isAuthenticated: F[Boolean] =
    F.point(AppSecurityRealm.isAuthenticated())

  override val authenticatedUser: F[Option[User]] =
    F.point(AppSecurityRealm.authenticatedUser())

  override val logout: F[Unit] =
    F.point(AppSecurityRealm.logout())
}
