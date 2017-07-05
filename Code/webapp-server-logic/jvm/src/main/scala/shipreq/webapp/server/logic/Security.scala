package shipreq.webapp.server.logic

import scalaz.\/
import shipreq.base.util.Permission
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

    def attemptLogin(user: Username \/ EmailAddr, password: PlainTextPassword): F[Permission]

    def hashPassword(p: PlainTextPassword): F[PasswordAndSalt]
  }

}
