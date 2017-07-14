package shipreq.webapp.server.security

import org.apache.shiro.SecurityUtils
import org.apache.shiro.authc._
import org.apache.shiro.codec.Base64
import org.apache.shiro.config.IniSecurityManagerFactory
import org.apache.shiro.crypto.SecureRandomNumberGenerator
import org.apache.shiro.crypto.hash.SimpleHash
import org.apache.shiro.realm.AuthenticatingRealm
import org.apache.shiro.util.ByteSource
import shipreq.webapp.base.user.{PlainTextPassword, User}
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.logic.{PasswordAndSalt, PasswordHash, Salt}
import shipreq.webapp.server.security.AppSecurityRealm._

/**
 * Bridge between Shiro and this app. Performs authentication checks.
 *
 * @since 25/06/2013
 */
final class AppSecurityRealm extends AuthenticatingRealm {

  override protected def doGetAuthenticationInfo(token: AuthenticationToken) = {
    // Parse input
    val userPassToken = token.asInstanceOf[UsernamePasswordToken]
    val usernameOrEmail = userPassToken.getUsername

    // Query database
    val result = Global.security.db.getUserAndPassword(usernameOrEmail).unsafePerformIO()
    if (result.isEmpty)
      throw new UnknownAccountException("No account found for [" + usernameOrEmail + "]")
    val r = result.get
    val u = r._1
    val ps = r._2

    // Result
    val info = new SimpleAuthenticationInfo(Principal(u), ps.passwordHash.value, getName)
    val saltBytes = ByteSource.Util.bytes(Base64.decode(ps.salt.base64))
    info.setCredentialsSalt(saltBytes)
    info
  }
}

object AppSecurityRealm {
  lazy val iniFactory = new IniSecurityManagerFactory("classpath:shiro.ini")

  def init(): Unit = {
    val securityManager = iniFactory.getInstance()
    SecurityUtils.setSecurityManager(securityManager)
  }

  @inline private def subject() = SecurityUtils.getSubject

  private type Principal = Some[User]
  @inline private def Principal(u: User): Principal = Some(u)
  @inline private def principalOrNull(): Principal = subject().getPrincipal.asInstanceOf[Principal]

  private[security] def authenticatedUser(): Option[User] = {
    val p = principalOrNull()
    if (p eq null) None else p
  }

  /** throws AuthenticationException */
  private[security] def loginOrThrow(usernameOrEmail: String, password: PlainTextPassword): Unit =
    SecurityUtils.getSubject.login(new UsernamePasswordToken(usernameOrEmail, password.value))

  private[security] def logout(): Unit =
    subject().logout()

  private[security] def isAuthenticated(): Boolean =
    subject().isAuthenticated

  lazy val pureHashFn: (PlainTextPassword, ByteSource) => PasswordAndSalt = {
    val ini = iniFactory.getIni
    val hashAlgorithm = ini.getSection("main").get("cm.hashAlgorithmName")
    val hashIterations = ini.getSection("main").get("cm.hashIterations").toInt
    (plainTextPassword, saltBytes) => {
      val hash = new SimpleHash(hashAlgorithm, plainTextPassword.value, saltBytes, hashIterations)
      PasswordAndSalt(PasswordHash(hash.toBase64), Salt(saltBytes.toBase64))
    }
  }

  lazy val randomHashFn: PlainTextPassword => PasswordAndSalt = {
    val hash = pureHashFn
    val rng = new SecureRandomNumberGenerator()
    p => hash(p, rng.nextBytes())
  }
}
