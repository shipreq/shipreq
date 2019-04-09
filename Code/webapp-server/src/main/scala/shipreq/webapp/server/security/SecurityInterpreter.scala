package shipreq.webapp.server.security

import com.typesafe.scalalogging.StrictLogging
import io.jsonwebtoken.{Claims, JwtParser, Jwts}
import io.jsonwebtoken.security.Keys
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import java.security.SecureRandom
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import net.logstash.logback.encoder.org.apache.commons.lang.StringEscapeUtils
import org.apache.shiro.authc.AuthenticationException
import scala.concurrent.blocking
import scala.util.control.NonFatal
import scalaz.syntax.monad._
import scalaz.{-\/, Monad, \/, \/-}
import shipreq.base.ops.Trace
import shipreq.webapp.base.data.Obfuscated
import shipreq.webapp.base.user._
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.logic.Cookie.LookupFn
import shipreq.webapp.server.logic.Security.SessionToken
import shipreq.webapp.server.logic._

final class SecurityInterpreter[F[_]](implicit F: Monad[F],
                                      config : ServerConfig.Security,
                                      secDb  : DB.ForSecurity[F],
                                      trace  : Trace.Algebra[F]) extends Security.Algebra[F] {

  override val db = secDb

  private[this] val fUnit = F.point(())

  private[this] val delay: F[Unit] =
    config.attackFrustrationDelayMs match {
      case 0  => fUnit
      case ms =>
        val f = F.point(blocking(Thread.sleep(ms)))
        trace.newSpan("Security delay")(_ => f)
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

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final class SecurityInterpreter2[F[_]](implicit F: Monad[F],
                                       config : ServerConfig.Security,
                                       secDb  : DB.ForSecurity[F],
                                       trace  : Trace.Algebra[F]) extends Security.Algebra2[F] with StrictLogging {
  override val db = secDb

  private[this] val fUnit                    = F.point(())
  private[this] val fNewToken                = F pure SessionToken.anonymous
  private[this] val passwordSecretKeyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512")
  private[this] val cookieName               = Cookie.Name("shipreq.jwt")
  private[this] val jwtMainKey               = Keys.hmacShaKeyFor(config.jwtSecret.bytes)
  private[this] val jwtMainParser            = Jwts.parser().setSigningKey(jwtMainKey)

  private[this] val delay: F[Unit] =
    config.attackFrustrationDelayMs match {
      case 0  => fUnit
      case ms =>
        val f = F.point(blocking(Thread.sleep(ms)))
        trace.newSpan("Security delay")(_ => f)
    }

  override def protect[A](vulnerable: F[A]): F[A] =
    delay >> vulnerable

  override def hashPassword(p: PlainTextPassword): F[PasswordAndSalt] = F point {
    val saltBytes = new Array[Byte](config.passwordSaltLength)
    new SecureRandom().nextBytes(saltBytes)
    val salt = Salt.fromBytes(saltBytes)
    val hash = doHash(p, saltBytes)
    PasswordAndSalt(hash, salt)
  }

  private def doHash(p: PlainTextPassword, saltBytes: Array[Byte]): PasswordHash = {
    val keySpec = new PBEKeySpec(p.value.toCharArray, saltBytes, 24576, 512)
    val hashBytes = passwordSecretKeyFactory.generateSecret(keySpec).getEncoded
    PasswordHash.fromBytes(hashBytes)
  }

  override def attemptLogin(id: Username \/ EmailAddr, providedPassword: PlainTextPassword): F[Option[SessionToken]] =
    db.getUserAndPassword(id).map(_.flatMap { case (user, real) =>
      val providedHash = doHash(providedPassword, real.salt.toBytes)
      Option.when(providedHash ==* real.passwordHash) {
        SessionToken(Some(user))
      }
    })

  private[this] final val claimUserId = "uid"
  private[this] final val claimRoles  = "rls"

  override def sessionPersist(token: SessionToken): F[Cookie.Update] = F.point {
    val jws: String = {
      val b = Jwts.builder()

      val now = System.currentTimeMillis()
      val exp = new java.util.Date(now + config.jwtLifespanMs)
      b.setExpiration(exp)

      for (u <- token.authenticatedUser) {
        b.claim(claimUserId, Obfuscators.userId.obfuscate(u.id).value)
        b.setSubject(u.username.value)
        if (u.roles.nonEmpty)
          b.claim(claimRoles, u.roles.mkString(","))
      }

      b.signWith(jwtMainKey).compact()
    }

    val cookie = Cookie(
      name        = cookieName,
      value       = jws,
      maxAgeInSec = config.jwtCookieMaxAgeInSecSome,
      httpOnly    = config.jwtCookieHttpOnlySome,
      secure      = config.jwtCookieSecureSome)

    Cookie.Update.add(cookie)
  }

  private def _parseAndVerifyJws(jws: String, parser: JwtParser): Option[SessionToken] =
    try {
      def warnThrow(errMsg: String): Nothing = {
        logger.warn(errMsg)
        throw new RuntimeException(errMsg)
      }

      val claims   = parser.parseClaimsJws(jws).getBody
      val username = Username(claims.getSubject)
      val userIdOb = Obfuscated[UserId](claims.get(claimUserId, classOf[String]))
      val userId   = Obfuscators.userId.deobfuscate(userIdOb) match {
        case \/-(x) => x
        case -\/(e) => warnThrow(s"Failed to deobfuscate user ID ${StringEscapeUtils.escapeJava(userIdOb.value)}: $e")
      }
      val roles = claims.get(claimRoles) match {
        case null      => Set.empty[String]
        case s: String => s.split(',').toSet
        case x         => warnThrow(s"Failed to parse roles: $x")
      }
      val user = User(userId, username, roles)
      Some(SessionToken(Some(user)))

    } catch {
      case NonFatal(_) =>
        None
    }

  private[this] val parseAndVerifyJws: String => Option[SessionToken] =
    config.jwtSecretPrevious match {
      case None =>
        _parseAndVerifyJws(_, jwtMainParser)

      case Some(altKey) =>
        val altParser = Jwts.parser().setSigningKey(Keys.hmacShaKeyFor(altKey.bytes))
        j => _parseAndVerifyJws(j, jwtMainParser) orElse _parseAndVerifyJws(j, altParser)
    }

  override def sessionRestore(cookies: LookupFn): F[SessionToken] =
    cookies(cookieName) match {
      case Some(jws) =>
        F.point {
          parseAndVerifyJws(jws).getOrElse {
            logger.warn("Failed to parse/verify JWT: " + StringEscapeUtils.escapeJava(jws))
            SessionToken.anonymous
          }
        }

      case None => fNewToken
    }

}
