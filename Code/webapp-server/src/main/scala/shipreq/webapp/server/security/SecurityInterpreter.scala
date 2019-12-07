package shipreq.webapp.server.security

import com.typesafe.scalalogging.StrictLogging
import io.jsonwebtoken.{JwtParser, Jwts}
import io.jsonwebtoken.security.Keys
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import java.security.SecureRandom
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import org.apache.commons.text.StringEscapeUtils
import scala.concurrent.blocking
import scala.util.{Failure, Success, Try}
import scalaz.syntax.monad._
import scalaz.{-\/, Monad, \/, \/-}
import shipreq.base.ops.Trace
import shipreq.webapp.base.data.Obfuscated
import shipreq.webapp.base.user._
import shipreq.webapp.server.ServerLogicConfig
import shipreq.webapp.server.logic.dispatch.Cookie
import shipreq.webapp.server.logic.Security.SessionToken
import shipreq.webapp.server.logic._

object SecurityInterpreter {
  val cookieName = Cookie.Name("jwt")
}

final class SecurityInterpreter[F[_]](implicit F: Monad[F],
                                      config : ServerLogicConfig.Security,
                                      secDb  : DB.ForSecurity[F],
                                      trace  : Trace.Algebra[F]) extends Security.Algebra[F] with StrictLogging {
  import SecurityInterpreter._

  override val db = secDb

  private[this] val fUnit                    = F.point(())
  private[this] val fNoToken                 = F.pure[Option[SessionToken]](None)
  private[this] val passwordSecretKeyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512")
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

  override def attemptLogin(id: Username \/ EmailAddr, providedPassword: PlainTextPassword): F[Option[User]] =
    db.getUserAndPassword(id).map(_.flatMap { case (user, real) =>
      val providedHash = doHash(providedPassword, real.salt.toBytes)
      Option.when(providedHash ==* real.passwordHash)(user)
    })

  private[this] final val claimUserId = "uid"

  override def sessionPersist(token: SessionToken): F[Cookie.Update] = F.point {
    val jws: String = {
      val b = Jwts.builder()

      val now = System.currentTimeMillis()
      val exp = new java.util.Date(now + config.jwtLifespanMs)
      b.setExpiration(exp)

      for (u <- token.authenticatedUser) {
        b.claim(claimUserId, Obfuscators.userId.obfuscate(u.id).value)
        b.setSubject(u.username.value)
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

  private def _parseAndVerifyJws(jws: String, parser: JwtParser): Try[SessionToken] =
    Try {
      def fail(errMsg: String): Nothing = {
        // logger.warn(errMsg)
        throw new RuntimeException(errMsg)
      }

      val claims = parser.parseClaimsJws(jws).getBody
      val subject = claims.getSubject
      if (subject eq null)
        SessionToken.anonymous
      else {
        val username = Username(subject)
        val userIdOb = Obfuscated[UserId](claims.get(claimUserId, classOf[String]))
        val userId   = Obfuscators.userId.deobfuscate(userIdOb) match {
          case \/-(x) => x
          case -\/(e) => fail(s"Failed to deobfuscate user ID ${StringEscapeUtils.escapeJava(userIdOb.value)}: $e")
        }
        val user = User(userId, username)
        SessionToken(Some(user))
      }
    }

  private[this] val parseAndVerifyJws: String => Try[SessionToken] =
    config.jwtSecretPrevious match {
      case None =>
        _parseAndVerifyJws(_, jwtMainParser)

      case Some(altKey) =>
        val altParser = Jwts.parser().setSigningKey(Keys.hmacShaKeyFor(altKey.bytes))
        j => {
          val t1 = _parseAndVerifyJws(j, jwtMainParser)
          if (t1.isSuccess)
            t1
          else {
            val t2 = _parseAndVerifyJws(j, altParser)
            if (t2.isSuccess) t2 else t1
          }
        }
    }

  override def sessionRestore(cookies: Cookie.LookupFn): F[Option[SessionToken]] =
    cookies(cookieName) match {
      case Some(jws) =>
        F.point {
          parseAndVerifyJws(jws) match {
            case Success(t) => Some(t)
            case Failure(t) =>
              logger.warn("Failed to parse/verify JWT: " + StringEscapeUtils.escapeJava(jws), t)
              None
          }
        }

      case None => fNoToken
    }

}
