package shipreq.webapp.server.interpreter

import cats.Monad
import cats.syntax.all._
import com.typesafe.scalalogging.StrictLogging
import io.jsonwebtoken.security.{Keys, SignatureException}
import io.jsonwebtoken.{Claims, ExpiredJwtException, JwtParser, Jwts}
import java.security.SecureRandom
import java.time.Instant
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import org.apache.commons.text.StringEscapeUtils
import scala.Predef.classOf
import scala.concurrent.blocking
import scala.util.{Failure, Success, Try}
import shipreq.base.ops.Trace
import shipreq.base.util.CatsExtra.ApplicativeDelay
import shipreq.base.util.log.WebappLogFields
import shipreq.base.util.{Allow, Permission}
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.Obfuscated
import shipreq.webapp.server.logic.algebra.Security.{SessionId, SessionRestoreResult, SessionToken}
import shipreq.webapp.server.logic.algebra.{DB, Security}
import shipreq.webapp.server.logic.config.ServerLogicConfig
import shipreq.webapp.server.logic.data.{PasswordAndSalt, PasswordHash, Salt}
import shipreq.webapp.server.logic.dispatch.Cookie
import shipreq.webapp.server.logic.util.Obfuscators

object SecurityInterpreter {
  val cookieName = Cookie.Name("jwt")
}

final class SecurityInterpreter[F[_]](implicit _F: Monad[F],
                                      config : ServerLogicConfig.Security,
                                      secDb  : DB.ForSecurity[F],
                                      trace  : Trace.Algebra[F]) extends Security.Algebra[F] with StrictLogging {
  import SecurityInterpreter._

  override val F = _F
  override val db = secDb

  private[this] val fNoToken                 = F.pure[SessionRestoreResult[Instant]](SessionRestoreResult.None)
  private[this] val passwordSecretKeyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512")
  private[this] val jwtMainKey               = Keys.hmacShaKeyFor(config.jwtSecret.bytes)
  private[this] val jwtMainParser            = Jwts.parserBuilder().setSigningKey(jwtMainKey).build()

  private[this] val delay: F[Unit] =
    config.attackFrustrationDelayMs match {
      case 0  => F.unit
      case ms =>
        val f = F.delay(blocking(Thread.sleep(ms)))
        trace.newSpan("Security delay")(_ => f)
    }

  override def protect[A](vulnerable: F[A]): F[A] =
    delay >> vulnerable

  override def hashPassword(p: PlainTextPassword): F[PasswordAndSalt] = F delay {
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

  private[this] final val claimSessionId = "sid"
  private[this] final val claimUserId    = "uid"

  override def sessionPersist(token: SessionToken[Any]): F[Cookie.Update] = F.delay {
    val jws: String = {
      val b = Jwts.builder()

      val now = System.currentTimeMillis()
      val exp = new java.util.Date(now + config.jwtLifespanMs)
      b.setExpiration(exp)

      b.claim(claimSessionId, token.sessionId.value)

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

  private def parseClaims(claims: Claims): Try[SessionToken[Instant]] =
    Try {
      def fail(errMsg: String): Nothing = {
        // Note: not doing any logging here for two reasons:
        // 1. This Try is unpacked in sessionRestore() and there is logging there on failure
        // 2. This may be called twice if config.jwtSecretPrevious is defined, in which case failure the first time
        //    shouldn't be logged as a problem if the second time succeeds.
        throw new RuntimeException(errMsg)
      }

      val sessionId = SessionId(claims.get(claimSessionId, classOf[String]))
      if (sessionId.value eq null)
        throw new RuntimeException("Session ID not specified")

      val authenticatedUser: Option[User] =
        claims.getSubject match {
          case null => None
          case subject =>
            val username = Username(subject)
            val userIdOb = Obfuscated[UserId](claims.get(claimUserId, classOf[String]))
            val userId   = Obfuscators.userId.deobfuscate(userIdOb) match {
              case \/-(x) => x
              case -\/(e) => fail(s"Failed to deobfuscate user ID ${StringEscapeUtils.escapeJava(userIdOb.value)}: $e")
            }
            Some(User(userId, username))
        }

      val expiration = claims.getExpiration.toInstant

      SessionToken(sessionId, authenticatedUser, expiration)
    }

  private def parseAndVerifyJws(jws: String, parser: JwtParser): Try[SessionRestoreResult.NonEmpty[Instant]] =
    Try(parser.parseClaimsJws(jws).getBody)
      .flatMap(parseClaims)
      .map(SessionRestoreResult.Success.apply)
      .recoverWith {
        case e: ExpiredJwtException =>
          parseClaims(e.getClaims).map(SessionRestoreResult.Expired.apply)
      }

  private[this] val parseAndVerifyJws: String => Try[SessionRestoreResult[Instant]] =
    config.jwtSecretPrevious match {
      case None =>
        parseAndVerifyJws(_, jwtMainParser)

      case Some(altKey) =>
        val altParser = Jwts.parserBuilder().setSigningKey(Keys.hmacShaKeyFor(altKey.bytes)).build()
        j => {
          val t1 = parseAndVerifyJws(j, jwtMainParser)
          t1.recoverWith {
            case _: SignatureException =>
              val t2 = parseAndVerifyJws(j, altParser)
              if (t2.isSuccess) t2 else t1
          }
        }
    }

  override def sessionRestore(cookies: Cookie.LookupFn): F[SessionRestoreResult[Instant]] =
    cookies(cookieName) match {

      case Some(jws) =>
        F.delay {
          parseAndVerifyJws(jws) match {
            case Success(r) => r
            case Failure(t) =>
              logger.warn("Failed to parse/verify JWT", t, WebappLogFields.jwt.invalid(jws))
              SessionRestoreResult.None
          }
        }

      case None =>
        fNoToken
    }

  override def allowProjectAccess(requester: User, projectId: ProjectId, projectOwner: UserId): Permission =
    Allow.when(requester.id ==* projectOwner) | config.projectAccessHacks(requester, projectId)
}
