package shipreq.webapp.server.logic.test

import cats.{Eval, Monad}
import io.circe._
import io.circe.syntax._
import java.time.Instant
import shipreq.base.test.JsonTestUtil._
import shipreq.webapp.base.data._
import shipreq.webapp.server.logic.algebra._
import shipreq.webapp.server.logic.config.ServerLogicConfig
import shipreq.webapp.server.logic.data._
import shipreq.webapp.server.logic.dispatch.Cookie

object MockSecurity {
  private[MockSecurity] object Codecs {
    import shipreq.webapp.server.logic.algebra.Security._

    implicit val decoderSessionId: Decoder[SessionId] =
      Decoder[String].map(SessionId.apply)

    implicit val encoderSessionId: Encoder[SessionId] =
      Encoder[String].contramap(_.value)

    implicit val decoderUserId: Decoder[UserId] =
      Decoder[Long].map(UserId.apply)

    implicit val encoderUserId: Encoder[UserId] =
      Encoder[Long].contramap(_.value)

    implicit val decoderUsername: Decoder[Username] =
      Decoder[String].map(Username.apply)

    implicit val encoderUsername: Encoder[Username] =
      Encoder[String].contramap(_.value)

    implicit val decoderUser: Decoder[User] =
      Decoder.forProduct2("id", "username")(User.apply)

    implicit val encoderUser: Encoder[User] =
      Encoder.forProduct2("id", "username")(a => (a.id, a.username))

    implicit val decoderSessionToken: Decoder[SessionToken[Instant]] =
      Decoder.forProduct3("sessionId", "authenticatedUser", "expiry")(SessionToken.apply[Instant])

    implicit val encoderSessionToken: Encoder[SessionToken[Instant]] =
      Encoder.forProduct3("sessionId", "authenticatedUser", "expiry")(a => (a.sessionId, a.authenticatedUser, a.expiry))
  }
}

final class MockSecurity(override val db: MockDb, now: Eval[Instant], cfg: ServerLogicConfig.Security) extends Security.Algebra[Eval] {
  import MockSecurity.Codecs._
  import shipreq.webapp.server.logic.algebra.Security._

  override val F = Monad[Eval]

  var protectedActions = 0
  override def protect[A](vulnerable: Eval[A]): Eval[A] =
    vulnerable.map { a =>
      protectedActions += 1
      a
    }

  override def attemptLogin(u: Username \/ EmailAddr, p: PlainTextPassword) = Eval.always[Option[User]] {
    db.getUser(u)
      .filter(e => e.ps ==* mkPasswordAndSalt(p, e.ps.salt))
      .map(_.toUser)
  }

  var prevSalt = 0
  override def hashPassword(p: PlainTextPassword) = Eval.always[PasswordAndSalt] {
    prevSalt += 1
    mkPasswordAndSalt(p, Salt(prevSalt.toString))
  }

  def mkPasswordAndSalt(p: PlainTextPassword, salt: Salt): PasswordAndSalt =
    PasswordAndSalt(PasswordHash(s"${salt.base64}:${p.value}"), salt)

  val cookieName = Cookie.Name("MockSecurity")

  def expiry() = now.value.plus(cfg.jwtLifespan)

  override def sessionPersist(token: SessionToken[Any]) = Eval.always[Cookie.Update] {
    val token2 = token.copy(expiry = expiry())
    val json   = token2.asJson.noSpaces
    val cookie = Cookie(cookieName, json, None, None, None)
    Cookie.Update.add(cookie)
  }

  override def sessionRestore(cookies: Cookie.LookupFn) = Eval.always[SessionRestoreResult[Instant]] {
    cookies(cookieName) match {
      case Some(cookieValue) =>
        SessionRestoreResult.Success(decodeOrThrow[SessionToken[Instant]](cookieValue))

      case None =>
        SessionRestoreResult.None
    }
  }
}
