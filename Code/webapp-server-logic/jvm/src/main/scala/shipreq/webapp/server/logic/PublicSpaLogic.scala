package shipreq.webapp.server.logic

import java.sql.Connection
import java.time.{Duration, Instant}
import scalaz.{-\/, Monad, \/, \/-, ~>}
import scalaz.std.option.optionInstance
import scalaz.syntax.monad._
import shipreq.base.util._
import shipreq.taskman.api.{Msg, TaskmanApi}
import shipreq.webapp.base.PublicUrls
import shipreq.webapp.base.data.SecurityToken
import shipreq.webapp.base.protocol.ErrorMsg
import shipreq.webapp.base.user._
import shipreq.webapp.client.public.PublicSpaProtocols._
import shipreq.webapp.server.ServerConfig
import WebappTaskmanConverters._
import Implicits._

trait PublicSpaLogic[F[_]] {
  val initData: F[InitData]
}

object PublicSpaLogic {

  private[this] val rightUnit = \/-(())

  def apply[D[_], F[_]](implicit config  : ServerConfig,
                                 db      : DB.ForPublicSpa[D],
                                 runDB   : D ~> F,
                                 security: Security.Algebra[F],
                                 svr     : Server.Algebra[F],
                                 taskman : TaskmanApi[F],
                                 D       : Monad[D],
                                 F       : Monad[F]): PublicSpaLogic[F] = {

    val absUrlResetPwd2 = config.baseUrl / PublicUrls.resetPassword2

    def isExpired_?(startTime: Instant, timeToLive: Duration, now: Instant): Boolean =
      startTime plus timeToLive isBefore now

    def tokenStatus(ttl: Duration): Option[Instant] => F[SecurityToken.Status] = {
      case Some(i) => svr.now.map(now =>
        if (isExpired_?(i, ttl, now))
          SecurityToken.Status.Expired
        else
          SecurityToken.Status.Valid
      )
      case None => F pure SecurityToken.Status.Invalid
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    val landingPageFn: F[LandingPage.Fn.Instance] =
      svr.createServerSideProc(LandingPage.Fn)(
        _.validate.onValid { req =>
          val msg = Msg.LandingPageHit(
            name       = req.name.value,
            email      = req.email.toTaskman,
            msg        = req.msg,
            newsletter = req.newsletter)
          taskman.submitMsg(msg).map(_ => rightUnit)
        }
      )

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    val loginFn: F[Login.Fn.Instance] =
      svr.createServerSideProc(Login.Fn)(
        security.protectFn(req =>
          security.attemptLogin(req.user, req.password).map(\/-(_))))

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object RegisterFns {

      private val absUrlRegister2 = config.baseUrl / PublicUrls.register2

      private def registrationProc[A, B](f: A => F[ErrorMsg \/ B]): A => F[ErrorMsg \/ B] =
        security.protectFn(
          config.allowRegister match {
            case Allow => f
            case Deny  => _ => F.pure(-\/(ErrorMsg("Registration is disabled.")))
          }
        )

      private def getTokenStatus(t: SecurityToken): F[SecurityToken.Status] =
        runDB(db.getUserRegistrationTokenIssueDate(t))
          .flatMap(tokenStatus(config.confirmationTokenLifespan))

      def preRegistrationMsg(email: EmailAddr, u: DB.UserRegistration, now: Instant): D[Msg] =
        u match {
          case _: DB.UserRegistration.Complete =>
            D pure onAlreadyRegistered(email)
          case r: DB.UserRegistration.Pending =>
            if (isExpired_?(r.tokenSentAt, config.confirmationTokenLifespan, now))
              onTokenExpired(email, r.id)
            else
              D pure onTokenReusable(email, r.token)
        }

      private def onTokenReusable(email: EmailAddr, token: SecurityToken): Msg =
        registrationRequestedTask(email, token)

      private def onTokenExpired(email: EmailAddr, id: UserId): D[Msg] =
        db.updateUserRegistrationToken(id).map(registrationRequestedTask(email, _))

      private def onAlreadyRegistered(email: EmailAddr): Msg =
        Msg.ReRegistrationAttempted(email.toTaskman)

      private def registrationRequestedTask(email: EmailAddr, token: SecurityToken): Msg =
        Msg.RegistrationRequested(email.toTaskman, absUrlRegister2(token).absoluteUrl)

      val registerFn1: F[Register.Fn1.Instance] = {

        def registerInDb(emailAddr: EmailAddr, now: Instant): D[Msg] =
          db.inDbTransaction(
            db.getUserRegistration(emailAddr).flatMap {
              case None    => onNewUser(emailAddr)
              case Some(u) => preRegistrationMsg(emailAddr, u, now)
            })

        def onNewUser(email: EmailAddr): D[Msg] =
          db.createUserPlaceholder(email).map(registrationRequestedTask(email, _))

        svr.createServerSideProc(Register.Fn1)(
          registrationProc(i =>
            UserValidators.emailAddr.unnamed(i.value).onValid(emailAddr =>
              for {
                now <- svr.now
                msg <- runDB(registerInDb(emailAddr, now))
                _   <- taskman.submitMsg(msg)
              } yield rightUnit
            )))
      }

      val registerFn2A: F[Register.Fn2A.Instance] =
        svr.createServerSideProc(Register.Fn2A)(
          registrationProc(
            getTokenStatus(_).map(\/-(_))))

      val registerFn2B: F[Register.Fn2B.Instance] =
        svr.createServerSideProc(Register.Fn2B)(
          registrationProc(
            _.validate.onValid { req =>

              import Register.Response
              val stack = MonadEE[F, ErrorMsg, Response]
              import stack._

              val validateToken: Stack[Unit] =
                getTokenStatus(req.token).mapToStack {
                  case SecurityToken.Status.Valid   => rightUnit
                  case SecurityToken.Status.Invalid => -\/(\/-(Response.TokenInvalid))
                  case SecurityToken.Status.Expired => -\/(\/-(Response.TokenExpired))
                }

              def register(ps: PasswordAndSalt, ip: Option[IP]): Stack[UserId] =
                runDB(db.completeUserRegistration(req.token, req.personName, req.username, ps, req.newsletter, ip)).mapToStack {
                  case DB.UserRegistrationResult.Success(i)    => \/-(i)
                  case DB.UserRegistrationResult.TokenNotFound => -\/(\/-(Response.TokenInvalid))
                  case DB.UserRegistrationResult.UsernameTaken => -\/(\/-(Response.UsernameTaken))
                }

              val login: Stack[Unit] =
                security.attemptLogin(-\/(req.username), req.password).mapToStack {
                  case Allow => rightUnit
                  case Deny => -\/(-\/(ErrorMsg("Registration completed but login failed.")))
                }

              (for {
                _  <- validateToken
                ps <- security.hashPassword(req.password).toStack
                ip <- svr.clientIP.toStack
                id <- register(ps, ip)
                _  <- svr.fork(taskman.submitMsg(Msg.RegistrationCompleted(id.toTaskman))).toStack
                _  <- login
              } yield Response.Success)
                .unstackFailure(\/-(_))
            }
          ))
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object ResetPasswordFns {

      private def getTokenStatus(t: SecurityToken): F[SecurityToken.Status] =
        runDB(db.getResetPasswordTokenIssueDate(t))
          .flatMap(tokenStatus(config.passwordResetTokenLifespan))

      private val absUrlRegister2 = config.baseUrl / PublicUrls.register2

      val resetPasswordFn1: F[ResetPassword.Fn1.Instance] =
        svr.createServerSideProc(ResetPassword.Fn1)(
          security.protectFn { user =>

            def resetInDb(now: Instant): D[Option[Msg]] = {
              import DB.PasswordResetState._
              db.inDbTransaction(Connection.TRANSACTION_SERIALIZABLE,
                db.getPasswordResetState(user) flatMap {

                  // No token
                  case Some((email, t: NoToken)) =>
                    db.createResetPasswordToken(t.reg.id).map(token => Some(resetMsg(email, token)))

                  // Valid token available
                  case Some((email, t: TokenExists)) if !isExpired_?(t.tokenSentAt, config.passwordResetTokenLifespan, now) =>
                    db.updateResetPasswordTokenOnReissue(t.reg.id).map(_ => Some(resetMsg(email, t.token)))

                  // Token expired
                  case Some((email, t: TokenExists)) =>
                    db.createResetPasswordToken(t.reg.id).map(token => Some(resetMsg(email, token)))

                  // Account not activated yet
                  case Some((email, UserRegistrationPending(u))) =>
                    RegisterFns.preRegistrationMsg(email, u, now).map(Some(_))

                  // No associated account
                  case None => D pure None
                }
              )
            }

            def resetMsg(email: EmailAddr, token: SecurityToken): Msg =
              Msg.PasswordResetRequested(email.toTaskman, absUrlRegister2(token).absoluteUrl)

            for {
              now <- svr.now
              msg <- runDB(resetInDb(now))
              _   <- taskman.submitMsgs_(msg)
            } yield rightUnit
          })

      val resetPasswordFn2A: F[ResetPassword.Fn2A.Instance] =
        svr.createServerSideProc(ResetPassword.Fn2A)(
          security.protectFn(
            getTokenStatus(_).map(\/-(_))))

      val resetPasswordFn2B: F[ResetPassword.Fn2B.Instance] =
        svr.createServerSideProc(ResetPassword.Fn2B)(
          security.protectFn(req =>
            UserValidators.password.named(req.newPassword.value).onValid { newPassword =>

              import ResetPassword.Response
              val stack = MonadEE[F, ErrorMsg, Response]
              import stack._

              val validateToken: Stack[Unit] =
                getTokenStatus(req.token).mapToStack {
                  case SecurityToken.Status.Valid   => rightUnit
                  case SecurityToken.Status.Invalid => -\/(\/-(Response.TokenInvalid))
                  case SecurityToken.Status.Expired => -\/(\/-(Response.TokenExpired))
                }

              (for {
                _  <- validateToken
                ps <- security.hashPassword(newPassword).toStack
                _  <- runDB(db.updateUserPassword(req.token, ps)).toStack
              } yield Response.Success)
                .unstackFailure(\/-(_))
            }))
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    new PublicSpaLogic[F] {
      val initData: F[InitData] =
        for {
          a <- landingPageFn
          b <- RegisterFns.registerFn1
          c <- RegisterFns.registerFn2A
          d <- RegisterFns.registerFn2B
          e <- loginFn
          f <- ResetPasswordFns.resetPasswordFn1
          g <- ResetPasswordFns.resetPasswordFn2A
          h <- ResetPasswordFns.resetPasswordFn2B
        } yield InitData(a, config.allowRegister, b, c, d, e, f, g, h)
    }
  }
}
