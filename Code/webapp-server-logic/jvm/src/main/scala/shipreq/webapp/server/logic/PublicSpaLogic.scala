package shipreq.webapp.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.sql.Connection
import java.time.{Duration, Instant}
import scalaz.{-\/, Monad, \/, \/-, ~>}
import scalaz.std.option.optionInstance
import scalaz.syntax.monad._
import scalaz.syntax.std.option._
import shipreq.base.util._
import shipreq.base.util.log.{HasLogger, MDC}
import shipreq.taskman.api.{Msg, MsgId, TaskmanApi}
import shipreq.webapp.base.Urls
import shipreq.webapp.base.data.SecurityToken
import shipreq.webapp.base.user._
import shipreq.webapp.client.public.PublicSpaProtocols
import shipreq.webapp.server.ServerLogicConfig
import WebappTaskmanConverters._
import Implicits._

trait PublicSpaLogic[F[_]] {

  val ajaxLandingPage   : PublicSpaProtocols.landingPage   .ServerSideFn [F]
  val ajaxLogin         : PublicSpaProtocols.login         .ServerSideFnO[F, Option[Security.SessionToken]]
  val ajaxRegister1     : PublicSpaProtocols.register1     .ServerSideFn [F]
  val ajaxRegister2     : PublicSpaProtocols.register2     .ServerSideFnO[F, Option[Security.SessionToken]]
  val ajaxResetPassword1: PublicSpaProtocols.resetPassword1.ServerSideFn [F]
  val ajaxResetPassword2: PublicSpaProtocols.resetPassword2.ServerSideFn [F]

  /** Ignores publicRegistration setting.
    * Lacks security protection.
    */
  def apiRegister1(emailAddr: String): F[ErrorMsg \/ MsgId]
}

object PublicSpaLogic extends HasLogger {

  private[this] val rightUnit = \/-(())

  private[this] final val MdcSecurityToken = "security_token"

  def isExpired_?(startTime: Instant, timeToLive: Duration, now: Instant): Boolean =
    startTime plus timeToLive isBefore now

  def tokenStatus[F[_]](ttl: Duration)
                       (implicit F: Monad[F], svr: Server.Time[F]): Option[Instant] => F[SecurityToken.Status] = {
    val invalid: F[SecurityToken.Status] =
      F pure SecurityToken.Status.Invalid

    val check: Instant => F[SecurityToken.Status] = i =>
      for (now <- svr.now) yield
        if (isExpired_?(i, ttl, now))
          SecurityToken.Status.Expired
        else
          SecurityToken.Status.Valid

    _.fold(invalid)(check)
  }

  def tokenStatusFn[F[_] : Monad : Server.Time](issueDate: SecurityToken => F[Option[Instant]],
                                                ttl: Duration): SecurityToken => F[SecurityToken.Status] = {
    val f = tokenStatus[F](ttl)
    issueDate(_).flatMap(f)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  def apply[D[_], F[_]](implicit config  : ServerLogicConfig,
                                 db      : DB.ForPublicSpa[D],
                                 runDB   : D ~> F,
                                 metrics : MetricsLogic[F],
                                 security: Security.Algebra[F],
                                 svr     : Server.Algebra[F],
                                 taskman : TaskmanApi[F],
                                 D       : Monad[D],
                                 F       : Monad[F]): PublicSpaLogic[F] =
    new PublicSpaLogic[F] {

      override val ajaxLandingPage =
        _.untyped.validate.onValid { req =>
          val msg = Msg.LandingPageHit(
            name       = req.name.value,
            email      = req.email.toTaskman,
            msg        = req.msg,
            newsletter = req.newsletter)
          taskman.submitMsg(msg).map(_ => rightUnit)
        }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      private type LoginResult = (Permission, Option[Security.SessionToken])
      private[this] val loginFail: F[LoginResult] = {val x = (Deny, None); F pure x}

      private def attemptLogin(id: Username \/ EmailAddr, password: PlainTextPassword): F[LoginResult] =
        security.attemptLogin(id, password).flatMap {

          case Some(user) =>
            // Login succeeded
            val logToDB       = svr.clientIP.flatMap(ip => svr.fork(security.db.logLoginSuccess(user.id, ip)))
            val log           = F.point(logger.info(s"User #${user.id.value} logged in."))
            val updateMetrics = metrics.securityEvent(Security.Event.Login, Security.Result.Success)
            val token         = Security.SessionToken(Some(user))
            log >> logToDB >> updateMetrics >| (Allow, Some(token))

          case None =>
            // User not found, or password didn't match
            // The inability to distinguish is a security feature
            val log = F.point(logger.warn(s"Login for ${id.fold(_.with_@, _.value)} with password hash ${password.hashStr} failed."))
            val updateMetrics = metrics.securityEvent(Security.Event.Login, Security.Result.Failure)
            log >> updateMetrics >> loginFail
        }

      override val ajaxLogin =
        security.protectFn(req =>
          attemptLogin(req.user, req.password))

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      override def apiRegister1(emailAddr: String) = RegisterFns.register1(emailAddr)
      override val ajaxRegister1 = RegisterFns.register1
      override val ajaxRegister2 = RegisterFns.register2

      private[this] object RegisterFns {

        private val absUrlRegister2 = config.baseUrl / Urls.PublicSpaRoute.Register2.url

        private def registrationProc[A, B](e: Security.Event, f: A => F[ErrorMsg \/ B]): A => F[ErrorMsg \/ B] =
          security.protectFn(
            config.publicRegistration match {
              case Allow => f
              case Deny  => _ =>
                for {
                  _ <- metrics.securityEvent(e, Security.Result.Failure)
                  _ <- F.point(logger.warn("Denying public registration."))
                } yield -\/(ErrorMsg("Registration is disabled."))
            }
          )

        private val getTokenStatus: SecurityToken => F[SecurityToken.Status] =
          tokenStatusFn(t => runDB(db.getUserRegistrationTokenIssueDate(t)), config.security.registrationTokenLifespan)

        def preRegistrationMsg(email: EmailAddr, u: DB.UserRegistration, now: Instant): D[(Msg, Security.Result)] =
          u match {
            case _: DB.UserRegistration.Complete =>
              D pure onAlreadyRegistered(email)
            case r: DB.UserRegistration.Pending =>
              if (isExpired_?(r.tokenSentAt, config.security.registrationTokenLifespan, now))
                onTokenExpired(email, r.id)
              else
                D pure onTokenReusable(email, r.token)
          }

        private def onTokenReusable(email: EmailAddr, token: SecurityToken): (Msg, Security.Result) =
          registrationRequestedTask(email, token)

        private def onTokenExpired(email: EmailAddr, id: UserId): D[(Msg, Security.Result)] =
          db.updateUserRegistrationToken(id).map(registrationRequestedTask(email, _))

        private def onAlreadyRegistered(email: EmailAddr): (Msg, Security.Result) =
          (Msg.ReRegistrationAttempted(email.toTaskman), Security.Result.Failure)

        private def registrationRequestedTask(email: EmailAddr, token: SecurityToken): (Msg, Security.Result) =
          (Msg.RegistrationRequested(email.toTaskman, absUrlRegister2(token).absoluteUrl), Security.Result.Success)

        def register1(emailAddrStr: String): F[ErrorMsg \/ MsgId] = {
          def registerInDb(emailAddr: EmailAddr, now: Instant): D[(Msg, Security.Result)] =
            db.inDbTransaction(
              db.getUserRegistration(emailAddr).flatMap {
                case None    => onNewUser(emailAddr)
                case Some(u) => preRegistrationMsg(emailAddr, u, now)
              })

          def onNewUser(email: EmailAddr): D[(Msg, Security.Result)] =
            db.createUserPlaceholder(email).map(registrationRequestedTask(email, _))

          val main: F[ErrorMsg \/ (MsgId, Security.Result)] =
            UserValidators.emailAddr.named(emailAddrStr).onValid(emailAddr =>
              for {
                now           <- svr.now
                (msg, secRes) <- runDB(registerInDb(emailAddr, now))
                id            <- taskman.submitMsg(msg)
              } yield \/-((id, secRes))
            )

          for {
            result ← main
            secRes = result.fold(_ => Security.Result.Failure, _._2)
            _      ← metrics.securityEvent(Security.Event.Register1, secRes)
          } yield result.map(_._1)
        }

        val register1: PublicSpaProtocols.register1.ServerSideFn[F] =
          registrationProc(Security.Event.Register1, i =>
            register1(i.value).map(_.void))

        val register2: PublicSpaProtocols.register2.ServerSideFnO[F, Option[Security.SessionToken]] = {
          type T = Option[Security.SessionToken]
          import PublicSpaProtocols.Register.{Request, Response}
          val stack = MonadEE[F, ErrorMsg, Response]
          import stack._

          val body: Request => F[ErrorMsg \/ (Response, T)] =
            registrationProc[Request, (Response, T)](Security.Event.Register2, unvalidatedReq => {

              unvalidatedReq.validate.onValid[F, (Response, T)](req =>
                MDC(MdcSecurityToken, req.token.value) {

                  val validateToken: Stack[Unit] =
                    getTokenStatus(req.token).mapToStack {
                      case SecurityToken.Status.Valid   => rightUnit
                      case SecurityToken.Status.Invalid => -\/(\/-(Response.TokenInvalid))
                      case SecurityToken.Status.Expired => -\/(\/-(Response.TokenExpired))
                    }

                  def register(ps: PasswordAndSalt): Stack[UserId] =
                    runDB(db.completeUserRegistration(req.token, req.personName, req.username, ps, req.newsletter)).mapToStack {
                      case DB.UserRegistrationResult.Success(i)    => \/-(i)
                      case DB.UserRegistrationResult.TokenNotFound => -\/(\/-(Response.TokenInvalid))
                      case DB.UserRegistrationResult.UsernameTaken => -\/(\/-(Response.UsernameTaken))
                    }

                  val login: Stack[Option[Security.SessionToken]] =
                    attemptLogin(-\/(req.username), req.password).mapToStack {
                      case (Allow, t) => \/-(t)
                      case (Deny, _) => -\/(-\/(ErrorMsg("Registration completed but login failed.")))
                    }

                  val main: Stack[Option[Security.SessionToken]] =
                    for {
                      _  <- validateToken
                      ps <- security.hashPassword(req.password).toStack
                      id <- register(ps)
                      _  <- svr.fork(taskman.submitMsg(Msg.RegistrationCompleted(id.toTaskman))).toStack
                      t  <- login
                    } yield t

                  def logAndMap(i: StackLeft \/ Option[Security.SessionToken]): F[(ErrorMsg \/ (Response, Option[Security.SessionToken]), Security.Result)] =
                    F.point(i match {
                      case \/-(t) =>
                        logger.info(s"${req.username.with_@} completed user registration.")
                        (\/-((Response.Success, t)), Security.Result.Success)
                      case -\/(\/-(f)) =>
                        logger.warn(s"${req.username.with_@} failed to complete user registration: $f")
                        (\/-((f, None)), Security.Result.Failure)
                      case -\/(r@ -\/(e)) =>
                        logger.warn(s"${req.username.with_@} failed to complete user registration: ${e.value}")
                        (r, Security.Result.Failure)
                    })

                  for {
                    stackRes      <- main.underlying
                    (res, secRes) <- logAndMap(stackRes)
                    _             <- metrics.securityEvent(Security.Event.Register2, secRes)
                  } yield res
                }
              )
            })

          body(_).map {
            case \/-((r, t)) => (\/-(r), t)
            case e@ -\/(_)   => (e, None)
          }
        }
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      override val ajaxResetPassword1 = ResetPasswordFns.resetPassword1
      override val ajaxResetPassword2 = ResetPasswordFns.resetPassword2

      private[this] object ResetPasswordFns {

        private val getTokenStatus: SecurityToken => F[SecurityToken.Status] =
          tokenStatusFn(t => runDB(db.getResetPasswordTokenIssueDate(t)), config.security.passwordResetTokenLifespan)

        private val absUrlRegister2 = config.baseUrl / Urls.PublicSpaRoute.ResetPassword.url

        val resetPassword1: PublicSpaProtocols.resetPassword1.ServerSideFn[F] =
          security.protectFn { user =>

            def resetInDb(now: Instant): D[(Option[Msg], Security.Result)] = {
              import DB.PasswordResetState._
              db.inDbTransaction(Connection.TRANSACTION_SERIALIZABLE,
                db.getPasswordResetState(user) flatMap {

                  // No token
                  case Some((email, t: NoToken)) =>
                    db.createResetPasswordToken(t.reg.id).map(token =>
                      (Some(resetMsg(email, token)), Security.Result.Failure))

                  // Valid token available
                  case Some((email, t: TokenExists)) if !isExpired_?(t.tokenSentAt, config.security.passwordResetTokenLifespan, now) =>
                    db.updateResetPasswordTokenOnReissue(t.reg.id).map(_ =>
                      (Some(resetMsg(email, t.token)), Security.Result.Success))

                  // Token expired
                  case Some((email, t: TokenExists)) =>
                    db.createResetPasswordToken(t.reg.id).map(token =>
                      (Some(resetMsg(email, token)), Security.Result.Failure))

                  // Account not activated yet
                  case Some((email, UserRegistrationPending(u))) =>
                    RegisterFns.preRegistrationMsg(email, u, now).map(_.map1(Some(_)))

                  // No associated account
                  case None => D.pure((None, Security.Result.Failure))
                }
              )
            }

            def resetMsg(email: EmailAddr, token: SecurityToken): Msg =
              Msg.PasswordResetRequested(email.toTaskman, absUrlRegister2(token).absoluteUrl)

            for {
              now           <- svr.now
              (msg, secRes) <- runDB(resetInDb(now))
              _             <- metrics.securityEvent(Security.Event.ResetPassword1, secRes)
              _             <- taskman.submitMsgs_(msg)
            } yield ()
          }

        val resetPassword2: PublicSpaProtocols.resetPassword2.ServerSideFn[F] =
          security.protectFn(req =>
            MDC(MdcSecurityToken, req.token.value)(
              UserValidators.password.named(req.newPassword.value).onValid { newPassword =>

                import PublicSpaProtocols.ResetPassword.Response
                val stack = MonadEE[F, ErrorMsg, Response]
                import stack._

                val validateToken: Stack[Unit] =
                  getTokenStatus(req.token).mapToStack {
                    case SecurityToken.Status.Valid   => rightUnit
                    case SecurityToken.Status.Invalid => -\/(\/-(Response.TokenInvalid))
                    case SecurityToken.Status.Expired => -\/(\/-(Response.TokenExpired))
                  }

                val main: Stack[Response.Success.type] =
                  for {
                    _  <- validateToken
                    ps <- security.hashPassword(newPassword).toStack
                    u  <- runDB(db.updateUserPassword(req.token, ps)).toStack
                    id <- (u \/> (Response.TokenInvalid: Response)).toStack
                  } yield {
                    logger.info(s"Password reset for user #${id.value}.")
                    Response.Success
                  }

                def logAndMap(i: StackLeft \/ Response.Success.type): F[(ErrorMsg \/ Response, Security.Result)] =
                  F.point(i match {
                    case r@ \/-(_) =>
                      (r, Security.Result.Success)
                    case -\/(r@ \/-(f)) =>
                      logger.warn(s"Password reset failed: $f")
                      (r, Security.Result.Failure)
                    case -\/(r@ -\/(e)) =>
                      logger.warn(s"Password reset failed: ${e.value}")
                      (r, Security.Result.Failure)
                  })

                for {
                  stackRes      <- main.underlying
                  (res, secRes) <- logAndMap(stackRes)
                  _             <- metrics.securityEvent(Security.Event.ResetPassword2, secRes)
                } yield res
              }
            )
          )
      }

    }
}
