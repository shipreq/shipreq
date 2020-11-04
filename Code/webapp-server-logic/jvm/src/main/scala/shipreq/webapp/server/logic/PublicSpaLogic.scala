package shipreq.webapp.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.sql.Connection
import java.time.{Duration, Instant}
import scalaz.std.option.optionInstance
import scalaz.syntax.monad._
import scalaz.syntax.std.option._
import scalaz.{Catchable, Monad, ~>}
import shipreq.base.util._
import shipreq.base.util.log.{HasLogger, WebappLogFields}
import shipreq.taskman.api.{Task, TaskId, TaskmanApi}
import shipreq.webapp.base.config.Urls
import shipreq.webapp.base.data._
import shipreq.webapp.base.validation.UserValidators
import shipreq.webapp.client.public.PublicSpaProtocols
import shipreq.webapp.server.ServerLogicConfig
import shipreq.webapp.server.logic.Implicits._
import shipreq.webapp.server.logic.WebappTaskmanConverters._

trait PublicSpaLogic[F[_]] {

  val ajaxLandingPage   : PublicSpaProtocols.LandingPage   .ajax.ServerSideFn [F]
  val ajaxRegister1     : PublicSpaProtocols.Register1     .ajax.ServerSideFn [F]
  val ajaxResetPassword1: PublicSpaProtocols.ResetPassword1.ajax.ServerSideFn [F]
  val ajaxResetPassword2: PublicSpaProtocols.ResetPassword2.ajax.ServerSideFn [F]

  val ajaxRegister2: Security.SessionToken[Any] => PublicSpaProtocols.Register2.ajax.ServerSideFnO[F, Option[Security.SessionToken[Unit]]]

  /** Ignores publicRegistration setting.
    * Lacks security protection.
    */
  def apiRegister1(emailAddr: String): F[ErrorMsg \/ TaskId]
}

object PublicSpaLogic extends HasLogger {

  private[this] val rightUnit = \/-(())

  def isExpired_?(startTime: Instant, timeToLive: Duration, now: Instant): Boolean =
    startTime plus timeToLive isBefore now

  def tokenStatus[F[_]](ttl: Duration)
                       (implicit F: Monad[F], svr: Server.Time[F]): Option[Instant] => F[VerificationToken.Status] = {
    val invalid: F[VerificationToken.Status] =
      F pure VerificationToken.Status.Invalid

    val check: Instant => F[VerificationToken.Status] = i =>
      for (now <- svr.now) yield
        if (isExpired_?(i, ttl, now))
          VerificationToken.Status.Expired
        else
          VerificationToken.Status.Valid

    _.fold(invalid)(check)
  }

  def tokenStatusFn[F[_] : Monad : Server.Time](issueDate: VerificationToken => F[Option[Instant]],
                                                ttl: Duration): VerificationToken => F[VerificationToken.Status] = {
    val f = tokenStatus[F](ttl)
    issueDate(_).flatMap(f)
  }

  def registrationTokenStatusFn[D[_], F[_] : Monad : Server.Time](db    : DB.ForPublicSpa[D],
                                                                  runDB : D ~> F,
                                                                  config: ServerLogicConfig.Security): VerificationToken => F[VerificationToken.Status] =
    tokenStatusFn(t => runDB(db.getUserRegistrationTokenIssueDate(t)), config.registrationTokenLifespan)

  def passwordResetTokenStatusFn[D[_], F[_] : Monad : Server.Time](db    : DB.ForPublicSpa[D],
                                                                   runDB : D ~> F,
                                                                   config: ServerLogicConfig.Security): VerificationToken => F[VerificationToken.Status] =
    tokenStatusFn(t => runDB(db.getResetPasswordTokenIssueDate(t)), config.passwordResetTokenLifespan)

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  def apply[D[_], F[_]](implicit config  : ServerLogicConfig,
                                 db      : DB.ForPublicSpa[D],
                                 runDB   : D ~> F,
                                 common  : CommonProtocolLogic[F],
                                 metrics : MetricsLogic[F],
                                 security: Security.Algebra[F],
                                 svr     : Server.Algebra[F],
                                 taskman : TaskmanApi[F],
                                 D       : Monad[D],
                                 F       : Monad[F],
                                 FC      : Catchable[F]): PublicSpaLogic[F] =
    new PublicSpaLogic[F] {

      override val ajaxLandingPage =
        _.untyped.validate.onValid { req =>
          for {
            ip <- svr.clientIP

            msg = Task.LandingPageHit(
              name       = req.name.value,
              email      = req.email.toTaskman,
              msg        = req.msg,
              newsletter = req.newsletter,
              ip         = ip.map(_.value)
            )

            _ <- taskman.submit(msg)

          } yield rightUnit
        }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      override def apiRegister1(emailAddr: String) = RegisterFns.register1(emailAddr)
      override val ajaxRegister1 = RegisterFns.register1
      override val ajaxRegister2 = RegisterFns.register2

      private[this] object RegisterFns {

        private val absUrlRegister2 = config.baseUrl / Urls.PublicSpaRoute.Register2.url

        private val getTokenStatus: VerificationToken => F[VerificationToken.Status] =
          registrationTokenStatusFn(db, runDB, config.security)

        def preRegistrationMsg(email: EmailAddr, u: DB.UserRegistration, now: Instant): D[(Task, Security.Result)] =
          u match {
            case _: DB.UserRegistration.Complete =>
              D pure onAlreadyRegistered(email)
            case r: DB.UserRegistration.Pending =>
              if (isExpired_?(r.tokenSentAt, config.security.registrationTokenLifespan, now))
                onTokenExpired(email, r.id)
              else
                D pure onTokenReusable(email, r.token)
          }

        private def onTokenReusable(email: EmailAddr, token: VerificationToken): (Task, Security.Result) =
          registrationRequestedTask(email, token)

        private def onTokenExpired(email: EmailAddr, id: UserId): D[(Task, Security.Result)] =
          db.updateUserRegistrationToken(id).map(registrationRequestedTask(email, _))

        private def onAlreadyRegistered(email: EmailAddr): (Task, Security.Result) =
          (Task.ReRegistrationAttempted(email.toTaskman), Security.Result.Failure)

        private def registrationRequestedTask(email: EmailAddr, token: VerificationToken): (Task, Security.Result) =
          (Task.RegistrationRequested(email.toTaskman, absUrlRegister2(token).absoluteUrl), Security.Result.Success)

        def register1(emailAddrStr: String): F[ErrorMsg \/ TaskId] = {
          def registerInDb(emailAddr: EmailAddr, now: Instant): D[(Task, Security.Result)] =
            db.getUserRegistration(emailAddr).flatMap {
              case None    => onNewUser(emailAddr)
              case Some(u) => preRegistrationMsg(emailAddr, u, now)
            }

          def onNewUser(email: EmailAddr): D[(Task, Security.Result)] =
            db.createUserPlaceholder(email).map(registrationRequestedTask(email, _))

          val main: F[ErrorMsg \/ (TaskId, Security.Result)] =
            UserValidators.emailAddr.named(emailAddrStr).onValid(emailAddr =>
              for {
                now           <- svr.now
                (msg, secRes) <- runDB(registerInDb(emailAddr, now))
                id            <- taskman.submit(msg)
              } yield \/-((id, secRes))
            )

          for {
            result <- main
            secRes = result.fold(_ => Security.Result.Failure, _._2)
            _      <- metrics.securityEvent(Security.Event.Register1, secRes)
          } yield result.map(_._1)
        }

        val register1: PublicSpaProtocols.Register1.ajax.ServerSideFn[F] =
          security.protectFn(
            config.publicRegistration match {
              case Allow => i => register1(i.value).map(_.void)
              case Deny  => _ =>
                for {
                  _ <- metrics.securityEvent(Security.Event.Register1, Security.Result.Failure)
                  _ <- F.point(logger.warn("Denying public registration."))
                } yield -\/(ErrorMsg("Registration is disabled."))
            }
          )

        val register2: Security.SessionToken[Any] => PublicSpaProtocols.Register2.ajax.ServerSideFnO[F, Option[Security.SessionToken[Unit]]] = session => {
          type T = Option[Security.SessionToken[Unit]]
          import PublicSpaProtocols.Register2.{Request, Result}
          val stack = MonadEE[F, ErrorMsg, Result]
          import stack._

          val body: Request => F[ErrorMsg \/ (Result, T)] =
            security.protectFn { unvalidatedReq =>

              unvalidatedReq.validate.onValid[F, (Result, T)] { req =>
                WebappLogFields.request.verificationToken.mdc(req.token.value).para {

                  val validateToken: Stack[Unit] =
                    getTokenStatus(req.token).mapToStack {
                      case VerificationToken.Status.Valid   => rightUnit
                      case VerificationToken.Status.Invalid => -\/(\/-(Result.TokenInvalid))
                      case VerificationToken.Status.Expired => -\/(\/-(Result.TokenExpired))
                    }

                  def register(ps: PasswordAndSalt): Stack[UserId] =
                    runDB(db.completeUserRegistration(req.token, req.personName, req.username, ps, req.newsletter)).mapToStack {
                      case DB.UserRegistrationResult.Success(i)    => \/-(i)
                      case DB.UserRegistrationResult.TokenNotFound => -\/(\/-(Result.TokenInvalid))
                      case DB.UserRegistrationResult.UsernameTaken => -\/(\/-(Result.UsernameTaken))
                    }

                  val login: Stack[Option[Security.SessionToken[Unit]]] =
                    common.attemptLoginUnprotected(-\/(req.username), req.password, session).mapToStack {
                      case (Allow, t) => \/-(t)
                      case (Deny, _) => -\/(-\/(ErrorMsg("Registration completed but login failed.")))
                    }

                  val main: Stack[Option[Security.SessionToken[Unit]]] =
                    for {
                      _  <- validateToken
                      ps <- security.hashPassword(req.password).toStack
                      id <- register(ps)
                      _  <- svr.fork(taskman.submit(Task.RegistrationCompleted(id.toTaskman))).toStack
                      t  <- login
                    } yield t

                  def logAndMap(i: StackLeft \/ Option[Security.SessionToken[Unit]]): F[(ErrorMsg \/ (Result, Option[Security.SessionToken[Unit]]), Security.Result)] =
                    F.point(i match {
                      case \/-(t) =>
                        logger.info(s"${req.username.with_@} completed user registration.")
                        (\/-((Result.Success, t)), Security.Result.Success)
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
              }
            }

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

        private val getTokenStatus: VerificationToken => F[VerificationToken.Status] =
          passwordResetTokenStatusFn(db, runDB, config.security)

        private val absUrlRegister2 = config.baseUrl / Urls.PublicSpaRoute.ResetPassword.url

        val resetPassword1: PublicSpaProtocols.ResetPassword1.ajax.ServerSideFn[F] =
          security.protectFn { user =>

            def resetInDb(now: Instant): F[(Option[Task], Security.Result)] = {
              import DB.PasswordResetState._
              db.withTransactionLevel(runDB, Connection.TRANSACTION_SERIALIZABLE)(
                db.getPasswordResetState(user).flatMap {

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

            def resetMsg(email: EmailAddr, token: VerificationToken): Task =
              Task.PasswordResetRequested(email.toTaskman, absUrlRegister2(token).absoluteUrl)

            for {
              now           <- svr.now
              (msg, secRes) <- resetInDb(now)
              _             <- metrics.securityEvent(Security.Event.ResetPassword1, secRes)
              _             <- taskman.submitBulk_(msg)
            } yield ()
          }

        val resetPassword2: PublicSpaProtocols.ResetPassword2.ajax.ServerSideFn[F] =
          security.protectFn { req =>
            WebappLogFields.request.verificationToken.mdc(req.token.value).para {
              UserValidators.password.named(req.newPassword.value).onValid { newPassword =>

                import PublicSpaProtocols.ResetPassword2.Result
                val stack = MonadEE[F, ErrorMsg, Result]
                import stack._

                val validateToken: Stack[Unit] =
                  getTokenStatus(req.token).mapToStack {
                    case VerificationToken.Status.Valid   => rightUnit
                    case VerificationToken.Status.Invalid => -\/(\/-(Result.TokenInvalid))
                    case VerificationToken.Status.Expired => -\/(\/-(Result.TokenExpired))
                  }

                val main: Stack[Result.Success.type] =
                  for {
                    _  <- validateToken
                    ps <- security.hashPassword(newPassword).toStack
                    u  <- runDB(db.updateUserPassword(req.token, ps)).toStack
                    id <- (u \/> (Result.TokenInvalid: Result)).toStack
                  } yield {
                    logger.info(s"Password reset for user #${id.value}.")
                    Result.Success
                  }

                def logAndMap(i: StackLeft \/ Result.Success.type): F[(ErrorMsg \/ Result, Security.Result)] =
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
            }
          }
      }

    }
}
