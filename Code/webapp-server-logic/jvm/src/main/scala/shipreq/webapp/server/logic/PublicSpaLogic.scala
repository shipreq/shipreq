package shipreq.webapp.server.logic

import java.time.{Duration, Instant}
import scalaz.{-\/, Monad, \/, \/-, ~>}
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

      private def getUserRegistrationTokenStatus(t: SecurityToken): F[SecurityToken.Status] =
        runDB(db.getUserRegistrationTokenIssueDate(t))
          .flatMap(tokenStatus(config.confirmationTokenLifespan))

      val registerFn1: F[Register.Fn1.Instance] = {
        import DB.UserRegistration

        def registerInDb(emailAddr: EmailAddr, now: Instant): D[Msg] =
          db.inDbTransaction(
            db.getUserRegistration(emailAddr).flatMap {
              case None    => onNewUser(emailAddr)
              case Some(u) => preRegistrationMsg(emailAddr, u, now)
            })

        def onNewUser(email: EmailAddr): D[Msg] =
          db.createUserPlaceholder(email).map(registrationRequestedTask(email, _))

        def preRegistrationMsg(email: EmailAddr, u: UserRegistration, now: Instant): D[Msg] =
          u match {
            case _: UserRegistration.Complete =>
              D pure onAlreadyRegistered(email)
            case r: UserRegistration.Pending =>
              if (isExpired_?(r.tokenSentAt, config.confirmationTokenLifespan, now))
                onTokenExpired(email, r.id)
              else
                D pure onTokenReusable(email, r.token)
          }

        def onTokenReusable(email: EmailAddr, token: SecurityToken): Msg =
          registrationRequestedTask(email, token)

        def onTokenExpired(email: EmailAddr, id: UserId): D[Msg] =
          db.updateUserRegistrationToken(id).map(registrationRequestedTask(email, _))

        def onAlreadyRegistered(email: EmailAddr): Msg =
          Msg.ReRegistrationAttempted(email.toTaskman)

        def registrationRequestedTask(email: EmailAddr, token: SecurityToken): Msg =
          Msg.RegistrationRequested(email.toTaskman, absUrlRegister2(token).absoluteUrl)

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
            getUserRegistrationTokenStatus(_).map(\/-(_))))

      val registerFn2B: F[Register.Fn2B.Instance] =
        svr.createServerSideProc(Register.Fn2B)(
          registrationProc(
            _.validate.onValid { req =>

              import Register.Response
              val stack = MonadEE[F, ErrorMsg, Response]
              import stack._

              val validateToken: Stack[Unit] =
                getUserRegistrationTokenStatus(req.token).mapToStack {
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
    val resetPasswordFn1: F[ResetPassword.Fn1.Instance] =
      svr.createServerSideProc(ResetPassword.Fn1)(i =>
//-    def onSubmit(): JsCmd = {
//-      securityProvider().enforceHumanSpeed()
//-      perform(form validate vars)
//-    }
//-
//-    form.csssel(vars, vars = _) & ":submit" #> ajaxSubmitOnClick(() => onSubmit())
//-  }
//-
//-  def perform(v: Invalidity \/ EmailAddr): JsCmd =
//-    handleCompositeInvalidity(v) { email =>
//-
//-      val dbPlan = resetLogic(email).withTransactionLevel(Connection.TRANSACTION_SERIALIZABLE)
//-      val plan = db().io.trans(dbPlan).flatMap {
//-        case Some(msg) => taskman().submitMsg(msg).map(_ => ())
//-        case None => IO.ioUnit
//-      }
//-      plan.unsafePerformIO()
//-
//-      // Respond the same in all cases (for security purposes)
//-      jsEmailSent
//-    }
//-
//-  def resetLogic(email: EmailAddr): ConnectionIO[Option[Msg]] =
//-    DbLogic.user.findRegAndResetPwInfo(email) flatMap {
//-
//-      // No associated account
//-      case None =>
//-        Free pure None
//-
//-      // Account not activated yet
//-      case Some((u@UserRegistrationInfo(_, _, _, None), _)) =>
//-        Register1.preRegistrationMsg(email, u).map(Some(_))
//-
//-      // Valid token available
//-      case Some((UserRegistrationInfo(id, _, _, Some(_)), ResetPasswordInfo(Some(token), Some(issued)))) if !isTokenExpired(issued) =>
//-        reuseToken(id).map(_ => Some(passwordResetMsg(email, token)))
//-
//-      // No token or token expired
//-      case Some((UserRegistrationInfo(id, _, _, Some(_)), _)) =>
//-        issueNewToken(id).map(token => Some(passwordResetMsg(email, token)))
//-    }
//-
//-  def passwordResetMsg(email: EmailAddr, token: String): Msg =
//-    Msg.PasswordResetRequested(email, AppSiteMap.ResetPassword2.absoluteUrl(token))
//-
//-  val jsEmailSent: JsCmd =
//-    JqExpr("#resetpw1Form,#resetpwTokenSent") ~> JqToggle
//-
//-  private def issueNewToken(id: UserId): ConnectionIO[String] =
//-    DbLogic.user.performInstallNewResetPasswordToken(id, () => randomConfirmationToken())
//-
//-  private def reuseToken(id: UserId): ConnectionIO[Unit] =
//-    DbLogic.user.performReuseResetPasswordToken(id)
        ???
      )

    val resetPasswordFn2: F[ResetPassword.Fn2.Instance] =
      svr.createServerSideProc(ResetPassword.Fn2)(i =>
//-  def isTokenExpired(dateIssued: Instant): Boolean =
//-    Misc.isExpired_?(dateIssued, DI.serverConfig.passwordResetTokenLifespan)
//-  def validateToken_!(): Unit =
//-    db().io.trans(DbLogic.user.findResetPasswordTokenIssuedDate(token)).unsafePerformIO() match {
//-      case None =>
//-        S.error("The token associated with that URL is invalid.")
//-        redirectTo(AppSiteMap.Login)
//-
//-      case Some(issued) if isTokenExpired(issued) =>
//-        S.error("Your password-reset token has expired. Please re-enter your email address to get a new token.")
//-        redirectTo(AppSiteMap.ResetPassword1)
//-
//-      case _ => // valid
//-    }
//-
//-  def render = {
//-    validateToken_!()
//-    form.csssel(vars, vars = _) & ":submit" #> ajaxSubmitOnClick(() => onSubmit())
//-  }
//-
//-  def onSubmit(): JsCmd =
//-    try
//-      handleCompositeInvalidity(form validate vars)(resetPassword)
//-    finally
//-      vars = FormVar.emptyPasswordPair // Let's not keep the plaintext passwords around
//-
//-  def resetPassword(password: String): JsCmd = {
//-    val ps = PasswordAndSalt.createWithRandomSalt(password)
//-    db().io.trans(DbLogic.user.performPasswordReset(ps, token)).unsafePerformIO()
//-    jsClearError & JqExpr("#resetpw2Form,#resetpwComplete") ~> JqToggle
        ???
      )

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    new PublicSpaLogic[F] {
      val initData: F[InitData] =
        for {
          a <- landingPageFn
          b <- RegisterFns.registerFn1
          c <- RegisterFns.registerFn2A
          d <- RegisterFns.registerFn2B
          e <- loginFn
          f <- resetPasswordFn1
          g <- resetPasswordFn2
        } yield InitData(a, config.allowRegister, b, c, d, e, f, g)
    }
  }
}
