package shipreq.webapp.server.logic

import scalaz.syntax.monad._
import scalaz.{Catchable, Monad, \/}
import shipreq.base.util._
import shipreq.base.util.log.{HasLogger, WebappLogFields}
import shipreq.webapp.base.protocol.CommonProtocols
import shipreq.webapp.base.user._

trait CommonProtocolLogic[F[_]] {

  /** "Unprotected" means that this isn't wrapped in `security.protect` */
  def attemptLoginUnprotected(id      : Username \/ EmailAddr,
                              password: PlainTextPassword,
                              session : Security.SessionToken): F[CommonProtocolLogic.LoginResult]

  val ajaxLogin: Security.SessionToken => CommonProtocols.Login.ajax.ServerSideFnO[F, Option[Security.SessionToken]]
}

object CommonProtocolLogic extends HasLogger {

  type LoginResult = (Permission, Option[Security.SessionToken])

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  def apply[F[_]](implicit metrics : MetricsLogic[F],
                           security: Security.Algebra[F],
                           svr     : Server.Algebra[F],
                           F       : Monad[F],
                           FC      : Catchable[F]): CommonProtocolLogic[F] =
    new CommonProtocolLogic[F] {

      private[this] val loginFail: F[LoginResult] = {val x = (Deny, None); F pure x}

      override def attemptLoginUnprotected(id      : Username \/ EmailAddr,
                                           password: PlainTextPassword,
                                           session : Security.SessionToken): F[LoginResult] =
        security.attemptLogin(id, password).flatMap {

          case Some(user) =>
            // Login succeeded
            val logToDB       = svr.clientIP.flatMap(ip => svr.fork(security.db.logLoginSuccess(user.id, ip)))
            val log           = F.point(logger.info(s"User #${user.id.value} logged in."))
            val updateMetrics = metrics.securityEvent(Security.Event.Login, Security.Result.Success)
            val newSession    = session.login(user)
            val result        = (Allow, Some(newSession)): LoginResult
            val main          = log >> logToDB >> updateMetrics >| result

            val mdc = WebappLogFields.jwt.userId.mdc(user.id.value) ++
                      WebappLogFields.jwt.username.mdc(user.username.value) ++
                      WebappLogFields.jwt.sessionId.mdc(newSession.sessionId.value)
            mdc.para(main)

          case None =>
            // User not found, or password didn't match
            // The inability to distinguish is a security feature
            val log = F.point(logger.warn(s"Login for ${id.fold(_.with_@, _.value)} with password hash ${password.hashStr} failed."))
            val updateMetrics = metrics.securityEvent(Security.Event.Login, Security.Result.Failure)
            log >> updateMetrics >> loginFail
        }

      override val ajaxLogin =
        token =>
          security.protectFn(req =>
            attemptLoginUnprotected(req.user, req.password, token))
    }
}
