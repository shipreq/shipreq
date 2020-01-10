package shipreq.webapp.server.logic

import japgolly.microlibs.utils.ConciseIntSetFormat
import scalaz.syntax.monad._
import scalaz.{-\/, Catchable, Monad, \/}
import shipreq.base.util._
import shipreq.base.util.log.{HasLogger, WebappLogFields}
import shipreq.taskman.api.{Task, TaskmanApi, UserId => TaskmanUserId}
import shipreq.webapp.base.protocol.CommonProtocols
import shipreq.webapp.base.user._

trait CommonProtocolLogic[F[_]] {

  /** "Unprotected" means that this isn't wrapped in `security.protect` */
  def attemptLoginUnprotected(id      : Username \/ EmailAddr,
                              password: PlainTextPassword,
                              session : Security.SessionToken[Any]): F[CommonProtocolLogic.LoginResult]

  val ajaxLogin: Security.SessionToken[Any] => CommonProtocols.Login.ajax.ServerSideFnO[F, Option[Security.SessionToken[Unit]]]

  val ajaxSubmitFeedback: Security.SessionToken[Any] => CommonProtocols.SubmitFeedback.ajax.ServerSideFnO[F, Permission]
}

object CommonProtocolLogic extends HasLogger {

  type LoginResult = (Permission, Option[Security.SessionToken[Unit]])

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  def apply[F[_]](implicit metrics : MetricsLogic[F],
                           security: Security.Algebra[F],
                           svr     : Server.Algebra[F],
                           taskman : TaskmanApi[F],
                           F       : Monad[F],
                           FC      : Catchable[F]): CommonProtocolLogic[F] =
    new CommonProtocolLogic[F] {

      private[this] val loginFail: F[LoginResult] = {val x = (Deny, None); F pure x}

      override def attemptLoginUnprotected(id      : Username \/ EmailAddr,
                                           password: PlainTextPassword,
                                           session : Security.SessionToken[Any]): F[LoginResult] =
        security.attemptLogin(id, password).flatMap {

          case Some(user) =>
            // Login succeeded
            val logToDB       = svr.clientIP.flatMap(ip => svr.fork(security.db.logLoginSuccess(user.id, ip)))
            val log           = F.point(logger.info(s"User #${user.id.value} logged in."))
            val updateMetrics = metrics.securityEvent(Security.Event.Login, Security.Result.Success)
            val newSession    = session.login(user).withoutExpiry
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

      override val ajaxSubmitFeedback =
        token => req => {

          val getUser: F[Option[User]] =
            token.authenticatedUser match {
              case Some(u) => F.pure(Some(u)) // ← source of truth
              case None    => security.db.getUserAndPassword(-\/(req.metadata.username)).map(_.map(_._1))
            }

          getUser.flatMap {
            case Some(user) =>

              val fMetadata: F[Map[String, String]] =
                for {
                  ipOption <- svr.clientIP
                } yield {
                  var metadata = Map.empty[String, String]
                  @inline def add(k: String, v: String): Unit = metadata = metadata.updated(k, v)
                  add("browser.url"      , req.metadata.url)
                  add("browser.userAgent", req.metadata.userAgent)
                  add("user.username"    , user.username.value)
                  for (ip <- ipOption)
                    add("request.ip", ip.value)
                  for (p <- req.metadata.project) {
                    add("project.id"          , Obfuscators.projectId.deobfuscate(p.id).fold("ERROR:" + _, _.value.toString))
                    add("project.futureEvents", ConciseIntSetFormat(p.futureEvents))
                  }
                  metadata
                }

              for {
                metadata <- fMetadata

                task = Task.UserFeedbackReceived(
                  userId   = TaskmanUserId(user.id.value),
                  feedback = req.input.feedback,
                  metadata = metadata)

                _ <- taskman.submit(task)

              } yield ((), Allow)

            case None =>
              F.pure(((), Deny))
          }
        }

    }
}
