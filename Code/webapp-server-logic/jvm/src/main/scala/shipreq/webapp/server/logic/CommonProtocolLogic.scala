package shipreq.webapp.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.ConciseIntSetFormat
import scalaz.syntax.monad._
import scalaz.{-\/, Catchable, Monad, \/, \/-}
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

  val ajaxReportClientError: Security.SessionToken[Any] => CommonProtocols.ReportClientError.ajax.ServerSideFnO[F, Permission]

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
      import CommonProtocols.Metadata

      private[this] val loginFail: F[LoginResult] = {val x = (Deny, None); F pure x}

      private implicit def userIdToTaskman(userId: UserId): TaskmanUserId =
        TaskmanUserId(userId.value)

      private def resolveUser(token: Security.SessionToken[Any], username: Option[Username]): F[Unit \/ Option[User]] =
        token.authenticatedUser match {
          case Some(u) =>
            F.pure(\/-(Some(u))) // ← source of truth

          case None =>
            username match {
              case Some(u) =>
                security.db.getUserAndPassword(-\/(u)).map {
                  case Some((user, _)) => \/-(Some(user))
                  case None            => -\/(())
                }
              case None =>
                F.pure(\/-(None))
            }
        }

      private def prepareMetadata(metadata: Metadata.Client): F[Map[String, String]] =
        for {
          ipOption <- svr.clientIP
        } yield {
          var m = Map.empty[String, String]
          @inline def add(k: String, v: String): Unit = m = m.updated(k, v)
          add("browser.url"      , metadata.url)
          add("browser.userAgent", metadata.userAgent)
          for (ip <- ipOption)
            add("request.ip", ip.value)
          for (p <- metadata.project) {
            add("project.id"          , Obfuscators.projectId.deobfuscate(p.id).fold("ERROR:" + _, _.value.toString))
            add("project.futureEvents", ConciseIntSetFormat(p.futureEvents))
          }
          m
        }

      private final val exceptionPrefix  = "error."
      private final val exceptionNameKey = exceptionPrefix + "name"
      private final val exceptionMsgKey  = exceptionPrefix + "message"

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

      override val ajaxReportClientError =
        token => req => {

          resolveUser(token, req.metadata.username).flatMap {
            case \/-(userOption) =>

              val data1 = req.error.other.mapKeysNow(exceptionPrefix + _)
                .updated(exceptionNameKey                  , req.error.name)
                .updated(exceptionMsgKey                   , req.error.message)
                .updated(exceptionPrefix + "stack"         , req.error.stack)
                .updated(exceptionPrefix + "componentStack", req.error.componentStack)

              for {
                data2 <- prepareMetadata(req.metadata)

                data = data1 ++ data2

                task = Task.ReportClientError(
                  userId     = userOption.map(_.id),
                  nameKey    = exceptionNameKey,
                  messageKey = exceptionMsgKey,
                  data       = data)

                _ <- taskman.submit(task)

              } yield ((), Allow)

            case -\/(_) =>
              F.pure(((), Deny))
          }
        }

      override val ajaxSubmitFeedback =
        token => req => {

          resolveUser(token, req.metadata.username).flatMap {
            case \/-(Some(user)) =>

              for {
                metadata <- prepareMetadata(req.metadata)

                task = Task.UserFeedbackReceived(
                  userId   = user.id,
                  feedback = req.input.feedback,
                  metadata = metadata)

                _ <- taskman.submit(task)

              } yield ((), Allow)

            case -\/(_) | \/-(None) =>
              F.pure(((), Deny))
          }
        }

    }
}
