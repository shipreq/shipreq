package shipreq.webapp.server.logic

import java.time.Duration
import shipreq.webapp.base.user.User

trait MetricsLogic[F[_]] {

  // {HttpRequests, HttpIO, HttpDuration} done directly in webapp-server
  // Here we just set the names
  def setHttpName(name: String): F[Unit]
  def setServerSideProcName(name: String): F[Unit]

  def sessionStart(sessionId: SessionId): F[Unit]
  def sessionEnd(sessionId: SessionId): F[Unit]

  def login(sessionId: SessionId, user: User): F[Unit]
  def logout(sessionId: SessionId): F[Unit]

  def securityEvent(event: Security.Event, result: Security.Result): F[Unit]

  def setActiveProjectCount(n: Int): F[Unit]

  def projectSpaWebSocketMsg(msgType : String,
                             bytesIn : Long,
                             bytesOut: Long,
                             duration: Duration,
                             success : Boolean): F[Unit]

  def projectSpaWebSocketPush(bytesOut: Long): F[Unit]
}

object MetricsLogic {

  def const[F[_]](f: F[Unit]): MetricsLogic[F] =
    new MetricsLogic[F] {
      override def setHttpName            (x: String)                                            = f
      override def setServerSideProcName  (x: String)                                            = f
      override def sessionStart           (x: SessionId)                                         = f
      override def sessionEnd             (x: SessionId)                                         = f
      override def login                  (x: SessionId, y: User)                                = f
      override def logout                 (x: SessionId)                                         = f
      override def securityEvent          (x: Security.Event, y: Security.Result)                = f
      override def setActiveProjectCount  (x: Int)                                               = f
      override def projectSpaWebSocketMsg (a: String, b: Long, c: Long, d: Duration, e: Boolean) = f
      override def projectSpaWebSocketPush(a: Long)                                              = f
    }
}
