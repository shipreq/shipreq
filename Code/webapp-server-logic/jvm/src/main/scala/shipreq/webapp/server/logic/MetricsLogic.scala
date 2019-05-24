package shipreq.webapp.server.logic

import java.time.Duration

trait MetricsLogic[F[_]] extends MetricsLogic.ForEvents[F] with MetricsLogic.ForRedis[F] {

  // {HttpRequests, HttpIO, HttpDuration} done directly in webapp-server
  // Here we just set the names
  def setHttpName(name: String): F[Unit]
  def setServerSideProcName(name: String): F[Unit]

  def securityEvent(event: Security.Event, result: Security.Result): F[Unit]

  def projectSpaWebSocketMsg(msgType : String,
                             bytesIn : Long,
                             bytesOut: Long,
                             duration: Duration,
                             success : Boolean): F[Unit]

  def projectSpaWebSocketPush(bytesOut: Long): F[Unit]

  def projectSpaWebSocketConnected(dur: Duration, result: String): F[Unit]
  def projectSpaWebSocketOpened(dur: Duration): F[Unit]
  def projectSpaWebSocketClosed(dur: Duration, sessionDur: Duration): F[Unit]
  def projectSpaWebSocketStep[A](process: String, step: String)(f: F[A]): F[A]
}

object MetricsLogic {

  trait ForEvents[F[_]] {
    def appliedEvents(eventCount: Int, dur: Duration, trusted: Boolean): F[Unit]
  }

  trait ForRedis[F[_]] {
    def redis(opName: String, dur: Duration): F[Unit]
  }

  def const[F[_]](f: F[Unit]): MetricsLogic[F] =
    new MetricsLogic[F] {
      override def setHttpName                 (x: String)                                            = f
      override def setServerSideProcName       (x: String)                                            = f
      override def securityEvent               (x: Security.Event, y: Security.Result)                = f
      override def projectSpaWebSocketMsg      (a: String, b: Long, c: Long, d: Duration, e: Boolean) = f
      override def projectSpaWebSocketPush     (a: Long)                                              = f
      override def projectSpaWebSocketConnected(a: Duration, b: String)                               = f
      override def projectSpaWebSocketOpened   (a: Duration)                                          = f
      override def projectSpaWebSocketClosed   (a: Duration, b: Duration)                             = f
      override def projectSpaWebSocketStep[A]  (a: String, b: String)(c: F[A])                        = c
      override def redis                       (a: String, d: Duration)                               = f
      override def appliedEvents               (a: Int, b: Duration, c: Boolean)                      = f
    }
}
