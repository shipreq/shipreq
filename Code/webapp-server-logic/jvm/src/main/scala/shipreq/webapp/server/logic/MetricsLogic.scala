package shipreq.webapp.server.logic

import scalaz.Monad
import scalaz.syntax.bind._
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

//    val ProjectsActive =
//      Gauge.build(prefix + "projects_active", "Projects currently being served")

  final def injectServer(orig: Server.Algebra[F])(implicit F: Monad[F]): Server.Algebra[F] =
    new Server.Delegate(orig) {
      override val registerServerSideProc = (name, f) =>
        orig.registerServerSideProc(name, i =>
          setServerSideProcName(name) >> f(i))
    }

}

object MetricsLogic {

  def const[F[_]](f: F[Unit]): MetricsLogic[F] =
    new MetricsLogic[F] {
      override def setHttpName(name: String) = f
      override def setServerSideProcName(name: String) = f
      override def sessionStart(sessionId: SessionId) = f
      override def sessionEnd(sessionId: SessionId) = f
      override def login(sessionId: SessionId, user: User) = f
      override def logout(sessionId: SessionId) = f
    }
}