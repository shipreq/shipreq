package shipreq.webapp.server.logic

import scalaz.Monad
import scalaz.syntax.bind._
import shipreq.base.util.FreeOption
import shipreq.webapp.base.data.ProjectId
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

  def injectServer(orig: Server.Algebra[F])(implicit F: Monad[F]): Server.Algebra[F] =
    new Server.Delegate(orig) {
      override val registerServerSideProc = (name, f) =>
        orig.registerServerSideProc(name, i =>
          setServerSideProcName(name) >> f(i))
    }

  def injectProjectStore(orig: ProjectServer.StoreAlgebra[F])(implicit F: Monad[F]): ProjectServer.StoreAlgebra[F] =
    new ProjectServer.StoreAlgebra[F] {
      import ProjectServer._
      import Store.Register.Node

      override val storeKeyCount =
        orig.storeKeyCount

      val updateMetrics =
        storeKeyCount.flatMap(setActiveProjectCount)

      override def storeGet(key: ProjectId) =
        orig.storeGet(key)

      override def storeMod(key: ProjectId)(f: FreeOption[Node[State, OnChange[F]]] => FreeOption[Node[State, OnChange[F]]]) =
        orig.storeMod(key)(f) <* updateMetrics

      override def storeModSet(key: ProjectId)(f: FreeOption[Node[State, OnChange[F]]] => Node[State, OnChange[F]]) =
        orig.storeModSet(key)(f) <* updateMetrics
    }
}

object MetricsLogic {

  def const[F[_]](f: F[Unit]): MetricsLogic[F] =
    new MetricsLogic[F] {
      override def setHttpName          (x: String)                                              = f
      override def setServerSideProcName(x: String)                                              = f
      override def sessionStart         (x: SessionId)                                           = f
      override def sessionEnd           (x: SessionId)                                           = f
      override def login                (x: SessionId, y: User)                                  = f
      override def logout               (x: SessionId)                                           = f
      override def securityEvent        (x: Security.Event, y: Security.Result)                  = f
      override def setActiveProjectCount(x: Int)                                                 = f
      override def injectServer         (x: Server.Algebra[F])(implicit F: Monad[F])             = x
      override def injectProjectStore   (x: ProjectServer.StoreAlgebra[F])(implicit F: Monad[F]) = x
    }
}
