package shipreq.webapp.server.logic

import scalaz.Monad
import scalaz.syntax.monad._
import shipreq.base.util.log.HasLogger

object SimpleEndpoints extends HasLogger {

  def logout[F[_]](implicit F: Monad[F],
                   metrics: MetricsLogic[F],
                   security: Security.Algebra2[F],
                   svr: Server.Session[F]): F[Cookie.Update] = {

    val updateMetrics: F[Unit] =
      svr.sessionId.flatMap {
        case Some(s) => metrics.logout(s)
        case None    => F.point(logger.warn("Logout in progress but session unavailable."))
      }

    for {
      _ <- updateMetrics
      c <- security.sessionPersist(Security.SessionToken.anonymous)
    } yield c
  }
}
