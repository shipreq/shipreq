package shipreq.webapp.server.logic

import scalaz.Monad
import shipreq.base.util.log.HasLogger

object SimpleEndpoints extends HasLogger {

  def logout[F[_]](implicit F: Monad[F],
                   security: Security.Algebra[F]): F[Cookie.Update] =
    security.sessionPersist(Security.SessionToken.anonymous)
}
