package shipreq.webapp.server.logic

import shipreq.base.util.log.HasLogger
import shipreq.webapp.server.logic.dispatch.Cookie

object SimpleEndpoints extends HasLogger {

  def logout[F[_]](implicit security: Security.Algebra[F]): F[Cookie.Update] =
    security.sessionPersist(Security.SessionToken.anonymous)
}
