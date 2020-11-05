package shipreq.webapp.server.logic.impl

import scalaz.Monad
import scalaz.syntax.monad._
import shipreq.base.util.log.HasLogger
import shipreq.webapp.server.logic.algebra.Security
import shipreq.webapp.server.logic.dispatch.Cookie

object SimpleEndpoints extends HasLogger {

  def logout[F[_]](cookies: Cookie.LookupFn)(implicit security: Security.Algebra[F], F: Monad[F]): F[Cookie.Update] =
    for {
      s <- security.sessionRestoreOrCreate(cookies)
      u <- security.sessionPersist(s.logout)
    } yield u

}
