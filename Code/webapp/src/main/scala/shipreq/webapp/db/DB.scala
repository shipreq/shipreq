package shipreq.webapp.db

import scala.slick.session.Session
import shipreq.base.db.{DbTemplate, BaseDbConnection}

/**
 * Database connectivity.
 *
 * @since 21/05/2013
 */
object DB extends DbTemplate {

  override protected def establishConnection() = {
    import shipreq.webapp.util.PropsRetrievers._
    new BaseDbConnection()
  }

  override protected def onInit(implicit s: Session) = {
    FieldKeyType.init
  }

  @inline def DataSource = baseConn.DataSource
  import baseConn.Slick

  object DaoProvider extends DaoProvider {
    override def withRawSession[T](f: Session => T): T = Slick.withSession(f)
    override protected def rawSession(): Session       = Slick.createSession()
  }
}
