package shipreq.webapp.db

import scala.slick.jdbc.JdbcBackend.Session
import shipreq.base.db.{DatabaseConnection, DbTemplate}

/**
 * Database connectivity.
 *
 * @since 21/05/2013
 */
object DB extends DbTemplate {

  override protected def newConnection = {
    import shipreq.webapp.util.PropsRetrievers._
    DatabaseConnection.establish_!()
  }

  @inline def DataSource = connection.ds

  private[this] val slick = _slick

  // Making public for tests
  override def wipe_!(): Unit = super.wipe_!()

  override protected def onInit(implicit s: Session) = {
    FieldKeyType.init
  }

  object DaoProvider extends DaoProvider {
    override def withRawSession[T](f: Session => T): T = slick.withSession(f)
    override protected def rawSession(): Session       = slick.createSession()
  }
}
