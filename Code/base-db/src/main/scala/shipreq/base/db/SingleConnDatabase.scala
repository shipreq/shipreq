package shipreq.base.db

import scala.slick.session.{Database, Session}

case class SingleConnDatabase(s: Session) extends Database {
  override def createConnection() = s.conn
  override def withSession[T](f: Session => T): T = f(s)
}

