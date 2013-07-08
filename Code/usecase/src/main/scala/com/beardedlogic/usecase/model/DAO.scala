package com.beardedlogic.usecase.model

import scala.slick.driver.PostgresDriver.simple._
import java.sql.Connection
import com.beardedlogic.usecase.lib.db.{DaoProvider, DB}

/**
 * Provides database connectivity.
 *
 * Methods should follow this pattern:
 * - `create`: Creates a new row. May or may not have an existing `data` or `value` row.
 * - `createInitial`: Creates a value for the first time. A new `data` row is created, and the value revision is 1.
 * - `find`: Searches for a single row. Returns Option[T].
 * - `findAll`: Searches for a multiple rows. Returns List[T].
 * - `findOrCreate`: Searches for an item and creates it if not found.
 * - `sync`: Syncs the database to a given object. If DB is up-to-date, nothing happens, else a new value is created.
 */
trait DatabaseAccessor {
  implicit def db: Session
}

/**
 * Single, monolithic interface to the database.
 */
class DAO(_session: Session)
  extends DataAccessor
          with ValueAccessor
          with FieldKeyAccessor
          with FieldValueAccessor
          with FieldListAccessor
          with StepAccessor
          with UseCaseAccessor
          with RelationAccessor
          with UserAccessor {

  override implicit val db = _session

  def conn = db.conn
  def withTransaction[T](f: => T): T = db.withTransaction(f)
  def close() = db.close
  def rollback() = db.rollback
}

object DefaultDaoProvider extends DaoProvider {
  override def get: DAO = new DAO(DB.Slick.createSession())
  override def withSession[T](block: DAO => T): T = DB.Slick.withSession(initConnAndExec(_, block))
  override def withTransaction[T](block: DAO => T): T = DB.Slick.withTransaction(initConnAndExec(_, block))

  @inline private def initConnAndExec[T](s: Session, block: DAO => T): T = {
    s.conn.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE)
    block(new DAO(s))
  }
}