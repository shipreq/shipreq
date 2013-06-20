package com.beardedlogic.usecase.model

import scala.slick.driver.PostgresDriver.simple._
import java.sql.Connection
import com.beardedlogic.usecase.lib.db.DB

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
          with RelationAccessor {

  override implicit val db = _session

  def conn = db.conn
  def setTransactionIsolation() = conn.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE)
  def withTransaction[T](f: => T): T = db.withTransaction {setTransactionIsolation(); f}
  def close() = db.close
  def rollback() = db.rollback
}

object DAO {

  def withInstance[T](transaction: Boolean)(block: DAO => T): T = {
    if (transaction) withTransaction(block) else withSession(block)
  }

  def withSession[T](block: DAO => T): T = {
    DB.Slick.withSession { s => block(new DAO(s)) }
  }

  def withTransaction[T](block: DAO => T): T = {
    DB.Slick.withTransaction(s => {
      val dao = new DAO(s)
      dao.setTransactionIsolation()
      block(dao)
    })
  }

  def get = new DAO(DB.Slick.createSession())

  trait DaoMonad {
    protected def exec[T](f: DAO => T): T
    def foreach[T](f: DAO => T): Unit = exec(f(_))
    def map[T](f: DAO => T): T = exec(f(_))
  }
  trait DaoMonad1[M[_]] extends DaoMonad {def flatMap[T](f: DAO => M[T]): M[T] = exec(f(_))}
  trait DaoMonadR[L, M[L, _]] extends DaoMonad {def flatMap[T](f: DAO => M[L, T]): M[L, T] = exec(f(_))}
  trait DaoMonadL[R, M[_, R]] extends DaoMonad {def flatMap[T](f: DAO => M[T, R]): M[T, R] = exec(f(_))}

  /** Provides a DAO and new session in a for-comprehension. */
  def forSession[M[_]] = new DaoMonad1[M] {
    protected override def exec[T](f: DAO => T): T = DAO.withSession(f(_))
  }
  /** Provides a DAO and new transaction in a for-comprehension. */
  def forTransaction[M[_]] = new DaoMonad1[M] {
    protected override def exec[T](f: DAO => T): T = DAO.withTransaction(f(_))
  }

  def forTransactionLeft[R, M[_, R]] = new DaoMonadL[R, M] {
    protected override def exec[T](f: DAO => T): T = DAO.withTransaction(f(_))
  }
  def forSessionLeft[R, M[_, R]] = new DaoMonadL[R, M] {
    protected override def exec[T](f: DAO => T): T = DAO.withSession(f(_))
  }

  def forTransactionRight[L, M[L, _]] = new DaoMonadR[L, M] {
    protected override def exec[T](f: DAO => T): T = DAO.withTransaction(f(_))
  }
  def forSessionRight[L, M[L, _]] = new DaoMonadR[L, M] {
    protected override def exec[T](f: DAO => T): T = DAO.withSession(f(_))
  }
}