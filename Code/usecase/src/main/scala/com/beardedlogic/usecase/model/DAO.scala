package com.beardedlogic.usecase.model

import scala.slick.driver.PostgresDriver.simple._
import com.beardedlogic.usecase.lib.db.DB

trait DatabaseAccessor {
  implicit def db: Session
}

/**
 * Single, monolithic interface to the database.
 */
class DAO(_session: Session)
  extends DataAccessor
          with ValueAccessor
          with RelationAccessor
          with FieldKeyAccessor
          with FieldValueAccessor
          with FieldListAccessor
          with StepAccessor {
  override implicit val db = _session

  def withTransaction[T](f: => T): T = db.withTransaction(f)
  def close() = db.close
  def rollback() = db.rollback
}

object DAO {

  def withSession[T](block: DAO => T): T = {
    DB.Slick.withSession { db => block(new DAO(db)) }
  }

  def withTransaction[T](block: DAO => T): T = {
    DB.Slick.withTransaction { db => block(new DAO(db)) }
  }

  def get = new DAO(DB.Slick.createSession())
}
