package com.beardedlogic.usecase
package db

import scala.slick.driver.PostgresDriver.simple._

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
  implicit def session: Session
}

/**
 * Single, monolithic interface to the database.
 */
class Dao(_session: Session)
  extends FieldKeyAccessor
          with FieldListAccessor
          with TextAccessor
          with UcFieldAccessor
          with UseCaseAccessor
          with UserAccessor {

  override implicit final val session = _session

  def withTransaction[T](f: => T): T = session.withTransaction(f)
}
