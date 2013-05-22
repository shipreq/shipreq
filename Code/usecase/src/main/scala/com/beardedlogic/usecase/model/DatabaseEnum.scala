package com.beardedlogic.usecase
package model

import scala.slick.session.Session
import scala.slick.jdbc.{StaticQuery => Q}
import lib.{Enum, EnumValue}

/**
 * Enum with a corresponding database table.
 *
 * @tparam V The type of this enum's values.
 * @since 22/05/2013
 */
trait DatabaseEnum[V <: EnumValue] extends Enum[V] {

  val TableName: String
}

object DatabaseEnum {

  /**
   * Propagates all declared enum values to their corresponding DB tables.
   *
   * @param types The enum types.
   * @tparam V
   */
  def init[V <: EnumValue](types: DatabaseEnum[V]*)(implicit s: Session) {
    for {
      t <- types
      u = Q.update[(String, Int)](s"UPDATE ${t.TableName} SET name=? WHERE id=?")
      i = Q.update[(Int, String)](s"INSERT INTO ${t.TableName} VALUES(?,?)")
      v <- t.Values
    } {
      if (u.first((v.name, v.ordinal)) == 0)
        i.execute((v.ordinal, v.name))
    }
  }
}