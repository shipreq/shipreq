package com.beardedlogic.usecase
package test

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{Outcome, Suite}
import scala.slick.jdbc.{StaticQuery => Q}
import scala.slick.session.Session
import lib.db.DB
import scala.util.Random
import net.liftweb.common.Logger

object TestDatabaseSupport {

  @volatile private var ready = false

  def init() {
    synchronized {
      if (!ready) {
        ready = true
        DB.wipe_!
        (new bootstrap.liftweb.Boot).boot
      }
    }
  }

  val Random = new Random()
}

trait TestDatabaseSupport extends ShouldMatchers with Logger {
  self: Suite =>

  TestDatabaseSupport.init()

  override protected def withFixture(test: NoArgTest): Outcome = {
    DB.Slick.withTransaction { s: Session =>
      this.dbVar = s
      try test()
      finally {
        s.rollback()
        this.dbVar = null
      }
    }
  }

  var dbVar: Session = null
  implicit def db = dbVar

  def randomId = -TestDatabaseSupport.Random.nextLong().abs

  def countRowsIn(table: String) = Q.queryNA[Int](s"select count(*) from $table").first

  def assertTableDiffs[T](expectations: (String, Int)*)(block: => T) = {
    def count = expectations.map { case (t, _) => (t -> countRowsIn(t)) }.toMap
    val before = count
    val result = block
    val after = count.map { case (t, newCount) => (t, newCount - before(t)) }.toMap
    after should be(expectations.toMap)
    result
  }

  def truncate(tables: String*) {
    tables.foreach { table =>
    // Dependents first
      table.toLowerCase.trim match {
        case "data_type"      => truncate("data")
        case "data"           => truncate("value")
        case "value"          => truncate("relation", "field_key", "field_value", "text", "usecase")
        case "text"           => truncate("step")
        case "relation_type"  => truncate("relation")
        case "field_key_type" => truncate("field_key")
        case _                =>
      }
      Q.updateNA(s"delete from $table").execute
    }
  }
}
