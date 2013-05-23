package com.beardedlogic.usecase
package test

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{Outcome, Suite}
import scala.slick.jdbc.{StaticQuery => Q}
import scala.slick.session.Session
import lib.db.DB

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
}

trait TestDatabaseSupport extends ShouldMatchers {
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

  def countRowsIn(table: String) = Q.queryNA[Int](s"select count(*) from $table").first

  def assertTableDiffs[T](expectations: (String, Int)*)(block: => T) = {
    def count = expectations.map { case (t, _) => (t -> countRowsIn(t)) }.toMap
    val before = count
    val expected = expectations.map { case (t, delta) => (t, delta + before(t)) }.toMap
    val result = block
    val after = count
    after should be(expected)
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
