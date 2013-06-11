package com.beardedlogic.usecase
package test

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{Exceptional, Outcome, Suite}
import scala.slick.jdbc.{StaticQuery => Q}
import scala.slick.session.Session
import lib.db.DB
import scala.util.Random
import net.liftweb.common.Logger
import com.beardedlogic.usecase.model.DAO
import java.sql.Connection

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

  override protected def withFixture(test: NoArgTest): Outcome = {
    TestDatabaseSupport.init()
    debug(s"DB Test start: ${test.name}")
    try {
      val outcome = DB.Slick.withTransaction { s: Session =>
        this.sessionVar = s
        this.dbVar = new DAO(s)
        s.conn.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE)
        try test()
        finally {
          s.rollback()
          this.sessionVar = null
          this.dbVar = null
        }
      }
      outcome match {
        case Exceptional(e) => debug("Test failure.", e)
        case _ =>
      }
      outcome
    }
    catch {case e: Throwable => error("Test error.", e); throw e }
    finally debug(s"DB Test end: ${test.name}")
  }

  var sessionVar: Session = null
  implicit def session = sessionVar

  var dbVar: DAO = null
  def db = dbVar

  def randomId = -TestDatabaseSupport.Random.nextLong().abs

  def countRowsIn(table: Symbol) = Q.queryNA[Int](s"select count(*) from ${table.name}").first

  val Tables = List(
    'data_type,
    'relation_type,
    'field_key_type,
    'data,
    'value,
    'relation,
    'field_key,
    'field_value,
    'usecase,
    'step
  )
  val ValueTables = List(
    'value,
    'field_key,
    'field_value,
    'usecase,
    'step
  )

  def assertTableDiffs[T](expectations: (Symbol, Int)*)(block: => T) = {
    val specTables = expectations.map(_._1)
    val unspecTables = Tables.filter(!specTables.contains(_)).map((_,0))
    val fullExp = expectations ++ unspecTables
    val fullExpMap = fullExp.toMap

    def count = fullExp.map { case (t, _) => (t -> countRowsIn(t)) }.toMap
    val before = count
    val result = block
    val after = count.map { case (t, newCount) => (t, newCount - before(t)) }.toMap

    if (after != fullExp) {
      val badKeys = after.keys.filter(k=> after(k) != fullExpMap(k)).toSet
      val a = after.filter(e => badKeys.contains(e._1))
      val e= fullExpMap.filter(e => badKeys.contains(e._1))
      a should be(e)
    }

    result
  }

  def truncate(tables: Symbol*) {
    tables.foreach { table =>
    // Dependents first
      table match {
        case 'data_type      => truncate('data)
        case 'data           => truncate('value)
        case 'value          => truncate('relation, 'field_key, 'field_value, 'usecase)
        case 'relation_type  => truncate('relation)
        case 'field_key_type => truncate('field_key)
        case _                =>
      }
      val tableName = table.name
      if (ValueTables.contains(table)) {
        Q.updateNA(s"delete from relation where from_id in (select id from $tableName) OR to_id in (select id from $tableName)").execute
        if (table != "value")
          Q.updateNA(s"delete from value where id in (select id from $tableName)").execute
      }
      Q.updateNA(s"delete from $tableName").execute
    }
  }
}
