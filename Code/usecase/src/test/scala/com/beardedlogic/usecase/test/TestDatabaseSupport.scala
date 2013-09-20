package com.beardedlogic.usecase
package test

import com.googlecode.flyway.core.dbsupport.{SqlScript, DbSupportFactory}
import java.sql.Connection
import net.liftweb.common.Logger
import org.apache.commons.io.IOUtils
import org.postgresql.util.PSQLException
import org.scalatest.{Exceptional, Outcome, Suite}
import scala.slick.jdbc.{StaticQuery => Q}
import slick.session.{Database, Session}
import scala.util.Random
import Q.interpolation

import db.{Dao, DaoProvider, DB}
import lib.DI

object TestDB {

  @volatile private var ready = false

  def init(): Unit = synchronized {
    if (!ready) {
      ready = true
      DB.wipe_!
      (new bootstrap.liftweb.Boot).boot
    }
  }

  val Random = new Random()

  val Slick = Database.forDataSource(DB.DataSource)

  def withInstance[T](useTransaction: Boolean)(block: Session => T): T = {
    init()
    if (useTransaction) Slick.withTransaction(block) else Slick.withSession(block)
  }
}

trait TestDatabaseSupport extends TestHelpers with Logger {
  self: Suite =>

  override protected def withFixture(test: NoArgTest): Outcome = {
    TestDB.init()
    debug(s"DB Test start: ${test.name}")
    try {
      val outcome = withTransactionInternal(wrapTestsInTransaction, wrapTestsInTransaction) {
        beforeEachWithDao()
        test()
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

  private def withTransactionInternal[U](transaction: Boolean, rollback: Boolean)(fn: => U): U =
    TestDB.withInstance(transaction) { s: Session =>
      val oldSessionVar = this.sessionVar
      val oldDaoVar = this.daoVar
      try {
        this.sessionVar = s
        this.daoVar = new Dao(s)
        s.conn.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE)
        DI.DaoProvider.doWith(testDaoProvider) {
          fn
        }
      }
      finally {
        if (rollback) s.rollback()
        this.sessionVar = oldSessionVar
        this.daoVar = oldDaoVar
      }
    }

  def beforeEachWithDao() {}

  val wrapTestsInTransaction = true

  var sessionVar: Session = null
  implicit def session = {
    if (sessionVar == null) throw new IllegalStateException("Trying to access DB session outside of test.")
    sessionVar
  }

  var daoVar: Dao = null
  def dao = daoVar

  def withNewTransaction[U](fn: => U): U = withTransactionInternal(true, true)(fn)

  def rollbackAfter[U](fn: => U): U = dao.withTransaction {
    val result = fn
    dao.session.rollback()
    result
  }

  def testDaoProvider = new TestDaoProvider(dao)

  def randomId = -TestDB.Random.nextLong().abs

  def countRowsIn(table: Table) = Q.queryNA[Int](s"select count(*) from ${table.name}").first

  sealed trait Table {def name: String; override def toString = name}
  object Tables {
    object FieldKeyType extends Table {def name = "field_key_type"}
    object FieldKey extends Table {def name = "field_key"}
    object Usecase extends Table {def name = "usecase"}
    object UsecaseRev extends Table {def name = "usecase_rev"}
    object Text extends Table {def name = "text"}
    object TextRev extends Table {def name = "text_rev"}
    object UcField extends Table {def name = "uc_field"}
    object Usr extends Table {def name = "usr"}
    val All = List(FieldKeyType, FieldKey, Usecase, UsecaseRev, Text, TextRev, UcField, Usr)
  }

  def countAllTableRows = Tables.All.map(t => (t -> countRowsIn(t))).toMap

  def collectTableDiffs[T](fn: => T): (T, Map[Table, Int]) = {
    val before = countAllTableRows
    val result = fn
    val after = try {
       countAllTableRows
    } catch {
      case e: PSQLException if e.getMessage.contains("current transaction is aborted") => before
    }
    val diff = after.map {case (t, newCount) => (t, newCount - before(t))}.toMap
    (result, diff)
  }

  def assertTableDiffs[T](expectations: (Table, Int)*)(fn: => T) = {
    val (result, diff) = collectTableDiffs(fn)

    val specTables = expectations.map(_._1)
    val unspecTables = Tables.All.filter(!specTables.contains(_)).map((_,0))
    val fullExp = expectations ++ unspecTables
    val fullExpMap = fullExp.toMap

    if (diff != fullExp) {
      val badKeys = diff.keys.filter(k => diff(k) != fullExpMap(k)).toSet
      val a = diff.filter(e => badKeys.contains(e._1))
      val e= fullExpMap.filter(e => badKeys.contains(e._1))
      a should be(e)
    }

    result
  }

  def truncateAll() = truncate(Tables.All: _*)

  def truncate(tables: Table*) {
    import Tables._
    tables.foreach { table =>
    // Dependents first
      table match {
        case FieldKeyType => truncate(FieldKey)
        case FieldKey     => truncate(Text)
        case Usecase      => truncate(UsecaseRev)
        case UsecaseRev   => Q.updateNA(s"update usecase set latest_rev_id = NULL").execute; truncate(UcField)
        case Text         => truncate(TextRev)
        case TextRev      => truncate(UcField)
        case _             =>
      }
      val tableName = table.name
      Q.updateNA(s"delete from $tableName").execute
    }
  }

  def lookupConfirmationToken(email: String) = sql"select confirmation_token from usr where email = $email".as[String].firstOption

  /**
   * Loads an SQL script on the classpath, and runs it.
   */
  def runSqlScript(filename: String) {
    val dbSupport = DbSupportFactory.createDbSupport(session.conn)
    val sqlFull = IOUtils.toString(getClass.getResource(filename.replaceFirst("^/?", "/")))
    val script = new SqlScript(sqlFull, dbSupport)
    script.execute(dbSupport.getJdbcTemplate)
  }
}

class TestDaoProvider(dao: Dao) extends DaoProvider {
  override def get = dao
  override def withSession[T](block: Dao => T): T = block(dao)
  override def withTransaction[T](block: Dao => T): T = block(dao)
}