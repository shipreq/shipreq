package shipreq.webapp.server.test

import com.googlecode.flyway.core.dbsupport.{SqlScript, DbSupportFactory}
import org.apache.commons.io.IOUtils
import org.postgresql.util.PSQLException
import org.scalatest.{Exceptional, Outcome, Suite}
import shipreq.base.util.{ThreadLocalRes, AsciiTable}
import scala.slick.jdbc.StaticQuery.{queryNA, query, updateNA, update}
import scala.slick.jdbc.JdbcBackend.{Database, Session}
import scalaz.Need
import shipreq.base.db.SqlHelpers._
import shipreq.taskman.api.UserId
import shipreq.webapp.base.data.Validators
import shipreq.webapp.server.app.{Defaults, DI}
import shipreq.webapp.server.data._
import shipreq.webapp.server.db
import shipreq.webapp.server.db.{AdminDao, DaoS, DaoT, DaoProvider, DB}
import shipreq.webapp.server.db.SqlHelpers._
import shipreq.webapp.server.security.PasswordAndSalt

object TestDB {

  @volatile private var ready = false

  def init(): Unit = synchronized {
    if (!ready) {
      ready = true
      TestHelpers.initLift()
      DB.wipe_!
      Defaults.uninit()
      (new bootstrap.liftweb.Boot).initDatabase()
    }
  }

  def reinitOnNextUse(): Unit = ready = false

  val Slick = Database.forDataSource(DB.DataSource)

  def withInstance[T](useTransaction: Boolean)(block: Session => T): T = {
    init()
    if (useTransaction) Slick.withTransaction(block) else Slick.withSession(block)
  }

  def withDbHelpers[R](transaction: Boolean)(f: TestDatabaseHelpers => R): R =
    withInstance(transaction)(s => f(TestDatabaseHelpers(s)))

  def threadLocalRes(rollback: Boolean = true) = ThreadLocalRes {
    TestDB.init()
    val s = TestDB.Slick.createSession()
    s.conn.setAutoCommit(false)
    s
  }(_.foreach { s =>
    if (rollback)
      s.conn.rollback()
    else
      s.conn.commit()
    s.close()
  })

  def threadLocalResH(rollback: Boolean = true) =
    threadLocalRes(rollback).strength(TestDatabaseHelpers(_))

  def threadLocalResHP(rollback: Boolean = true) =
    threadLocalResH(rollback).xmap(t => (t._1, t._2, t._2.newProjectId()))(t => (t._1, t._2))
}

trait TestDatabaseSupport extends TestHelpers with TestDatabaseHelpers {
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
      val oldAdminDaoVar = this.adminDaoVar
      try {
        this.sessionVar = s
        this.daoVar = db.Shim.newDaoT(s)
        this.adminDaoVar = db.Shim.newAdminDao(s)
        // s.conn.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE)
        DI.DaoProvider.doWith(testDaoProvider) {
          fn
        }
      }
      finally {
        if (rollback) s.rollback()
        this.sessionVar = oldSessionVar
        this.daoVar = oldDaoVar
        this.adminDaoVar = oldAdminDaoVar
      }
    }

  def beforeEachWithDao() {}

  val wrapTestsInTransaction = true

  var sessionVar: Session = null
  implicit def session = {
    if (sessionVar == null) throw new IllegalStateException("Trying to access DB session outside of test.")
    sessionVar
  }

  var daoVar: DaoT = null
  var adminDaoVar: AdminDao = null
  def dao = daoVar
  def adminDao = adminDaoVar

  def withNewTransaction[U](fn: => U, commit: Boolean = true): U = withTransactionInternal(true, !commit)(fn)
}

trait TestDatabaseHelpers extends TestHelpers2 {
  implicit def session: Session
  def dao: DaoT
  def adminDao: AdminDao

  def rollbackAfter[U](fn: => U): U = {
    dao.session.withTransaction {
      val result = fn
      dao.session.rollback()
      result
    }
  }

  def testDaoProvider = new TestDaoProvider(dao, adminDao)

  def randomId = -rnd.nextLong().abs

  def randomStr: String = rnd.nextString(32)

  def countRowsIn(table: Table) = queryNA[Int](s"select count(*) from ${table.name}").first

  // TODO @deprecated("Delete or redo, probably in a TestState-friendly way", "14/3/2016")
  sealed trait Table {def name: String; override def toString = name}
  object Tables {
    object Project extends Table {def name = "project"}
    object Usr extends Table {def name = "usr"}
    object UsrLoginLog extends Table {def name = "usr_login_log"}
    object Usrd extends Table {def name = "usrd"}
    object UsrhName extends Table {def name = "usrh_name"}
    val All = List(Project, Usr, UsrLoginLog, Usrd, UsrhName)
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
        case Usr          => truncate(Project, Usrd, UsrhName)
        case Project
          | Usrd | UsrhName
          | UsrLoginLog => // No one keys to these tables
      }
      val tableName = table.name
      updateNA(s"delete from $tableName").execute
    }
  }

  def lookupConfirmationToken(email: String) = query[String,String]("select confirmation_token from usr where email=?").apply(email).firstOption

  /**
   * Loads an SQL script on the classpath, and runs it.
   */
  def runSqlScript(filename: String) {
    val dbSupport = DbSupportFactory.createDbSupport(session.conn)
    val sqlFull = IOUtils.toString(getClass.getResource(filename.replaceFirst("^/?", "/")))
    val script = new SqlScript(sqlFull, dbSupport)
    script.execute(dbSupport.getJdbcTemplate)
  }

  def randomProjectName: String =
    findSuitable(Validators.projectName.correctAndValidateU(randomStr))(_.isSuccess).getOrElse(???)

  def newProjectId(userId: UserId = getOrCreateUserId): ProjectId =
    dao.createProject(userId, randomProjectName).gimme

  def getOrCreateUserId(): UserId =
    queryNA[UserId]("select id from usr where username is not null").firstOption.getOrElse(newUserId)

  def newUserId(): UserId =
    query[(String,String),UserId]("INSERT INTO usr(username, email, password, password_salt, password_changed_at, confirmation_sent_at, confirmed_at) VALUES(?,?,0,0,NOW(),NOW(),NOW()) RETURNING id")
    .apply(randomStr,randomStr).first

  def deleteUser(u: UserId): Unit =
    update[Long]("DELETE FROM usr WHERE id=?").apply(u.value).execute

  def debugSelect(sql: String): Unit = {
    val stmt = session.conn.createStatement()
    val rs = stmt.executeQuery(sql)
    val cols = (1 to rs.getMetaData.getColumnCount).toVector
    var lines = Vector.empty[Vector[String]]
    def readLine(f: Int => String): Unit =
      lines :+= cols.map(f)
    readLine(rs.getMetaData.getColumnName)
    while (rs.next())
      readLine(rs.getString)
    val table = AsciiTable(lines)
    println(s"\n> $sql\n$table\n")
  }

  def debugSelectOnError[A](sql: => String)(f: => A): A =
    try f catch {
      case t: Throwable =>
        debugSelect(sql)
        throw t
    }
}

object TestDatabaseHelpers {
  def apply(s: Session): TestDatabaseHelpers = new TestDatabaseHelpers {
    override implicit def session = s
    override val dao = db.Shim.newDaoT(s)
    override val adminDao = db.Shim.newAdminDao(s)
  }
}

class TestDaoProvider(dao: DaoT, adminDao: AdminDao) extends DaoProvider {
  override def withRawSession[T](f: Session => T): T       = f(dao.session)
  override protected def rawSession(): Session             = dao.session
  override protected def newDaoS    (s: Session): DaoS     = dao
  override protected def newDaoT    (s: Session): DaoT     = dao
  override protected def newAdminDao(s: Session): AdminDao = adminDao
  override def withLazySession[T](f: Need[DaoS] => T): T   = f(Need(dao))
}