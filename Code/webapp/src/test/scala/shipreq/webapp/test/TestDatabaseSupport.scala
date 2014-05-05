package shipreq.webapp
package test

import com.googlecode.flyway.core.dbsupport.{SqlScript, DbSupportFactory}
import org.apache.commons.io.IOUtils
import org.postgresql.util.PSQLException
import org.scalatest.{Exceptional, Outcome, Suite}
import scala.slick.jdbc.{StaticQuery => Q}
import scalaz.Need
import scala.slick.jdbc.JdbcBackend.{Database, Session}
import Q.interpolation

import shipreq.taskman.api.Types.IsUserId
import app.{Defaults, DI}
import db.{AdminDao, UseCaseHeader, DaoS, DaoT, DaoProvider, DB, UseCaseRev}
import db.SqlHelpers.SP_ProjectId
import lib.Types._
import lib.Locks
import feature.UcFilters
import feature.uc.UseCase
import feature.uc.persist.{UseCaseSaveCheckpoint, UseCasePersistence}
import feature.validation.Validators
import security.PasswordAndSalt

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

  def countRowsIn(table: Table) = Q.queryNA[Int](s"select count(*) from ${table.name}").first

  sealed trait Table {def name: String; override def toString = name}
  object Tables {
    object FieldKeyType extends Table {def name = "field_key_type"}
    object FieldKey extends Table {def name = "field_key"}
    object Project extends Table {def name = "project"}
    object Usecase extends Table {def name = "usecase"}
    object UsecaseRev extends Table {def name = "usecase_rev"}
    object Text extends Table {def name = "text"}
    object TextRev extends Table {def name = "text_rev"}
    object UcField extends Table {def name = "uc_field"}
    object Usr extends Table {def name = "usr"}
    object Share extends Table {def name = "share"}
    object ShareViewLog extends Table {def name = "share_view_log"}
    object UsrLoginLog extends Table {def name = "usr_login_log"}
    object Usrd extends Table {def name = "usrd"}
    object UsrhName extends Table {def name = "usrh_name"}
    val All = List(FieldKeyType, FieldKey, Project, Usecase, UsecaseRev, Text, TextRev, UcField, Usr, Share,
      ShareViewLog, UsrLoginLog, Usrd, UsrhName)
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
        case Project      => truncate(Share, Usecase)
        case Usr          => truncate(Project, Usrd, UsrhName)
        case UcField
          | Share
          | Usrd | UsrhName
          | UsrLoginLog
          | ShareViewLog  => // No one keys to these tables
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

  /**
   * When a new UC is saved it gets given the number available UC number. Use this to correct the UC number after the
   * fact.
   *
   * @param n The new UC number.
   */
  def forceUcNumber(cp: UseCaseSaveCheckpoint, n: Int): UseCaseSaveCheckpoint = {
    val ucn = n.toShort.tag[IsUseCaseNumber]
    val uc_ = cp.uc.copy(number = ucn)
    val rec_ = cp.rec.copy(ident = cp.rec.ident.copy(number = ucn))
    sqlu"UPDATE usecase set number=${ucn.toShort} where id = ${rec_.ident.identId.toLong}".execute
    cp.copy(uc = uc_, rec = rec_)
  }

  def randomUCTitle: String @@ Validated =
    findSuitable(Validators.usecase.title.correctAndValidate(randomStr))(_.isSuccess).getOrElse(???)

  def newProjectId(userId: UserId = getOrCreateUserId): ProjectId =
    dao.createProject(userId, randomUCTitle).gimme

  def getOrCreateUserId(): UserId =
    sql"select id from usr where username is not null".as[Long].firstOption.map(_.tag[IsUserId]).getOrElse(newUserId)

  def newUserId(): UserId =
    sql"INSERT INTO usr(username, email, password, password_salt, password_changed_at, confirmation_sent_at, confirmed_at) VALUES($randomStr,$randomStr,0,0,NOW(),NOW(),NOW()) RETURNING id".
    as[Long].first.tag[IsUserId]

  def deleteUser(u: UserId): Unit =
    Q.update[Long]("DELETE FROM usr WHERE id=?").execute(u)

  def newShare(projectId: ProjectId = newProjectId()): ShareId =
    dao.createShare(projectId, PasswordAndSalt.createWithRandomSalt(randomStr), randomStr, None, UcFilters.All.json).id

  def saveUseCase(uc: UseCase, prev: Option[UseCaseSaveCheckpoint], projectId: ProjectId): Option[UseCaseSaveCheckpoint] = prev match {
    case Some(cp) => UseCasePersistence.save(uc, cp, Locks.SingleUseCase.writeP(cp.rec, projectId), dao)
    case None =>
      val ucr = createUseCaseIdentAndRev1(projectId, uc.header)
      val someCp = Some(loadUseCase(ucr, projectId))
      saveUseCase(uc, someCp, projectId).orElse(someCp)
  }

  def loadUseCase(ucRev: UseCaseRev, projectId: ProjectId): UseCaseSaveCheckpoint =
    Locks.UseCaseNumbers.readP(projectId)(UseCasePersistence.load(ucRev).run(dao, _))

  def createUseCaseIdentAndRev1(projectId: ProjectId, header: UseCaseHeader): UseCaseRev =
    Locks.UseCaseNumbers.write(projectId)(dao.createUseCaseIdentAndRev1(projectId, header, _))

  def updateUseCaseHeader(ucId: UseCaseIdentId, modFn: UseCaseHeader => UseCaseHeader)(implicit projectId: ProjectId) =
    Locks.SingleUseCase.write(ucId, projectId)(dao.updateUseCaseHeader(ucId, modFn, _))
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