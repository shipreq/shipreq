package shipreq.base.test.db

import cats.effect.Resource
import cats.effect.unsafe.implicits.global
import cats.syntax.apply._
import doobie._
import doobie.free.{connection => C}
import doobie.implicits._
import doobie.util.Colors
import doobie.util.testing._
import doobie.util.transactor.Strategy
import japgolly.microlibs.testutil.TestUtil._
import java.sql.Connection
import java.util.concurrent.Semaphore
import org.tpolecat.typename._
import scala.concurrent.ExecutionContext
import shipreq.base.db._
import shipreq.base.ops.{JdbcLogging, SqlTracer}
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.base.util.{Props, ThreadUtils}
import sourcecode.Line

object TestDb extends TestDbHelpers with HasLogger {

  var debug = false

  var rollback = true

  private def debugLog(msg: => String): Unit =
    if (debug) {
      import Console._
      println(s"$BOLD$YELLOW[TestDb] $msg$RESET")
    }

  private def sqlTracer: SqlTracer =
    JdbcLogging

  lazy val (cfg, cfgReport) = {
    val (cfg, cfgReport) = DbConfig.config.withReport.run(Props.sources).unsafeRun().getOrDie()
    cfg.modifyHikariDataSource(sqlTracer.inject)
    debugLog(cfgReport.used)
    (cfg, cfgReport)
  }

  lazy val db: DbAccessor = {
    debugLog("Creating DbAccessor...")

    val poolSize = if (cfg.poolSize == -1) 4 else cfg.poolSize
    assert(poolSize >= 1, s"DB pool size = $poolSize ?!")

    val ds = sqlTracer.inject(cfg.pgDataSource)

    /** Rollback everything */
    val txnStrategy = Strategy(C.setAutoCommit(false), C.rollback, C.rollback, C.unit)

    val sync = true

    val executionContext: Resource[Fx, ExecutionContext] =
      if (sync)
        Resource.pure[Fx, ExecutionContext](ExecutionContexts.synchronous)
      else
        ExecutionContexts.fixedThreadPool[Fx](poolSize)

    val xaRes: Resource[Fx, XA] =
      for {
        ce <- executionContext
      } yield {
        val xa = Transactor.fromDataSource[Fx](ds, ce, None).copy(strategy0 = txnStrategy)
        new XA(xa)
      }

    // Notice I'm using cfg.pgDataSource directly instead of ds
    val migrator = SchemaMigrator(cfg.pgDataSource, cfg.schema)

    DbAccessor(cfg, ds, xaRes, migrator)
  }

  private[this] val semaphore: Semaphore =
    new Semaphore(1)

  private case class State(xa: XA, shutdown: Fx[Unit], initError: Option[Throwable])

  @volatile private[this] var state: Option[State] =
    None

  @volatile private[this] var _tables: Set[DbTable] =
    null

  private[this] val initLock = new AnyRef

  def init(): Unit = {
    _init()
    ()
  }

  private def exec[A](xa: XA, cio: ConnectionIO[A]): A =
    cio.transact(xa).unsafeRun()

  private def _init(): State = {

    @inline def get(): State =
      state match {
        case Some(s) =>
          for (t <- s.initError)
            throw t
          s
        case None =>
          throw new IllegalStateException("Init failed")
      }

    if (this.state.isDefined)
      return get()

    initLock.synchronized {
      if (this.state.isDefined)
        return get()

      debugLog("Database initialising...")
      val (xa, shutdown) = db.xa.allocated.unsafeRun()
      val xa_ = new XA(xa)
      val state = State(xa_, shutdown, None)
      this.state = Some(state)
      ThreadUtils.runOnShutdown("TestDb", this.shutdown())
      try {
        db.migrator.migrate[Fx].unsafeRun()
      } catch {
        case _: Throwable =>
          try {
            _dropSchemaWithoutLocking(xa_)
            db.migrator.migrate[Fx].unsafeRun()
          } catch {
            case t: Throwable =>
              this.state = Some(state.copy(initError = Some(t)))
              throw t
          }
      }

      _tables = exec(xa_, DbTable.all(db.schema))
      debugLog(_tables.toList.sortBy(_.name).iterator.map("  - " + _.name).mkString("Detected tables:\n", "\n", ""))

      _truncateAllWithoutLocking(xa_)
      debugLog("Database initialised.")
      state
    }
  }

  def shutdown(): Unit = {
    // No lock here because if I run only a single test in LiveTest, it doesn't shutdown which doesn't release
    // the lock which freezes everything. Rather than avoid that scenario, let's just not use a lock here
    // and avoid all similar issues. Remember that this is TestDb.shutdown() which NO ONE calls except for:
    // 1) SBT via Common.scala
    // 2) the shutdown hook registered in _init() above
    val state = initLock.synchronized(this.state)
    state match {
      case Some(s) =>
        debugLog("Database shutting down...")
        s.shutdown.unsafeRun()
        this.state = None
        debugLog("Database shut down.")
      case None =>
        ()
    }
  }

  private def _dropSchemaWithoutLocking(xa: XA): Unit = {
    val allowed = "shipreq_test"
    val dbName = db.databaseName
    if (dbName != allowed)
      sys.error(s"You're trying to wipe $dbName. Only $allowed is allowed to be wiped.")
    debugLog(s"Dropping schema in: $dbName")
    db.migrator.drop[Fx].unsafeRun()
    // Re-creating extension doesn't need singleConnXA if we have xa_
    // Running because flyway.clean (i.e. db.migrator.drop) removes the hll extension 😨
    exec(xa, Update0("CREATE EXTENSION hll", None).run)
  }

  /** Drops all objects (tables, views, procedures, triggers, ...) in the configured schemas. */
  def dropSchema(): Unit = {
    val s = _init()
    semaphore.acquire()
    try {
      _dropSchemaWithoutLocking(s.xa)
    } finally {
      semaphore.release()
    }
  }

  // Don't expose because it allows downstream to execute multiple txns, all of which will be automatically rolled back
  // by the txn strategy.
  private def useXa[A](f: XA => Fx[A]): Fx[A] = {
    val s = _init()
    Fx.blocking(semaphore.acquire()).bracketFx_(
      use = f(s.xa),
      release = Fx(semaphore.release()))
  }

  // Unsafe in that we're not using Fx
  private def unsafeUseXa[A](f: XA => A): A =
    useXa(xa => Fx(f(xa))).unsafeRun()

  override  def ![A](query: ConnectionIO[A]): A =
    useXa(query.transact(_)).unsafeRun()

  override def tables: Set[DbTable] = {
    if (_tables == null) _init()
    _tables
  }

  private def _truncateAllWithoutLocking(xa: XA): Unit = {
    val t = if (_tables == null) exec(xa, DbTable.all(db.schema)) else _tables
    debugLog("Truncating all tables...")
    exec(xa, DbTable.truncateAll(t) *> C.commit)
  }

  def truncateAll(): Unit = {
    val s = _init()
    semaphore.acquire()
    try {
      _truncateAllWithoutLocking(s.xa)
    } finally {
      semaphore.release()
    }
  }

  // ===================================================================================================================
  // Imperative

  private lazy val connection: Resource[Fx, Connection] =
    Resource {
      Fx.blocking(semaphore.acquire()).flatMap { _ =>
        Fx {
          val c = db.dataSource.getConnection()

          val c2: Connection =
            new DelegateConnection(c) {
              override def close(): Unit = ()
            }

          val teardown = Fx {
            try
              c.close()
            finally
              semaphore.release()
          }

          (c2, teardown)
        }
      }
    }

  private lazy val singleConnXA: Resource[Fx, XA] = {
    for {
      c <- connection
    } yield {
      val x = Transactor.fromConnection[Fx](c, None).copy(strategy0 = Strategy.void)
      new XA(x)
    }
  }

  /** Everything runs in a transaction and is automatically rolled back */
  def withImperativeXA[A](f: ImperativeXA => A): A = {
    _init()
    singleConnXA.use { xa =>
      Fx {
        val i = new ImperativeXA(xa, db, () => tables)
        i ! C.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE)
        i ! C.setAutoCommit(false)
        val s = i ! C.setSavepoint
        try
          f(i)
        finally
          if (rollback)
            i ! C.rollback(s)
          else
            i ! C.commit
      }
    }.unsafeRun()
  }

  /** Returns a connection that really commits transactions on completion and writes to the DB.
    *
    * Ensure that you call [[releaseRealXA()]] after use.
    *
    * Use with care.
    * Use as a last resort.
    */
  def acquireRealXA(): ImperativeXA = {
    val s = _init()
    semaphore.acquire()
    val transactor: Transactor[Fx] = s.xa
    val xa = transactor.copy(strategy0 = Strategy.default)
    new ImperativeXA(new XA(xa), db, () => tables)
  }

  def releaseRealXA(): Unit = {
    semaphore.release()
  }

  def withRealXA[A](f: ImperativeXA => A): A =
    try
      f(acquireRealXA())
    finally
      releaseRealXA()

  // ===================================================================================================================
  // SQL checking

  def check[A: Analyzable](a: A): Unit =
    checkImpl(Analyzable.unpack(a))

  def checkOutput[A: TypeName](q: Query0[A])(implicit l: Line): Unit =
    checkImpl(AnalysisArgs(s"Query0[${typeName[A]}]", q.pos, q.sql, q.analysis))

  def checkOutput[A: TypeName, B: TypeName](q: Query[A, B])(implicit l: Line): Unit =
    checkImpl(AnalysisArgs(s"Query[${typeName[A]}, ${typeName[B]}]", q.pos, q.sql, q.analysis))

  private def checkImpl(args: AnalysisArgs)(implicit l: Line): Unit =
    unsafeUseXa { xa =>
      val report = analyze(args).transact(xa).unsafeRunSync()
      def reportText = formatReport(args, report, Colors.Ansi).padLeft("  ").toString
      if (!report.succeeded)
        fail(reportText)
      println(reportText)
    }
}
