package shipreq.base.test.db

import cats.effect.{Blocker, ContextShift, IO, Resource}
import cats.syntax.apply._
import doobie._
import doobie.free.{connection => C}
import doobie.implicits._
import doobie.util.Colors
import doobie.util.testing._
import doobie.util.transactor.Strategy
import japgolly.microlibs.testutil.TestUtil._
import java.sql.Connection
import java.util.concurrent.locks.{ReadWriteLock, ReentrantReadWriteLock}
import scala.concurrent.ExecutionContext
import scala.reflect.runtime.universe.TypeTag
import shipreq.base.db._
import shipreq.base.ops.{JdbcLogging, SqlTracer}
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.base.util.{LockUtils, Props, ThreadUtils}
import sourcecode.Line

object TestDb extends TestDbHelpers with HasLogger {

  var debug = false

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

    implicit val cs: ContextShift[Fx] =
      if (sync)
        IO.contextShift(ExecutionContexts.synchronous)
      else
        ThreadUtils.newThreadPool("DB", logger).withThreads(poolSize).contextShift

    val blocker: Resource[Fx, Blocker] =
      if (sync)
        Resource.pure[Fx, Blocker](Blocker.liftExecutionContext(ExecutionContexts.synchronous))
      else
        Blocker[Fx]

    val executionContext: Resource[Fx, ExecutionContext] =
      if (sync)
        Resource.pure[Fx, ExecutionContext](ExecutionContexts.synchronous)
      else
        ExecutionContexts.fixedThreadPool[Fx](poolSize)

    val xaRes: Resource[Fx, XA] =
      for {
        ce <- executionContext
        be <- blocker
      } yield {
        val xa = Transactor.fromDataSource[Fx](ds, ce, be).copy(strategy0 = txnStrategy)
        new XA(xa)
      }

    // Notice I'm using cfg.pgDataSource directly instead of ds
    val migrator = SchemaMigrator(cfg.pgDataSource, cfg.schema)

    DbAccessor(cfg, ds, xaRes, migrator)
  }

  private[this] val rwlock: ReadWriteLock =
    new ReentrantReadWriteLock()

  private case class State(xa: XA, shutdown: Fx[Unit], initError: Option[Throwable])

  @volatile private[this] var state: Option[State] =
    None

  private[this] val initLock = new AnyRef

  def init(): Unit = {
    _init()
    ()
  }

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
      val state = State(new XA(xa), shutdown, None)
      this.state = Some(state)
      ThreadUtils.runOnShutdown("TestDb", this.shutdown())
      try {
        db.migrator.migrate[Fx].unsafeRun()
      } catch {
        case _: Throwable =>
          try {
            dropSchema()
            db.migrator.migrate[Fx].unsafeRun()
          } catch {
            case t: Throwable =>
              this.state = Some(state.copy(initError = Some(t)))
              throw t
          }
      }
      truncateAllWithoutLocking()
      debugLog("Database initialised.")
      state
    }
  }

  def shutdown(): Unit = {
    // No write-lock here because if I run only a single test in LiveTest, it doesn't shutdown which doesn't release
    // the write-lock which freezes everything. Rather than avoid that scenario, let's just not use a write-lock here
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

  /** Drops all objects (tables, views, procedures, triggers, ...) in the configured schemas. */
  def dropSchema(): Unit =
    LockUtils.inMutex(rwlock.writeLock) {
      val allowed = "shipreq_test"
      val dbName = db.databaseName
      if (dbName != allowed)
        sys.error(s"You're trying to wipe $dbName. Only $allowed is allowed to be wiped.")
      debugLog(s"Dropping schema in: $dbName")
      db.migrator.drop[Fx].unsafeRun()
      singleConnXA.use { xa =>
        Fx {
          val i = new ImperativeXA(xa, db, () => tables)
          i.update("CREATE EXTENSION hll") // because flyway.clean (i.e. db.migrator.drop) removes the hll extension 😨
        }
      }.unsafeRun()
    }

  // Unsafe because all usage should be wrapped in locks
  private val _xa: Fx[XA] =
    Fx(_init().xa)

  // Don't expose because it allows downstream to execute multiple txns, all of which will be automatically rolled back
  // by the txn strategy.
  private def useXa[A](mutex: Boolean)(f: XA => Fx[A]): Fx[A] = {
    val lock = if (mutex) rwlock.writeLock else rwlock.readLock
    LockUtils.inMutexFx(lock)(_xa.flatMap(f))
  }

  // Unsafe in that we're not using Fx
  private def unsafeUseXa[A](mutex: Boolean)(f: XA => A): A =
    useXa(mutex)(xa => Fx(f(xa))).unsafeRun()

  override  def ![A](query: ConnectionIO[A]): A =
    useXa(mutex = false)(query.transact(_)).unsafeRun()

  override lazy val tables: Set[DbTable] = {
    val t = this ! DbTable.all(db.schema)
    debugLog(t.toList.sortBy(_.name).iterator.map("  - " + _.name).mkString("Detected tables:\n", "\n", ""))
    t
  }

  private def truncateAllWithoutLocking(): Unit = {
    val t = tables
    debugLog("Truncating all tables...")
    this.!(DbTable.truncateAll(t) *> C.commit)
  }

  def truncateAll(): Unit =
    LockUtils.inMutex(rwlock.writeLock) {
      truncateAllWithoutLocking()
    }

  // ===================================================================================================================
  // Imperative

  private lazy val connection: Resource[Fx, Connection] =
    Resource {
      val lock = rwlock.readLock
      Fx {
        lock.lockInterruptibly()
        val c = db.dataSource.getConnection()

        val c2: Connection =
          new DelegateConnection(c) {
            override def close(): Unit = ()
          }

        val teardown = Fx {
          try
            c.close()
          finally
            lock.unlock()
        }

        (c2, teardown)
      }
    }

  private lazy val singleConnXA: Resource[Fx, XA] = {
    implicit val ec = IO.contextShift(ExecutionContexts.synchronous)
    for {
      c <- connection
    } yield {
      val b = Blocker.liftExecutionContext(ExecutionContexts.synchronous)
      val x = Transactor.fromConnection[Fx](c, b).copy(strategy0 = Strategy.void)
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
          i ! C.rollback(s)
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
    rwlock.writeLock().lockInterruptibly()
    val xa = _xa.unsafeRun().copy(strategy0 = Strategy.default)
    new ImperativeXA(new XA(xa), db, () => tables)
  }

  def releaseRealXA(): Unit = {
    rwlock.writeLock().unlock()
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

  def checkOutput[A: TypeTag](q: Query0[A])(implicit l: Line): Unit =
    checkImpl(AnalysisArgs(s"Query0[${typeName[A]}]", q.pos, q.sql, q.outputAnalysis))

  def checkOutput[A: TypeTag, B: TypeTag](q: Query[A, B])(implicit l: Line): Unit =
    checkImpl(AnalysisArgs(s"Query[${typeName[A]}, ${typeName[B]}]", q.pos, q.sql, q.outputAnalysis))

  private def checkImpl(args: AnalysisArgs)(implicit l: Line): Unit =
    unsafeUseXa(mutex = false) { xa =>
      val report = analyzeIO(args, xa).unsafeRunSync()
      def reportText = formatReport(args, report, Colors.Ansi).padLeft("  ").toString
      if (!report.succeeded)
        fail(reportText)
      println(reportText)
    }
}
