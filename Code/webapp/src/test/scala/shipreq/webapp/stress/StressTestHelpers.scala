package shipreq.webapp.stress

import java.util.concurrent.atomic.AtomicLong
import scala.slick.jdbc.StaticQuery
import scala.slick.session.Session
import shipreq.webapp.test.TestDB

object StressTestHelpers {

  def time[U](msg: String)(f: => U): U = {
    val started = System.currentTimeMillis
    val u = f
    val ended = System.currentTimeMillis
    println("%s in %.1f sec.".format(msg, (ended - started).toFloat / 1000f))
    u
  }

  class IdWithProgress(msg: String, startingId: Long = 100000) {
    private val nextId = new AtomicLong(startingId)
    def get(): Long = {
      val id = nextId.getAndIncrement
      if (id != startingId && id % 10000 == 0) println(msg.format(id - startingId))
      id
    }
  }

  class ThreadLocalDb extends ThreadLocal[Session] {
    private val sessionLock = new Object
    private var sessions = List.empty[Session]
    override def initialValue() = {
      val s = TestDB.Slick.createSession()
      sessionLock.synchronized {sessions :+= s}
      s
    }
    def query[P, R](query: => StaticQuery[P, R]) = new ThreadLocalQuery[P, R](this, query)
    def closeAll: Unit = sessionLock.synchronized {
      sessions.foreach(_.close)
      sessions = List.empty
    }
  }

  class ThreadLocalQuery[P, R](db: ThreadLocalDb, queryFn: => StaticQuery[P, R]) extends ThreadLocal[StaticQuery[P, R]] {
    override def initialValue() = {
      implicit val s = db.get()
      queryFn
    }
    def execute(param: P) = get.execute(param)(db.get)
  }
}
