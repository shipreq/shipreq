package shipreq.base.test.specs2.db

import doobie.imports._
import java.util.concurrent.locks.Lock
import org.specs2.execute.{AsResult, Result}
import org.specs2.mutable.Specification
import org.specs2.specification.AroundExample
import shipreq.base.test.db._

trait DatabaseTest extends AroundExample {
  this: Specification =>

  isolated

  private[this] var _xa: Option[SingleConnectionXA] = None
  final def xa: SingleConnectionXA = _xa getOrElse sys.error("DB connection not established.")

  def dbExec[A](c: ConnectionIO[A]): A =
    xa.trans(c).unsafePerformIO()

  implicit final class ConnIoExt[A](private val self: ConnectionIO[A]) {
    def runNow(): A = self.transact(xa).unsafePerformIO()
  }

  def mutex: Option[Lock] = None

  def wrapTestsInTransaction = true

  override def around[T: AsResult](test: => T): Result =
    TestDb(wrapTestsInTransaction, mutex).runNow(xa =>
      try {
        _xa = Some(xa)
        AsResult(test)
      } finally
        _xa = None
    )
}
