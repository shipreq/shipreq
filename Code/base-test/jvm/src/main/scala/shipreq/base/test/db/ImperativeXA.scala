package shipreq.base.test.db

import cats.effect.Resource
import doobie._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.db._
import shipreq.base.util.FxModule._

final class ImperativeXA(val xa: XA, realDb: DbAccessor, lazyTables: () => Set[DbTable]) extends TestDbHelpers {

  override def tables: Set[DbTable] =
    lazyTables()

  lazy val dbAccessor: DbAccessor =
    DbAccessor(
      realDb.config,
      realDb.dataSource, // hmmm...
      Resource.pure(xa),
      realDb.migrator)

  override def ![A](c: ConnectionIO[A]): A =
    try
      xa(c).unsafeRun()
    catch {
      case t: Throwable =>
        val stackTrace = t.stackTraceAsStringWithLineMod {
          case s if s contains "shipreq" => Console.BOLD + Console.MAGENTA + s + Console.RESET
        }
        System.err.println(stackTrace)
        throw t
    }
}

object ImperativeXA {

  implicit def toXa(xa: ImperativeXA): XA =
    xa.xa

}