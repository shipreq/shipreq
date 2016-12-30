package shipreq.taskman.server.app

import scalaz.effect.IO

/**
 * Performs any pending database migrations.
 */
object DatabaseMigration extends MainTemplate {

  def main(args: Array[String]): Unit =
    withDatabase(_ => IO.ioUnit).unsafePerformIO()
}