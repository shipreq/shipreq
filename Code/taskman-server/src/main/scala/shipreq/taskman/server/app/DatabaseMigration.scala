package shipreq.taskman.server.app

import shipreq.base.util.FxModule._

/**
 * Performs any pending database migrations.
 */
object DatabaseMigration extends MainTemplate {

  def main(args: Array[String]): Unit =
    withDatabase(_ => Fx.unit).unsafeRun()
}