package shipreq.taskman.server.app

/**
 * Performs any pending database migrations.
 */
object DatabaseMigration extends MainTemplate {

  def main(args: Array[String]): Unit =
    withDatabase(_ => ()) // Do nothing
}