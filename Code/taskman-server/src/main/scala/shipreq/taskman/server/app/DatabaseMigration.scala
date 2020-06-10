package shipreq.taskman.server.app

import cats.effect.{ExitCode, IO}
import shipreq.base.util.FxModule._

/**
 * Performs any pending database migrations.
 */
object DatabaseMigration extends TaskmanApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- withDatabase((_, _) => Fx.unit)
    } yield ExitCode.Success
}