package shipreq.webapp.client.util.ui.tablespec2

import scalaz.Bind
import scalaz.effect.IO

sealed trait RowStatus
object RowStatus {

  /** Row is in sync with the known (local) world. An edit may or may not be in progress. */
  case object Sync extends RowStatus

  /** Row is locked pending an external response to change. (Ajax in progress) */
  case object Locked extends RowStatus

  /** Failed to coordination Local change with external agent. (Ajax failure) */
  case class Failed(retry: IO[Unit]) extends RowStatus
  object Failed {
    def lazily(f: => IO[Unit]): Failed = Failed(Bind[IO].join(IO(f)))
  }
}
