package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.scalajs.react.Callback

sealed trait RowStatus
object RowStatus {

  /** Row is in sync with the known (local) world. An edit may or may not be in progress. */
  case object Sync extends RowStatus

  /** Row is locked pending an external response to change. (Ajax in progress) */
  case object Locked extends RowStatus

  /** Failed to coordination Local change with external agent. (Ajax failure) */
  case class Failed(retry: Callback) extends RowStatus
  object Failed {
    def lazily(f: => Callback): Failed = Failed(Callback lazily f)
  }
}
