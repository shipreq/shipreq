package shipreq.webapp.client.ui.table

import monocle._
import monocle.std.option.some
import monocle.function.Field1.first
import monocle.function.Field2.second
import monocle.std.tuple2._
import monocle.syntax._

sealed trait RowStatus
object RowStatus {

  /** Row is in sync with the known (local) world. An edit may or may not be in progress. */
  case object Sync extends RowStatus

  /** Row is locked pending an external response to change. (Ajax in progress) */
  case object Locked extends RowStatus

  /** Failed to coordination Local change with external agent. (Ajax failure) */
  case object Failed extends RowStatus
}

final case class UnsavedRow[II](status: RowStatus, ii: II)

final case class SavedRow[P, II](status: RowStatus, p: P, ii: II)

final class SavedUnsavedL[S, D, P, II](val savedL: SimpleLens[S, Saved[D, P, II]],
                                       val unsavedL: SimpleLens[S, Unsaved[II]]) {

  // TODO rename ↓

  def unsavedLO =
    unsavedL composeOptional some

  def unsavedStatusL: SimpleOptional[S, RowStatus] =
    unsavedLO composeOptional SimpleLens[UnsavedRow[II]](_.status)((a, b) => a.copy(status = b))

  def rowL(id: D) =
    savedL composeLens SimpleLens[Saved[D, P, II]](_(id))((a, b) => a + (id -> b))

  def rowStatus(id: D): SimpleLens[S, RowStatus] =
    rowL(id) |-> SimpleLens[SavedRow[P, II]](_.status)((a, b) => a.copy(status = b))

  def savedIL =
    SimpleLens[SavedRow[P, II]](_.ii)((a, b) => a.copy(ii = b))

  def rowIL(id: D): SimpleLens[S, II] =
    rowL(id) |-> savedIL

  def rowP(id: D): S => P =
    savedL.get(_)(id).p

  def rowDP(id: D): S => (D, P) =
    s => (id, rowP(id)(s))
}

object SavedUnsavedL {
  def default[D, P, II] =
    new SavedUnsavedL[SavedUnsaved[D, P, II], D, P, II](first, second)
}
