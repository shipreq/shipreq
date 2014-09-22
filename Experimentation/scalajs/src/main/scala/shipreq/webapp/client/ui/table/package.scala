package shipreq.webapp.client.ui

import monocle._
import monocle.function.Field1.first
import monocle.function.Field2.second
import monocle.std.tuple2._

/**
 * Done
 * ~~~~
 * [5] create new
 * [5] saves only when entire row is valid
 * [4] validation as you type
 * [4] input correction (valid or not)
 * [3] field validity depending on other fields (in same row)
 * [3] field validity depending on other rows
 * [2] escape to cancel change
 *
 * Done in Isolation
 * ~~~~~~~~~~~~~~~~~
 * [5.5] drag to reorder
 * [5.3] delete
 * [5.2] show/hide deleted
 *
 * TODO
 * ~~~~
 * [PRIORITY.EFFORT]
 * [3.?] handle name swap (should save both, not just one)
 * [3.5] different view when field not in edit (sometimes the edit view is too noisy)
 * [2.3] visual indication of save-in-progress & save-complete
 * [2.2] server-side only errors / errors on save
 * [1.3] validators with composite types (like new & change password)
 * [?.?] avoid NOP saves
 * [?.?] state date structure help
 * [?.?] add drag/drop ordering to table
 */
package object table {

  type Saved[DataId, P, I] = Map[DataId, (P, I)]
  type Unsaved[I] = Option[I]
  type SavedAndUnsaved[DataId, P, I] = (Saved[DataId, P, I], Unsaved[I])

  case class SavedUnsavedL[S, DataId, P, I](savedL: SimpleLens[S, Saved[DataId, P, I]],
                                            unsavedL: SimpleLens[S, Unsaved[I]])

  object SavedUnsavedL {
    def default[DataId, P, I] =
      SavedUnsavedL[SavedAndUnsaved[DataId, P, I], DataId, P, I](first, second)
  }

}
