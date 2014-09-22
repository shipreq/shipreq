package shipreq.webapp.client.ui

import monocle._
import monocle.function.Field1.first
import monocle.function.Field2.second
import monocle.std.tuple2._

/**
 * Types
 * ~~~~~
 * S  = State. Type containing all table data.
 * V  = View. Type of the DOM representation.
 * Iₙ = Input #n. Type of fieldₙ's data as it comes in from the UI.
 * Cₙ = Corrected input #n. Type of fieldₙ's data after being corrected (pre-processed).
 * Oₙ = Output #n. Type of fieldₙ's data after being successfully validated.
 * U  = Updated. Type of data once all of its fields have passed validation but before it's been saved.
 * P  = Persisted. The last saved copy of the row.
 * D  = Data ID. Type used to identity persisted data.
 * R  = Row ID. Type used to identity rows in the table (where not all rows are persisted).
 *
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

  type Saved[D, P, I] = Map[D, (P, I)]
  type Unsaved[I] = Option[I]
  type SavedAndUnsaved[D, P, I] = (Saved[D, P, I], Unsaved[I])

  def getSaved[D, P, I]: SavedAndUnsaved[D, P, I] => Saved[D, P, I] = _._1

  case class SavedUnsavedL[S, D, P, I](savedL: SimpleLens[S, Saved[D, P, I]],
                                       unsavedL: SimpleLens[S, Unsaved[I]])

  object SavedUnsavedL {
    def default[D, P, I] =
      SavedUnsavedL[SavedAndUnsaved[D, P, I], D, P, I](first, second)
  }
}
