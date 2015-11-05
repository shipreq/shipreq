package shipreq.webapp.client.ui

import monocle._
import monocle.function.Field1.first
import monocle.function.Field2.second
import monocle.std.tuple2._
import monocle.syntax._

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
 * II = Inputs. Tuple of (I₁,I₂,…,Iₙ).
 * VV = Views. Tuple of (V,V,V,…).
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

  type Saved[D, P, II] = Map[D, (P, II)]
  type Unsaved[II] = Option[II]
  type SavedAndUnsaved[D, P, II] = (Saved[D, P, II], Unsaved[II])

  def getSaved[D, P, II]: SavedAndUnsaved[D, P, II] => Saved[D, P, II] = _._1

  class SavedUnsavedL[S, D, P, II](val savedL: SimpleLens[S, Saved[D, P, II]],
                                   val unsavedL: SimpleLens[S, Unsaved[II]]) {
    def rowL(id: D) =
      savedL composeLens SimpleLens[Saved[D, P, II]](_(id))((a, b) => a + (id -> b))

    def rowIL(id: D) =
      rowL(id) |-> second

    def rowP(id: D): S => P =
      savedL.get(_)(id)._1

    def rowDP(id: D): S => (D, P) =
      s => (id, rowP(id)(s))
  }

  object SavedUnsavedL {
    def default[D, P, II] =
      new SavedUnsavedL[SavedAndUnsaved[D, P, II], D, P, II](first, second)
  }
}
