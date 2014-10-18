package shipreq.webapp.client.util.ui

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
 * [5] delete
 * [5] show/hide deleted
 * [4] validation as you type
 * [4] input correction (valid or not)
 * [3] field validity depending on other fields (in same row)
 * [3] field validity depending on other rows
 * [2] escape to cancel change
 * [2] visual indication of save-in-progress & save-complete
 * [1] avoid NOP saves
 *
 * Done in Isolation
 * ~~~~~~~~~~~~~~~~~
 * [5.5] drag to reorder
 *
 * TODO Additional TableSpec features ↙
 * ~~~~
 * [PRIORITY.EFFORT]
 * [5.?] add drag/drop ordering to table
 * [3.5] different view when field not in edit (sometimes the edit view is too noisy)
 * [2.?] handle name swap (should save both, not just one)
 * [2.2] server-side only errors / errors on save
 * [1.3] validators with composite types (like new & change password)
 * [?.?] state date structure help
 */
package object table {

  type Saved[D, P, II]        = Map[D, SavedRow[P, II]]
  type Unsaved[II]            = Option[UnsavedRow[II]]
  type SavedUnsaved[D, P, II] = (Saved[D, P, II], Unsaved[II])
}
