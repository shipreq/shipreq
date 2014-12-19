package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react.ScalazReact._
import monocle.macros.Lenser
import scalaz.effect.IO

object TypicalStoresAndState {
  def apply[P, I](fields: FieldSet[P, I]) = new B[P, I, fields.type](fields)
  @inline final class B[P, I, _FS <: FieldSet[P, I]](_fields: _FS) {
    @inline def keyedBy[K]: TypicalStoresAndState[P, I, K] {type FS = _FS} =
      new TypicalStoresAndState[P, I, K](_fields) {
        override type FS = _FS
      }
  }
}

/**
 * @tparam P Persisted data. Data known to be saved.
 * @tparam I Input. A subset of P's fields in a form that matches the editor state.
 * @tparam K Key. Data ID.
 */
abstract class TypicalStoresAndState[P, I, K](fields: FieldSet[P, I]) {
  type FS <: FieldSet[P, I]

  val savedRowStore = SavedRowStore.fields(fields).keyedBy[K]
  val newRowStore   = NewRowStore.of(fields)

  case class State(newRow: newRowStore.State, savedRows: savedRowStore.State, showDeleted: Boolean)

  object State {
    private[this] def l = Lenser[State]
    val _newRow      = l(_.newRow)
    val _savedRows   = l(_.savedRows)
    val _showDeleted = l(_.showDeleted)
  }

  type S  = State
  type ST = ReactST[IO, S, Unit]
  val ST = ReactS.FixT[IO, S]

  val savedRowStoreS = savedRowStore.contramap(State._savedRows)
  val newRowStoreS   = newRowStore  .contramap(State._newRow)

  /**
   * Validators requiring external data (eg. for uniqueness checking) typically need input in this shape.
   */
  def validatorInput(k: Option[K]): S => (Stream[P], Option[K]) =
    s => (savedRowStoreS.getAllP(s), k)

  def toggleShowDeleted =
    ST.modT(State._showDeleted.modify(v => !v))
}
