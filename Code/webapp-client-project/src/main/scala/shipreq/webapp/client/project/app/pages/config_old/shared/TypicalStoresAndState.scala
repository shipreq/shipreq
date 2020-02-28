package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.scalajs.react._
import japgolly.scalajs.react.ScalazReact._
import japgolly.univeq.UnivEq
import monocle.macros.Lenses

object TypicalStoresAndState {
  def apply[P, I](fields: FieldSet[P, I]) = new B[P, I, fields.type](fields)
  @inline final class B[P, I, _FS <: FieldSet[P, I]](_fields: _FS) {
    @inline def keyedBy[K: UnivEq]: TypicalStoresAndState[P, I, K] {type FS = _FS} =
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
abstract class TypicalStoresAndState[P, I, K: UnivEq](fields: FieldSet[P, I]) {
  type FS <: FieldSet[P, I]

  val savedRowStore = SavedRowStore.fields(fields).keyedBy[K]
  val newRowStore   = NewRowStore.of(fields)

  @Lenses
  case class State(newRow: newRowStore.State, savedRows: savedRowStore.State)

  final type Persisted = P
  final type Input = I
  final type Key = K
  final type S  = State
  final type ST = ReactST[CallbackTo, S, Unit]
  final val ST = ReactS.FixCB[S]

  val savedRowStoreS = savedRowStore.contramap(State.savedRows)
  val newRowStoreS   = newRowStore  .contramap(State.newRow)

  /**
   * Validators requiring external data (eg. for uniqueness checking) typically need input in this shape.
   */
  def validatorInput(k: Option[K]): S => (Stream[P], Option[K]) = // TODO Delete this kind of shit
    s => (savedRowStoreS.getAllP(s), k)
}
