package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react.ScalazReact._
import monocle.Lenser
import scalaz.effect.IO

object TypicalStoresAndState {
  def apply[P, I](fields: FieldSet[P, I]) = new B(fields)
  @inline final class B[P, I](fields: FieldSet[P, I]) {
    @inline def keyedBy[K]: TypicalStoresAndState[P, I, K] = new TypicalStoresAndState(fields)
  }
}

class TypicalStoresAndState[P, I, K](fields: FieldSet[P, I]) {

  val savedRowStore = SavedRowStore.of(fields).keyedBy[K]
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
    ST.modT(State._showDeleted.modifyF(v => !v))
}
