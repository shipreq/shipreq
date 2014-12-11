package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, ScalazReact._
import scalaz.Equal
import scalaz.effect.IO
import shipreq.webapp.base.data.Alive
import shipreq.webapp.base.validation.Validator
import shipreq.webapp.client.lib.CrudIO
import shipreq.webapp.client.lib.ui.Persistence.Realise

/**
 * Functions typically useful when building on TypicalStoresAndState.
 */
trait TypicalSupp[P, I, K, U] {
  val sas: TypicalStoresAndState[P, I, K]
  protected val realise: Realise[sas.S]
  protected val crudIO: CrudIO[P, K, U, _]
  protected val alive: P => Alive

  import sas._

  lazy val deletion =
    Persistence.asyncDeletionS(sas.savedRowStoreS)(alive, crudIO._deleteIO, realise)

  def saveNeed[B: Equal](extract: P => B) =
    SaveNeed.cmpToExtract[P, B](extract)

  def addEditorFeatures[A, V](e: Editor[A, FS#FieldValue, IO, S, FS#Field, IO[Unit], V])
                             (vali: Validator[(Stream[P], Option[K]), I, _, U],
                              ak: A => Option[K],
                              extract: P => U)
                             (implicit E: scalaz.Equal[U]) = {

    val save = Persistence.asyncSaveT(vali, sas)(saveNeed(extract), crudIO, realise)

    e.applyRowUpdateAndRevert(savedRowStoreS, newRowStoreS)(ak)
      .applyOnEditFinishedK(save)(ak)
  }
}

object TypicalSupp {
  @inline def apply[P, I, K, U](_sas: TypicalStoresAndState[P, I, K],
                                _crudIO: CrudIO[P, K, U, _])
                               (_c: ComponentStateFocus[_sas.S],
                                _alive: P => Alive)
      : TypicalSupp[P, I, K, U] {val sas: _sas.type} =
    new TypicalSupp[P, I, K, U] {
      override val sas: _sas.type = _sas
      override protected val realise: Realise[sas.S] = _c.runState(_)
      override protected val crudIO = _crudIO
      override protected val alive = _alive
    }
}
