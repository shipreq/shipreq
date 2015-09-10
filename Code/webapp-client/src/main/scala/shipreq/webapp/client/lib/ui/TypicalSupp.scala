package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, ScalazReact._
import japgolly.scalajs.react.extra.{Reusability, Px}
import scalaz.Equal
import shipreq.webapp.base.data.Live
import shipreq.webapp.base.protocol.CrudFn
import shipreq.webapp.base.validation.Validator
import shipreq.webapp.client.lib.CrudIO
import shipreq.webapp.client.lib.ui.Persistence.Realise

/**
 * Functions typically useful when building on TypicalStoresAndState.
 */
trait TypicalSupp[P, I, K, U] {
  val sas: TypicalStoresAndState[P, I, K]
  protected val realise: Realise[sas.S]
  protected val crudIO: Px[CrudIO[P, K, U, _]]

  import sas._

  lazy val deletion =
    crudIO.map(c =>
      Persistence.asyncDeletionS(sas.savedRowStoreS)(c._deleteIO, realise))

  def saveNeed[B: Equal](extract: P => B) =
    SaveNeed.cmpToExtract[P, B](extract)

  def addEditorFeatures[A, V](e: Editor[A, FS#FieldValue, CallbackTo, S, FS#Field, Callback, V])
                             (vali: Validator[(Stream[P], Option[K]), I, _, U],
                              ak: A => Option[K],
                              extract: P => U)
                             (implicit E: scalaz.Equal[U]) = {

    val save = crudIO.map(c => Persistence.asyncSaveT(vali, sas)(saveNeed(extract), c, realise)).extract

    e.applyRowUpdateAndRevert(savedRowStoreS, newRowStoreS)(ak)
      .applyOnEditFinishedK(save)(ak)
  }

  def addEditorFeatures2[A, V](e: Editor[A, FS#FieldValue, CallbackTo, S, FS#Field, Callback, V])
                              (saveFn: Option[K] => sas.ST,
                               ak: A => Option[K]) =
    e.applyRowUpdateAndRevert(savedRowStoreS, newRowStoreS)(ak)
      .applyOnEditFinishedK(saveFn)(ak)
}

object TypicalSupp {
  @inline def apply[P, I, K, U](_sas: TypicalStoresAndState[P, I, K])
                               (_crudIO: => CrudIO[P, K, U, _],
                                _c: StateAccessCB[_sas.S])
  : TypicalSupp[P, I, K, U] {val sas: _sas.type} =
    new TypicalSupp[P, I, K, U] {
      override val sas: _sas.type = _sas
      override protected val realise: Realise[sas.S] = _c.runState(_)
      override protected val crudIO = Px.thunkA(_crudIO: AnyRef)(Reusability.byRef).asInstanceOf[Px[CrudIO[P, K, U, _]]]
    }
}
