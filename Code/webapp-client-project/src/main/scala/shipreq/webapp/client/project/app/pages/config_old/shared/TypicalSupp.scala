package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.scalajs.react._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Px
import scalaz.Equal
import Persistence.Realise

/**
 * Functions typically useful when building on TypicalStoresAndState.
 */
trait TypicalSupp[P, I, K, U] {
  val sas: TypicalStoresAndState[P, I, K]
  val realise: Realise[sas.S]
  val crudIO: Px[CrudActionIO[P, K, U]]

  import sas._

  lazy val deletion =
    crudIO.map(c =>
      Persistence.asyncDeletionS(sas.savedRowStoreS)(c._deleteIO, realise))

  def saveNeed[B: Equal](extract: P => B) =
    SaveNeed.cmpToExtract[P, B](extract)

  def addEditorFeatures2[A, V](e: Editor[A, FS#FieldValue, CallbackTo, S, FS#Field, Callback, V]) // TODO remove
                              (saveFn: Option[K] => sas.ST,
                               ak: A => Option[K]) =
    e.applyRowUpdateAndRevert(savedRowStoreS, newRowStoreS)(ak)
      .applyOnEditFinishedK(saveFn)(ak)
}

object TypicalSupp {
  @inline def apply[P, I, K, U](_sas: TypicalStoresAndState[P, I, K])
                               (_crudIO: => CrudActionIO[P, K, U],
                                _c: StateAccessPure[_sas.S])
  : TypicalSupp[P, I, K, U] {val sas: _sas.type} =
    new TypicalSupp[P, I, K, U] {
      override val sas: _sas.type = _sas
      override val realise: Realise[sas.S] = _c.runState(_)
      override val crudIO = Px(_crudIO: AnyRef).withReuse(Reusability.byRef).autoRefresh.asInstanceOf[Px[CrudActionIO[P, K, U]]]
    }
}
