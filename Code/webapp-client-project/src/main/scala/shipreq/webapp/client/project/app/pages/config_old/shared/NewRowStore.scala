package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.scalajs.react.ScalazReact._
import monocle._
import monocle.macros.GenLens
import monocle.std.option.some
import scalaz.Monad

object NewRowStore {
  final case class Row[I](status: RowStatus, i: I)

  final class RowL[I] {
    val status = GenLens[Row[I]](_.status)
    val i      = GenLens[Row[I]](_.i)
  }

  type SS[I] = Option[Row[I]]

  def apply[I](emptyI: I): NewRowStore[SS[I], I] =
    new NewRowStore(Iso.id.asLens, new RowL, emptyI)

  def of[I](f: FieldSet[_, I]): NewRowStore[SS[I], I] =
    NewRowStore(f.emptyI)
}

/**
 * @tparam S State.
 * @tparam I Input. A subset of the subject datum's fields in a form that matches the editor state.
 */
final class NewRowStore[S, I](_ss: Lens[S, NewRowStore.SS[I]], rowL: NewRowStore.RowL[I], emptyI: I) {
  type State = S
  type Row   = NewRowStore.Row[I]
  type SS    = NewRowStore.SS[I]

  def contramap[T](f: Lens[T, S]): NewRowStore[T, I] =
    new NewRowStore(f ^|-> _ss, rowL, emptyI)

  def initState: SS = None

  private[this] def initRow: Row =
    NewRowStore.Row(RowStatus.Sync, emptyI)

  private[this] val _row   : Optional[S, Row]       = _ss  ^<-? some
  private[this] val _status: Optional[S, RowStatus] = _row ^|-> rowL.status
  private[this] val _i     : Optional[S, I]         = _row ^|-> rowL.i

  def get                    : S => Option[Row]       = _row.getOption
  def getI                   : S => Option[I]         = _i.getOption
  def getStatus              : S => Option[RowStatus] = _status.getOption
  def enableEdit             : S => S                 = s => if (editing(s)) s else _ss.set(Some(initRow))(s)
  def editing                : S => Boolean           = _ss.get(_).isDefined
  def remove                 : S => S                 = _ss.set(None)
  def setStatus(r: RowStatus): S => S                 = _status.set(r)

  def setStatusST[M[_]: Monad]: RowStatus => ReactST[M, S, Unit] =
    rs => ReactS.modT(setStatus(rs))

  def setField[X](fv: FieldSet[X, I]#FieldValue): S => S =
    (_i ^|-> fv.f.ilens).set(fv.v)

  def setFieldST[M[_]: Monad, X](fv: FieldSet[X, I]#FieldValue): ReactST[M, S, Unit] =
    ReactS modT setField(fv)
}