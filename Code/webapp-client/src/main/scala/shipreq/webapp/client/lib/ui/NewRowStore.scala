package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react.ScalazReact._
import monocle._
import monocle.macros.Lenser
import monocle.std.option.some
import scalaz.Applicative
import shipreq.webapp.base.WebappTmp

object NewRowStore {
  final case class Row[I](status: RowStatus, i: I)

  final class RowL[I] {
    private[this] def l = Lenser[Row[I]]
    val status = l(_.status)
    val i      = l(_.i)
  }

  type SS[I] = Option[Row[I]]

  def apply[I](emptyI: I): NewRowStore[SS[I], I] =
    new NewRowStore(WebappTmp.lensId, new RowL, emptyI)

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

  // TODO annoying changing maybe -> option
  def get                    : S => Option[Row]       = _row.getMaybe(_).toOption
  def getI                   : S => Option[I]         = _i.getMaybe(_).toOption
  def getStatus              : S => Option[RowStatus] = _status.getMaybe(_).toOption
  def enableEdit             : S => S                 = s => if (editing(s)) s else _ss.set(Some(initRow))(s)
  def editing                : S => Boolean           = _ss.get(_).isDefined
  def remove                 : S => S                 = _ss.set(None)
  def setStatus(r: RowStatus): S => S                 = _status.set(r)

  def setStatusST[M[_]: Applicative]: RowStatus => ReactST[M, S, Unit] =
    rs => ReactS.modT(setStatus(rs))

  def setField[X](fv: FieldSet[X, I]#FieldValue): S => S =
    (_i ^|-> fv.f.ilens).set(fv.v)

  def setFieldST[M[_]: Applicative, X](fv: FieldSet[X, I]#FieldValue): ReactST[M, S, Unit] =
    ReactS modT setField(fv)
}