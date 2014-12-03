package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react.ScalazReact._
import monocle._
import monocle.std.option.some
import monocle.syntax._
import scalaz.Applicative

object NewRowStore {
  final case class Row[I](status: RowStatus, i: I)

  class RowL[I] {
    private[this] def l = Lenser[Row[I]]
    val status = l(_.status)
    val i      = l(_.i)
  }

  type SS[I] = Option[Row[I]]

  def apply[I](emptyI: I): NewRowStore[SS[I], I] =
    new NewRowStore(SimpleIso.dummy, new RowL, emptyI)

  def of[I](f: FieldSet[_, I]): NewRowStore[SS[I], I] =
    NewRowStore(f.emptyI)
}

final class NewRowStore[S, I](_ss: SimpleLens[S, NewRowStore.SS[I]], rowL: NewRowStore.RowL[I], emptyI: I) {
  type State = S
  type Row   = NewRowStore.Row[I]
  type SS    = NewRowStore.SS[I]

  def contramap[T](f: SimpleLens[T, S]): NewRowStore[T, I] =
    new NewRowStore(f |-> _ss, rowL, emptyI)

  def initState: SS = None

  private[this] def initRow: Row =
    NewRowStore.Row(RowStatus.Sync, emptyI)

  private[this] val _row   : SimpleOptional[S, Row]       = _ss composeOptional some
  private[this] val _status: SimpleOptional[S, RowStatus] = _row composeOptional rowL.status
  private[this] val _i     : SimpleOptional[S, I]         = _row composeOptional rowL.i

  def get                    : S => Option[Row]       = _row.getOption
  def getI                   : S => Option[I]         = _i.getOption
  def getStatus              : S => Option[RowStatus] = _status.getOption
  def enableEdit             : S => S                 = s => if (editing(s)) s else _ss.set(s, Some(initRow))
  def editing                : S => Boolean           = _ss.get(_).isDefined
  def remove                 : S => S                 = _ss.set(_, None)
  def setStatus(r: RowStatus): S => S                 = _status.setF(r)

  def setStatusST[M[_]: Applicative]: RowStatus => ReactST[M, S, Unit] = rs => ReactS.modT(setStatus(rs))

  def setField[X](fv: FieldSet[X, I]#FieldValue): S => S =
    (_i composeOptional fv.f.ilens).setF(fv.v)

  def setFieldST[M[_]: Applicative, X](fv: FieldSet[X, I]#FieldValue): ReactST[M, S, Unit] = ReactS modT setField(fv)
}