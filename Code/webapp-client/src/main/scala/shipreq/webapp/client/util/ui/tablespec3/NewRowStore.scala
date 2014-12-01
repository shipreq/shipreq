package shipreq.webapp.client.util.ui.tablespec3

import japgolly.scalajs.react.ScalazReact._
import monocle._
import monocle.std.option.some
import monocle.syntax._
import scalaz.{Applicative, Bind}
import shipreq.base.util.ScalaExt._

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

class NewRowStore[S, I](_ss: SimpleLens[S, NewRowStore.SS[I]], rowL: NewRowStore.RowL[I], emptyI: I) {
  final type State = S
  final type Row   = NewRowStore.Row[I]
  final type SS    = NewRowStore.SS[I]

  def contramap[T](f: SimpleLens[T, S]): NewRowStore[T, I] =
    new NewRowStore(f |-> _ss, rowL, emptyI)

  def initState: SS = None

  private[this] def initRow: Row =
    NewRowStore.Row(RowStatus.Sync, emptyI)

  private val _row   : SimpleOptional[S, Row]       = _ss composeOptional some
  private val _status: SimpleOptional[S, RowStatus] = _row composeOptional rowL.status
  private val _i     : SimpleOptional[S, I]         = _row composeOptional rowL.i

  def getI                   : S => Option[I] = _i.getOption
  def startEdit              : S => S         = s => if (editable(s)) s else _ss.set(s, Some(initRow))
  def editable               : S => Boolean   = _ss.get(_).isDefined
  def remove                 : S => S         = _ss.set(_, None)
  def setStatus(r: RowStatus): S => S         = _status.setF(r)

  def setStatusST[M[_]: Applicative]: RowStatus => ReactST[M, S, Unit] = rs => ReactS.modT(setStatus(rs))

  def setField[X](fv: FieldSet[X, I]#FieldValue): S => S =
    (_i composeOptional fv.f.ilens).setF(fv.v)

  def setFieldST[M[_]: Applicative, X](fv: FieldSet[X, I]#FieldValue): ReactST[M, S, Unit] = ReactS modT setField(fv)

//  def applyRowUpdate[M[_]: Bind: Applicative, A, D, V, F, FV](e: Editor[A, FV, (F, ReactST[M, S, Unit]), D, V])
//                                                             (implicit wf: F <:< FieldSet[_, I]#Field, wv: FV <:< FieldSet[_, I]#FieldValue)
//  : Editor[A, FV, (F, ReactST[M, S, Unit]), D, V] =
//    e.modCallbacks {
//      _.pmodC(c => {
//        case OnChange(v) => c map2 (_ >> ReactS.modT(setField(v)))
//      })
//    }
}
