package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react.ScalazReact._
import monocle._
import monocle.syntax._
import scalaz.effect.IO
import shipreq.base.util.ScalaExt._

object SavedRowStore {

  final case class Row[P, I](status: RowStatus, p: P, i: I)

  class RowL[P, I] {
    private[this] def l = Lenser[Row[P, I]]
    val status = l(_.status)
    val p      = l(_.p)
    val i      = l(_.i)
  }

  type SS[K, P, I] = Map[K, Row[P, I]]

  def apply[K, P, I](pi: P => I): SavedRowStore[SS[K, P, I], K, P, I] =
    new SavedRowStore(SimpleIso.dummy, new RowL, pi)

  def of[P, I](f: FieldSet[P, I]) = new {
    def keyedBy[K]: SavedRowStore[SS[K, P, I], K, P, I] = SavedRowStore(f.pi)
  }
}

class SavedRowStore[S, K, P, I](_s: SimpleLens[S, SavedRowStore.SS[K,P,I]],
                                _savedRow: SavedRowStore.RowL[P, I],
                                pi: P => I) {
  final type State = S
  final type Row = SavedRowStore.Row[P, I]
  final type SS = SavedRowStore.SS[K, P, I]
  final type KP = (K, P)

  def contramap[T](f: SimpleLens[T, S]): SavedRowStore[T, K, P, I] =
    new SavedRowStore(f |-> _s, _savedRow, pi)

  @inline private def initRow(p: P): Row =
    SavedRowStore.Row(RowStatus.Sync, p, pi(p))

  private def __row  (k: K): SimpleLens[SS, Row]      = SimpleLens[SS](_ apply k)((m, p) => m + (k -> p))
  private def _row   (k: K): SimpleLens[S, Row]       = _s |-> __row(k)
  private def _status(k: K): SimpleLens[S, RowStatus] = _row(k) |-> _savedRow.status
  private def _i     (k: K): SimpleLens[S, I]         = _row(k) |-> _savedRow.i
  private def _p     (k: K): SimpleLens[S, P]         = _row(k) |-> _savedRow.p

  def initStateM(s: Map[K, P])         : SS = s mapValues initRow
  def initStateS(s: Seq[P], pk: P => K): SS = initStateM(s.foldLeft(Map.empty[K, P])((m, p) => m + p.mapStrengthL(pk)))
  def initStateT(s: Seq[KP])           : SS = initStateM(s.toMap)

  def remove   (k: K)              : S => S = _s.modifyF(_ - k)
  def set      (k: K, p: P)        : S => S = _s.modifyF(_ + (k -> initRow(p)))
  def setT     (kp: KP)            : S => S = set(kp._1, kp._2)
  def setStatus(k: K, r: RowStatus): S => S = _status(k).setF(r)

  def setStatusS(k: K): RowStatus => ReactS[S, Unit] = r => ReactS.mod(setStatus(k, r))

  def getP(k: K): S => P = _p(k).get
  def getI(k: K): S => I = _i(k).get

  def getAll(s: S): Stream[(RowStatus, K, P)] =
    _s.get(s).toStream.map(x => (x._2.status, x._1, x._2.p))

  def setField[X](k: K, fv: FieldSet[X, I]#FieldValue): S => S =
    (_i(k) |-> fv.f.ilens).setF(fv.v)

  def revertField(k: K, f: FieldSet[P, I]#Field): S => S =
    s => setField(k, f * f.pv(_p(k) get s))(s)

  //    private[this] implicit def autoLiftEndo(f: S => S): ReactS[S, Unit] = ReactS mod f
  //    def savedRemoveR(k: K): ReactS[S, Unit] = savedRemoveF(k)
  //    def savedSetR   (kp: KP)    : ReactS[S, Unit] = savedSetF(kp)

  // these two below are synchronous
//  def updateIO(saveIO: KP => IO[KP]): K => ReactST[IO, S, Unit] =
//    k => ReactS.modT[IO, S](s =>
//      saveIO(k, _row(k).get(s).p)
//        .map(setT(_)(s)))
//
//  def deleteIO(delIO: K => IO[Unit]): K => ReactST[IO, S, Unit] =
//    k => ReactS.retM(delIO(k)) >> ReactS.mod(remove(k)).lift[IO] // TODO revisit after 0.6.0
}