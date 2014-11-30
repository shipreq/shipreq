package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react.ScalazReact._
import monocle._
import monocle.syntax._
import scalaz.{Bind, Applicative}
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

class SavedRowStore[S, K, P, I](_ss: SimpleLens[S, SavedRowStore.SS[K,P,I]],
                                rowL: SavedRowStore.RowL[P, I],
                                pi: P => I) {
  final type State = S
  final type Row   = SavedRowStore.Row[P, I]
  final type SS    = SavedRowStore.SS[K, P, I]
  final type KP    = (K, P)

  def contramap[T](f: SimpleLens[T, S]): SavedRowStore[T, K, P, I] =
    new SavedRowStore(f |-> _ss, rowL, pi)

  @inline private def initRow(p: P): Row =
    SavedRowStore.Row(RowStatus.Sync, p, pi(p))

  def initStateM(s: Map[K, P])         : SS = s mapValues initRow
  def initStateS(s: Seq[P], pk: P => K): SS = initStateM(s.foldLeft(Map.empty[K, P])((m, p) => m + p.mapStrengthL(pk)))
  def initStateT(s: Seq[KP])           : SS = initStateM(s.toMap)

  private def __row  (k: K): SimpleLens[SS, Row]      = SimpleLens[SS](_ apply k)((m, p) => m + (k -> p))
  private def _row   (k: K): SimpleLens[S, Row]       = _ss |-> __row(k)
  private def _status(k: K): SimpleLens[S, RowStatus] = _row(k) |-> rowL.status
  private def _i     (k: K): SimpleLens[S, I]         = _row(k) |-> rowL.i
  private def _p     (k: K): SimpleLens[S, P]         = _row(k) |-> rowL.p

  def remove   (k: K)              : S => S = _ss.modifyF(_ - k)
  def set      (k: K, p: P)        : S => S = _ss.modifyF(_ + (k -> initRow(p)))
  def setT     (kp: KP)            : S => S = set(kp._1, kp._2)
  def setStatus(k: K, r: RowStatus): S => S = _status(k).setF(r)

//  def setStatusS(k: K, r: RowStatus): ReactS[S, Unit] = ReactS.mod(setStatus(k, r))
def setStatusST[M[_]: Applicative](k: K): RowStatus => ReactST[M, S, Unit] = rs => ReactS.modT(setStatus(k, rs))

  def getP(k: K): S => P = _p(k).get
  def getI(k: K): S => I = _i(k).get

  def getAllP(s: S): Stream[P] =
    _ss.get(s).toStream.map(_._2.p)

  def getAll(s: S): Stream[(RowStatus, K, P)] =
    _ss.get(s).toStream.map(x => (x._2.status, x._1, x._2.p))

  def setField[X](k: K, fv: FieldSet[X, I]#FieldValue): S => S =
    (_i(k) |-> fv.f.ilens).setF(fv.v)

  def revertField(k: K, f: FieldSet[P, I]#Field): S => S =
    s => setField(k, f * f.pv(_p(k) get s))(s)

  def setFieldST[M[_]: Applicative, X](k: K, fv: FieldSet[X, I]#FieldValue): ReactST[M, S, Unit] = ReactS modT setField(k, fv)
  def revertFieldST[M[_]: Applicative](k: K, f: FieldSet[P, I]#Field): ReactST[M, S, Unit] = ReactS modT revertField(k, f)

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

//  def applyRowUpdateAndRevert[M[_]: Bind: Applicative, A, D, V, F, FV]
//  (e: Editor[A, FV, (F, ReactST[M, S, Unit]), D, V])(k: A => K)(implicit wf: F <:< FieldSet[P, I]#Field, wv: FV <:< FieldSet[P, I]#FieldValue)
//    : Editor[A, FV, (F, ReactST[M, S, Unit]), D, V] =
//    e.modCallbacksA(a => {
//      val id = k(a)
//      _.pmodC(c => {
//        case OnChange(v) => c map2 (_ >> ReactS.modT(setField(id, v)))
//        case OnCancel    => c map2 (_ >> ReactS.modT(revertField(id, c._1)))
//      })
//    })
//
//  def applyRowUpdateAndRevertO[M[_]: Bind: Applicative, A, D, V, F, FV]
//  (e: Editor[A, FV, (F, ReactST[M, S, Unit]), D, V])(k: A => Option[K])(implicit wf: F <:< FieldSet[P, I]#Field, wv: FV <:< FieldSet[P, I]#FieldValue)
//    : Editor[A, FV, (F, ReactST[M, S, Unit]), D, V] =
//    e.modCallbacksA(a => {
//      k(a) match {
//        case None =>
//          identity
//        case Some(id) =>
//          _.pmodC(c => {
//            case OnChange(v) => c map2 (_ >> ReactS.modT(setField(id, v)))
//            case OnCancel    => c map2 (_ >> ReactS.modT(revertField(id, c._1)))
//          })
//      }
//    })
}