package shipreq.webapp.client.project.app.cfg.shared

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.ScalazReact._
import japgolly.univeq._
import monocle._
import monocle.macros.GenLens
import scalaz.Monad
import shipreq.base.util.IMap
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data.DataIdAux

object SavedRowStore {
  final case class Row[P, I](status: RowStatus, p: P, i: I)

  final class RowL[P, I] {
    val status = GenLens[Row[P, I]](_.status)
    val p      = GenLens[Row[P, I]](_.p)
    val i      = GenLens[Row[P, I]](_.i)
  }

  type SS[K, P, I] = Map[K, Row[P, I]]

  def apply[K: UnivEq, P, I](pi: P => I): SavedRowStore[SS[K, P, I], K, P, I] =
    new SavedRowStore(Iso.id.asLens, new RowL, pi)

  def data[P] = new BD[P]
  @inline final class BD[P] {
    @inline def apply[K, I](pi: P => I)(implicit D: DataIdAux[P, K], ev: UnivEq[K]): SavedRowStore[SS[K, P, I], K, P, I] = {
      val _ = D // used to type-infer K
      SavedRowStore(pi)
    }
  }

  def fields[P, I](f: FieldSet[P, I]) = new BF(f)
  @inline final class BF[P, I](f: FieldSet[P, I]) {
    @inline def keyedBy[K: UnivEq]: SavedRowStore[SS[K, P, I], K, P, I] = SavedRowStore(f.pi)
  }
}

/**
 * @tparam S State.
 * @tparam K Key. Data ID.
 * @tparam P Persisted data. Data known to be saved.
 * @tparam I Input. A subset of P's fields in a form that matches the editor state.
 */
final class SavedRowStore[S, K: UnivEq, P, I](_ss: Lens[S, SavedRowStore.SS[K, P, I]],
                                             rowL: SavedRowStore.RowL[P, I],
                                             pi: P => I) {
  type State = S
  type Row   = SavedRowStore.Row[P, I]
  type SS    = SavedRowStore.SS[K, P, I]
  type KP    = (K, P)

  def contramap[T](f: Lens[T, S]): SavedRowStore[T, K, P, I] =
    new SavedRowStore(f ^|-> _ss, rowL, pi)

  @inline private def initRow(p: P): Row =
    SavedRowStore.Row(RowStatus.Sync, p, pi(p))

          def initStateM(s: Map[K, P])         : SS = s mapValuesNow initRow
          def initStateS(s: Seq[P], pk: P => K): SS = initStateM(s.foldLeft(UnivEq.emptyMap[K, P])(_ + _.mapStrengthL(pk)))
  @inline def initStateT (s: Seq[KP])          : SS = initStateM(s.toMap)
  @inline def initStateIM(s: IMap[K, P])       : SS = initStateM(s.underlyingMap)

  private def __row  (k: K): Lens[SS, Row]      = Lens((_: SS) apply k)(p => _ + (k -> p))
  private def _row   (k: K): Lens[S, Row]       = _ss ^|-> __row(k)
  private def _status(k: K): Lens[S, RowStatus] = _row(k) ^|-> rowL.status
  private def _i     (k: K): Lens[S, I]         = _row(k) ^|-> rowL.i
  private def _p     (k: K): Lens[S, P]         = _row(k) ^|-> rowL.p

  def getAll    (s: S)              : Stream[Row]      = _ss.get(s).values.toStream
  def getAllP   (s: S)              : Stream[P]        = _ss.get(s).toStream.map(_._2.p)
  def getO      (k: K)              : S => Option[Row] = _ss.get(_).get(k)
  def get       (k: K)              : S => Row         = _row(k).get
  def getP      (k: K)              : S => P           = _p(k).get
  def getI      (k: K)              : S => I           = _i(k).get
  def getStatus (k: K)              : S => RowStatus   = _status(k).get
  def remove    (k: K)              : S => S           = _ss.modify(_ - k)
  def set       (k: K, p: P)        : S => S           = _ss.modify(_ + (k -> initRow(p)))
  def setT      (kp: KP)            : S => S           = set(kp._1, kp._2)
  def setI      (k: K, i: I)        : S => S           = _i(k).set(i)
  def setStatus (k: K, r: RowStatus): S => S           = _status(k).set(r)

  def setIST[M[_] : Monad](k: K, i: I): ReactST[M, S, Unit] =
    ReactS modT setI(k, i)

  def setStatusST[M[_]: Monad](k: K): RowStatus => ReactST[M, S, Unit] =
    rs => ReactS.modT(setStatus(k, rs))

  def setField[X](k: K, fv: FieldSet[X, I]#FieldValue): S => S =
    (_i(k) ^|-> fv.f.ilens).set(fv.v)

  def setFieldST[M[_]: Monad, X](k: K, fv: FieldSet[X, I]#FieldValue): ReactST[M, S, Unit] =
    ReactS modT setField(k, fv)

  def revertField(k: K, f: FieldSet[P, I]#Field): S => S =
    s => setField(k, f * f.pv(_p(k) get s))(s)

  def revertFieldST[M[_]: Monad](k: K, f: FieldSet[P, I]#Field): ReactST[M, S, Unit] =
    ReactS modT revertField(k, f)
}