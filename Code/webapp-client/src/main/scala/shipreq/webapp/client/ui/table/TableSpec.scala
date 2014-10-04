package shipreq.webapp.client.ui.table

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import monocle._
import shipreq.webapp.client.protocol.FailureIO
import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.client.ui._
import scalaz.{Equal, Bind}
import scalaz.Scalaz.Id
import scalaz.effect.IO
import scalaz.std.option._
import scalaz.syntax.bind._

trait RowRenderer[S, U, P, II, VV] {
  final def render(iL: SimpleLens[S, II], s2p: S => P) = renderM[Id](WeirdLens from iL, s2p) _

  def renderM[M[_] : Bind : Optional2](iL: WeirdLens[M, S, S, II], s2mp: S => M[P])(save: (S, U) => IO[S]): ComponentStateFocus[S] => M[VV]
}

// =====================================================================================================================

final class TableSpecB[S, D, U, P, II, VV](val p2ii: P => II,
                                           val rowRenderer: Option[D] => RowRenderer[S, U, P, II, VV],
                                           val savedUnsaved: SavedUnsavedL[S, D, P, II],
                                           val initialState: Seq[(D, P)] => S) {

  def saveNotNeededWhen(f: (U, P) => Boolean) =
    new B2(f)

  def saveNotNeededWhenE(f: P => U)(implicit U: Equal[U]) =
    saveNotNeededWhen((u,p) => U.equal(u, f(p)))

  def saveNotNeededWhenI(implicit ev: II =:= U, U: Equal[U]) =
    saveNotNeededWhenE(ev compose p2ii)

  def saveNotNeededWhenP(implicit ev: P =:= U, U: Equal[U]) =
    saveNotNeededWhenE(ev)

  class B2(saveNotNeeded: (U, P) => Boolean) {

    def syncSave(saveIO: (Option[(D, P)], U) => IO[(D, P)]) =
      new TableSpec.SyncSave(TableSpecB.this, saveNotNeeded, saveIO)

    def syncSaveP(id: P => D, saveIO: (Option[P], U) => IO[P]) =
      syncSave((odp, o) => saveIO(odp.map(_._2), o).map(p => (id(p), p)))

    def asyncSave[X](saveIO: (X, Option[(D, P)], U, FailureIO) => IO[Unit]) =
      new TableSpec.AsyncSave(TableSpecB.this, saveNotNeeded, saveIO)

    def asyncSaveP[X](id: P => D, saveIO: (X, Option[P], U, FailureIO) => IO[Unit]) =
      asyncSave[X]((x, odp, o, f) => saveIO(x, odp.map(_._2), o, f))
  }
}

object TableSpecB {

  def default[D, G, P, II, VV](spec: RowSpec[SavedAndUnsaved[D, P, II], Option[D], G, P, II, VV]) = {
    val init = spec.initial _
    val initialState: Seq[(D, P)] => spec.S =
      xs => (xs.map(x => x._1 ->(x._2, init(x._2))).toMap, None)
    new TableSpecB[spec.S, D, G, P, II, VV](init, spec.forRow, SavedUnsavedL.default, initialState)
  }
}

// =====================================================================================================================

import TableSpec._

abstract class TableSpec[X, S, D, U, P, II, VV](tsb: TableSpecB[S, D, U, P, II, VV], saveNotNeeded: (U, P) => Boolean) {
  import tsb.{p2ii, rowRenderer}
  import tsb.savedUnsaved._

  @inline final private def ST = ReactS.Fix[S]

  final type DP = (D, P)

  def mkPI(p: P): (P,II) = (p, p2ii(p))

  def initialState(d: Seq[P], id: P => D): S = tsb.initialState(d.map(x => id(x) -> x))
  def initialState(d: Map[D, P])         : S = tsb.initialState(d.toSeq)
  def initialState(d: Seq[(D, P)])       : S = tsb.initialState(d)

  // ----------------------------------------------------------------------
  // Unsaved

  private def renderAttrForUnsaved(saveIO: ComponentStateFocus[S] => (S, U) => IO[S]) = {
    val s2op: S => Option[P] = _ => None
    def setI(s: S, i: II): Option[S] = unsavedL.get(s).map(_ => unsavedL.set(s, Some(i)))
    val se = WeirdLens[Option, S, S, II](unsavedL.get, setI)
    (T: ComponentStateFocus[S]) => {
      val r = rowRenderer(None).renderM(se, s2op)(saveIO(T))
      r(T)
    }
  }

  def createUnsaved(empty: II) = ReactS.mod(unsavedL.modifyF(_ orElse Some(empty)))

  val removeUnsaved = unsavedL setF None
  val removeUnsavedS = ReactS.mod(removeUnsaved)
  //        val cancelUnsaved = scalaz.State.modify[S](unsavedL setF None)

  def insertUnsaved(px: DP)          : S => S = insertUnsaved(px._1, px._2)
  def insertUnsaved(id: D, p: P): S => S = updateSaved(id, p) compose removeUnsaved

  protected def createIO: X => ComponentStateFocus[S] => (S, U) => IO[S]

  def unsavedRow[V2](renderRow: (ComponentStateFocus[S], VV) => V2)(implicit x: X) =
    _unsavedRow(renderAttrForUnsaved(createIO(x)), renderRow)

  private def _unsavedRow[V2](renderAttr: ComponentStateFocus[S] => Option[VV], renderRow: (ComponentStateFocus[S], VV) => V2) =
    new FullRow[Option, S, VV, V2, Unit](_ => renderAttr(_), (T,_,vv) => renderRow(T, vv))

  // ----------------------------------------------------------------------
  // Saved

  protected def updateIO(T: ComponentStateFocus[S], x: X, id: D) =
    updateIfNeeded[S, U, DP, DP](
      s => (id, rowP(id)(s)),
      (dp, u) => if (saveNotNeeded(u, dp._2)) None else Some(dp)
    )(updateIO2(T, x))

  protected def updateIO2: (ComponentStateFocus[S], X) => (S, DP, U) => IO[S]

  private def renderAttrForSaved(x: X)(id: D) =
    (T: ComponentStateFocus[S]) => {
      val r = rowRenderer(Some(id)).render(rowIL(id), rowP(id))(updateIO(T, x, id))
      r(T)
    }


  def removeSaved(id: D) = savedL.modifyF(m => m - id)
  def removeSavedS(id: D) = ReactS.mod(removeSaved(id))

  def deleteSavedS(f: D => IO[Unit]): D => ReactST[IO, S, Unit] =
    id => ReactS.retM(f(id)) >> removeSavedS(id)

  def cancelChangesS(id: D) = ST.mod(s => rowIL(id).set(s, p2ii(rowP(id)(s))))

  def updateSavedSIO(modAndSaveIO: DP => IO[DP]): D => ReactST[IO, S, Unit] = id => {
    val modsaveS: ReactST[IO, S, DP] = ST.gets(rowDP(id)).lift[IO].flatMap(px1 => ST.retM(modAndSaveIO(px1)))
    //        .map(px2 => updateSaved(px2)(s1))
    //      ST.gets(rowPx(id)).flatMap()

    //      ReactS.modT[IO, S](s1 => {
    //        val px: Px = rowPx(id)(s1)
    //        modAndSaveIO(px).map(updateSaved(_)(s1))
    //      })
    //cancelChangesS(id)
    modsaveS flatMap updateSavedS
  }

  def updateSaved(dp: DP)     : S => S = updateSaved(dp._1, dp._2)
  def updateSaved(id: D, p: P): S => S = savedL.modifyF(_ + (id -> mkPI(p)))

  def updateSavedS(px: DP) = ST.mod(updateSaved(px))

  def savedRowP[V2](renderRow: (ComponentStateFocus[S], D, P, VV) => V2)(implicit x: X) =
    _savedRow(renderAttrForSaved(x), (t,i,v) => renderRow(t,i,rowL(i).get(t.state)._1,v))

  def savedRow[V2](renderRow: (ComponentStateFocus[S], D, VV) => V2)(implicit x: X) =
    _savedRow(renderAttrForSaved(x), renderRow)

  private def _savedRow[V2](renderAttr: D => ComponentStateFocus[S] => VV, renderRow: (ComponentStateFocus[S], D, VV) => V2) =
    new FullRow[Id, S, VV, V2, D](renderAttr, renderRow)

  type SavedPs = Stream[(D, P)]
  def renderSaved(T: ComponentStateFocus[S], r: FullRow[Id, S, VV, Tag, D])(f: SavedPs => SavedPs) = {
    val rr = r.render(T)
    f(getSaved(T)).map(x => rr(x._1)).toJsArray
  }

  def getSaved(T: ComponentStateFocus[S]): SavedPs =
    savedL.get(T.state).toStream.map(x => x._1 -> x._2._1)
}

// =====================================================================================================================

object TableSpec {

  implicit object NoX

  // TODO pass in overrides as fns instead of using inheritence?
  final class SyncSave[S, D, U, P, II, VV](
      tsb:           TableSpecB[S, D, U, P, II, VV],
      saveNotNeeded: (U, P) => Boolean,
      saveIO:        (Option[(D, P)], U) => IO[(D, P)])
      extends TableSpec[NoX.type, S, D, U, P, II, VV](tsb, saveNotNeeded) {

    override protected def createIO = _ => _ => (s, u) =>
      saveIO(None, u)
        .map(insertUnsaved(_)(s))

    override protected def updateIO2 = (_, _) => (s, dp, u) =>
      saveIO(Some(dp), u)
        .map(updateSaved(_)(s))
  }

  // =====================================================================================================================

  final class AsyncSave[X, S, D, U, P, II, VV](
      tsb:           TableSpecB[S, D, U, P, II, VV],
      saveNotNeeded: (U, P) => Boolean,
      saveIO:        (X, Option[(D, P)], U, FailureIO) => IO[Unit])
      extends TableSpec[X, S, D, U, P, II, VV](tsb, saveNotNeeded) {

    override protected def createIO = x => T => (s, u) =>
      saveIO(x, None, u, failureIO(T, None))
        .map(_ => setStatusToEffectInProgress(None)(s))

    override protected def updateIO2 = (T, x) => (s, dp, u) => {
      val row = Some(dp._1)
      saveIO(x, Some(dp), u, failureIO(T, row))
        .map(_ => setStatusToEffectInProgress(row)(s))
    }

    def setStatusToEffectInProgress(row: Option[D]): S => S = s => {println(s"$row → in-progress");s} // TODO
    def setStatusToRetry(row: Option[D]): S => S = s => {println(s"$row → retry");s}// TODO

    def failureS(row: Option[D]) =
      ReactS.mod(setStatusToRetry(row))

    def failureIO(T: ComponentStateFocus[S], row: Option[D]) =
      FailureIO(T.runState(failureS(row)))
  }

  // =====================================================================================================================

  final private[table]
  class FullRow[M[_] : Bind, S, V1, V2, R](renderAttr: R => ComponentStateFocus[S] => M[V1],
                                           renderRow: (ComponentStateFocus[S], R, V1) => V2) {
    def render(T: ComponentStateFocus[S]): R => M[V2] =
      id => renderAttr(id)(T).map(v1 => renderRow(T, id, v1))
  }

  @inline final private[table]
  def updateIfNeeded[S, U, L, L2](getLast: S => L, needSave: (L, U) => Option[L2])(f: (S, L2, U) => IO[S]): (S, U) => IO[S] =
    (s,u) => {
      needSave(getLast(s), u) match {
        case Some(l2) => f(s, l2, u)
        case None     => IO(s)
      }
    }
}
