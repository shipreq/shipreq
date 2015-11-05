package shipreq.webapp.client.ui.table

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import monocle._
import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.client.ui._
import scalaz.Bind
import scalaz.Scalaz.Id
import scalaz.effect.IO
import scalaz.std.option._
import scalaz.syntax.bind._

trait RowRenderer[S, U, P, II, VV] {
  final def render(iL: SimpleLens[S, II], s2p: S => P) = renderM[Id](WeirdLens from iL, s2p) _

  def renderM[M[_] : Bind : Optional2](iL: WeirdLens[M, S, S, II], s2mp: S => M[P])(save: (S, U) => IO[S]): ComponentStateFocus[S] => M[VV]
}

// =====================================================================================================================

final class TableSpecB[S, D, G, P, II, VV](val p2ii: P => II,
                                           val rowRenderer: Option[D] => RowRenderer[S, G, P, II, VV],
                                           val savedUnsaved: SavedUnsavedL[S, D, P, II],
                                           val initialState: Seq[(D, P)] => S) {

  def saveFn2(saveIO: (Option[P], G) => IO[P], id: P => D) =
    saveFn((opx, o) => saveIO(opx.map(_._2), o).map(p => (id(p), p)))

  def saveFn(saveIO: (Option[(D, P)], G) => IO[(D, P)]) =
    new TableSpec[S, D, G, P, II, VV](this, saveIO)
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

final class TableSpec[S, D, U, P, II, VV](tsb: TableSpecB[S, D, U, P, II, VV],
                                          saveIO: (Option[(D, P)], U) => IO[(D, P)]) {

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

  private def renderAttrForUnsaved(saveIO: (S, U) => IO[S]) = {
    val s2op: S => Option[P] = _ => None
    def setI(s: S, i: II): Option[S] = unsavedL.get(s).map(_ => unsavedL.set(s, Some(i)))
    val se = WeirdLens[Option, S, S, II](unsavedL.get, setI)
    rowRenderer(None).renderM(se, s2op)(saveIO)
  }

  def createUnsaved(empty: II) = ReactS.mod(unsavedL.modifyF(_ orElse Some(empty)))

  val removeUnsaved = unsavedL setF None
  val removeUnsavedS = ReactS.mod(removeUnsaved)
  //        val cancelUnsaved = scalaz.State.modify[S](unsavedL setF None)

  def insertUnsaved(px: DP)          : S => S = insertUnsaved(px._1, px._2)
  def insertUnsaved(id: D, p: P): S => S = updateSaved(id, p) compose removeUnsaved

  private val _renderAttrUnsaved =
    renderAttrForUnsaved((s,g) => saveIO(None, g).map(insertUnsaved(_)(s)))

  def unsavedRow[V2](renderRow: (ComponentStateFocus[S], VV) => V2) =
    _unsavedRow(_renderAttrUnsaved, renderRow)

  private def _unsavedRow[V2](renderAttr: ComponentStateFocus[S] => Option[VV], renderRow: (ComponentStateFocus[S], VV) => V2) =
    new FullRow[Option, S, VV, V2, Unit](_ => renderAttr(_), (T,_,vv) => renderRow(T, vv))

  // ----------------------------------------------------------------------
  // Saved

  private def saveRowFn(id: D) =
    saveHelper[S, U, DP, DP, DP](
      s => (id, rowP(id)(s)),
      (px,g) => if (px._2 == g) None else Some(px),
      (px,g) => saveIO(Some(px), g),
      (s,px) => updateSaved(px)(s))

  private def renderAttrForSaved(id: D) =
    rowRenderer(Some(id)).render(rowIL(id), rowP(id))(saveRowFn(id))

  def removeSaved(id: D) = savedL.modifyF(m => m - id)
  def removeSavedS(id: D) = ReactS.mod(removeSaved(id))

  def deleteSavedS(f: D => IO[Unit]): D => ReactST[IO, S, Unit] =
    id => ReactS.retM(f(id)) >> removeSavedS(id)

  def cancelChangesS(id: D) = ST.mod(s => rowIL(id).set(s, p2ii(rowP(id)(s))))

  def modAndSaveS(modAndSaveIO: DP => IO[DP]): D => ReactST[IO, S, Unit] = id => {
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

  def savedRow[V2](renderRow: (ComponentStateFocus[S], D, P, VV) => V2) =
    _savedRow(renderAttrForSaved, (t,i,v) => renderRow(t,i,rowL(i).get(t.state)._1,v))

  def savedRow[V2](renderRow: (ComponentStateFocus[S], D, VV) => V2) =
    _savedRow(renderAttrForSaved, renderRow)

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

  class FullRow[M[_] : Bind, S, V1, V2, R](renderAttr: R => ComponentStateFocus[S] => M[V1],
                                           renderRow: (ComponentStateFocus[S], R, V1) => V2) {
    def render(T: ComponentStateFocus[S]): R => M[V2] =
      id => renderAttr(id)(T).map(v1 => renderRow(T, id, v1))
  }

  def saveHelper[S, G, L, L2, DP](getLast: S => L,
                                  needSave: (L, G) => Option[L2],
                                  saveIO: (L2, G) => IO[DP],
                                  storeSaved: (S, DP) => S): (S, G) => IO[S] =
    (s,g) => {
      val last = getLast(s)
      needSave(last, g) match {
        case Some(l2) => saveIO(l2, g).map(storeSaved(s, _))
        case None     => IO(s)
      }
    }
}
