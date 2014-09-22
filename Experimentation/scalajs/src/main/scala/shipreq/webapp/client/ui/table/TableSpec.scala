package shipreq.webapp.client.ui.table

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import monocle._
import monocle.function.Field2.second
import monocle.std.tuple2._
import monocle.syntax._
import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.client.ui._

import scalaz.Bind
import scalaz.Scalaz.Id
import scalaz.effect.IO
import scalaz.std.option._
import scalaz.syntax.bind._

trait RowRenderer[S, G, P, E, V] {
  final def render(eL: SimpleLens[S, E], s2mp: S => P) = renderM[Id](WeirdLens from eL, s2mp) _

  def renderM[M[_] : Bind : Optional2](eL: WeirdLens[M, S, S, E], s2mp: S => M[P])(saveG: (S, G) => IO[S]): ComponentStateFocus[S] => M[V]
}

// =====================================================================================================================

class TableSpecB[S, DataId, O, P, I, V](val PtoI: P => I,
                                        val renderable: Option[DataId] => RowRenderer[S, O, P, I, V],
                                        val savedUnsaved: SavedUnsavedL[S, DataId, P, I],
                                        val initialState: Seq[(DataId, P)] => S) {

  def saveFn2(saveIO: (Option[P], O) => IO[P], id: P => DataId) =
    saveFn((opx, o) => saveIO(opx.map(_._2), o).map(p => (id(p), p)))

  def saveFn(saveIO: (Option[(DataId, P)], O) => IO[(DataId, P)]) =
    new TableSpec[S, DataId, O, P, I, V](this, saveIO)
}

object TableSpecB {

  def default[DataId, G, P, I, V](spec: RowSpec[SavedAndUnsaved[DataId, P, I], Option[DataId], G, P, I, V]) = {
    val init = spec.initial _
    val initialState: Seq[(DataId, P)] => spec.SS =
      xs => (xs.map(x => x._1 ->(x._2, init(x._2))).toMap, None)
    new TableSpecB[spec.SS, DataId, G, P, I, V](init, spec.forRow, SavedUnsavedL.default, initialState)
  }
}

// =====================================================================================================================

object TableSpec {

  class FullRow[M[_] : Bind, S, V1, V2, RowId](renderAttr: RowId => ComponentStateFocus[S] => M[V1],
                                               renderRow: (ComponentStateFocus[S], RowId, V1) => V2) {
    def render(T: ComponentStateFocus[S]): RowId => M[V2] =
      id => renderAttr(id)(T).map(v1 => renderRow(T, id, v1))
  }

  def saveHelper[S, G, L, L2, Px](getLast: S => L,
                                  needSave: (L, G) => Option[L2],
                                  saveIO: (L2, G) => IO[Px],
                                  storeSaved: (S, Px) => S): (S, G) => IO[S] =
    (s,g) => {
      val last = getLast(s)
      needSave(last, g) match {
        case Some(l2) => saveIO(l2, g).map(storeSaved(s, _))
        case None     => IO(s)
      }
    }
}

// =====================================================================================================================

import TableSpec._

class TableSpec[S, DataId, O, P, I, V](tsb: TableSpecB[S, DataId, O, P, I, V],
                                        saveIO: (Option[(DataId, P)], O) => IO[(DataId, P)]) {

  import tsb.{PtoI, renderable, initialState => _initialState}
  import tsb.savedUnsaved.{savedL, unsavedL}

  @inline final private def ST = ReactS.Fix[S]

  private type Unsaved = Option[I]
  private type Saved = Map[DataId, (P, I)]
  type Px = (DataId, P)

  def mkPI(p: P): (P,I) = (p, PtoI(p))

  def initialState(xs: Seq[P], id: P => DataId): S = _initialState(xs.map(x => id(x) -> x))
  def initialState(d: Map[DataId, P]): S = _initialState(d.toSeq)
  def initialState(d: Seq[(DataId, P)]): S = _initialState(d)

  // ----------------------------------------------------------------------
  // Unsaved

  private def renderAttrForUnsaved(saveIO: (S, O) => IO[S]) = {
    val s2op: S => Option[P] = _ => None
    def setI(s: S, i: I): Option[S] = unsavedL.get(s).map(_ => unsavedL.set(s, Some(i)))
    val se = WeirdLens[Option, S, S, I](unsavedL.get, setI)
    renderable(None).renderM(se, s2op)(saveIO)
  }

  def createUnsaved(empty: I) = ReactS.mod(unsavedL.modifyF(_ orElse Some(empty)))

  val removeUnsaved = unsavedL setF None
  val removeUnsavedS = ReactS.mod(removeUnsaved)
  //        val cancelUnsaved = scalaz.State.modify[S](unsavedL setF None)

  def insertUnsaved(px: Px)          : S => S = insertUnsaved(px._1, px._2)
  def insertUnsaved(id: DataId, p: P): S => S = updateSaved(id, p) compose removeUnsaved

  private val _renderAttrUnsaved =
    renderAttrForUnsaved((s,g) => saveIO(None, g).map(insertUnsaved(_)(s)))

  def unsavedRow[V2](renderRow: (ComponentStateFocus[S], V) => V2) =
    _unsavedRow(_renderAttrUnsaved, renderRow)

  private def _unsavedRow[V2](renderAttr: ComponentStateFocus[S] => Option[V], renderRow: (ComponentStateFocus[S], V) => V2) =
    new FullRow[Option, S, V, V2, Unit](_ => renderAttr(_), (T,_,vv) => renderRow(T, vv))

  // ----------------------------------------------------------------------
  // Saved

  private def rowL(id: DataId) = savedL composeLens SimpleLens[Saved](_(id))((a,b) => a + (id -> b))
  private def rowIL(id: DataId) = rowL(id) |-> second
  private def rowP(id: DataId): S => P = savedL.get(_)(id)._1
  private def rowPx(id: DataId): S => Px = s => (id, rowP(id)(s))

  private def saveRowFn(id: DataId) =
    saveHelper[S, O, Px, Px, Px](
      s => (id, rowP(id)(s)),
      (px,g) => if (px._2 == g) None else Some(px),
      (px,g) => saveIO(Some(px), g),
      (s,px) => updateSaved(px)(s))

  private def renderAttrForSaved(id: DataId) =
    renderable(Some(id)).render(rowIL(id), rowP(id))(saveRowFn(id))

  def removeSaved(id: DataId) = savedL.modifyF(m => m - id)
  def removeSavedS(id: DataId) = ReactS.mod(removeSaved(id))

  def deleteSavedS(f: DataId => IO[Unit]): DataId => ReactST[IO, S, Unit] =
    id => ReactS.retM(f(id)) >> removeSavedS(id)

  def cancelChangesS(id: DataId) = ST.mod(s => rowIL(id).set(s, PtoI(rowP(id)(s))))

  def modAndSaveS(modAndSaveIO: Px => IO[Px]): DataId => ReactST[IO, S, Unit] = id => {
    val modsaveS: ReactST[IO, S, Px] = ST.gets(rowPx(id)).lift[IO].flatMap(px1 => ST.retM(modAndSaveIO(px1)))
    //        .map(px2 => updateSaved(px2)(s1))
    //      ST.gets(rowPx(id)).flatMap()

    //      ReactS.modT[IO, S](s1 => {
    //        val px: Px = rowPx(id)(s1)
    //        modAndSaveIO(px).map(updateSaved(_)(s1))
    //      })
    //cancelChangesS(id)
    modsaveS flatMap updateSavedS
  }

  def updateSaved(px: Px)          : S => S = updateSaved(px._1, px._2)
  def updateSaved(id: DataId, p: P): S => S = savedL.modifyF(_ + (id -> mkPI(p)))

  def updateSavedS(px: Px) = ST.mod(updateSaved(px))

  def savedRow[V2](renderRow: (ComponentStateFocus[S], DataId, P, V) => V2) =
    _savedRow(renderAttrForSaved, (t,i,v) => renderRow(t,i,rowL(i).get(t.state)._1,v))

  def savedRow[V2](renderRow: (ComponentStateFocus[S], DataId, V) => V2) =
    _savedRow(renderAttrForSaved, renderRow)

  private def _savedRow[V2](renderAttr: DataId => ComponentStateFocus[S] => V, renderRow: (ComponentStateFocus[S], DataId, V) => V2) =
    new FullRow[Id, S, V, V2, DataId](renderAttr, renderRow)

  type SavedPs = Stream[(DataId, P)]
  def renderSaved(T: ComponentStateFocus[S], r: FullRow[Id, S, V, Tag, DataId])(f: SavedPs => SavedPs) = {
    val rr = r.render(T)
    f(getSaved(T)).map(x => rr(x._1)).toJsArray
  }

  def getSaved(T: ComponentStateFocus[S]): SavedPs =
    savedL.get(T.state).toStream.map(x => x._1 -> x._2._1)
}