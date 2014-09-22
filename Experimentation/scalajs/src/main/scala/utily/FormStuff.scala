package utily

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._
import ScalazReact._

import scalaz.effect.IO
import scalaz.Scalaz.Id
import scalaz.Bind
import scalaz.std.option._
import scalaz.syntax.bind._

import monocle._
import monocle.syntax._
import monocle.function.Field1.first
import monocle.function.Field2.second
import monocle.std.tuple2._

import shipreq.webapp.client.ui._
import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.client.ui.Util._

/**
 * Done
 * ~~~~
 * [5] create new
 * [5] saves only when entire row is valid
 * [4] validation as you type
 * [4] input correction (valid or not)
 * [3] field validity depending on other fields (in same row)
 * [3] field validity depending on other rows
 * [2] escape to cancel change
 *
 * Done in Isolation
 * ~~~~~~~~~~~~~~~~~
 * [5.5] drag to reorder
 * [5.3] delete
 * [5.2] show/hide deleted
 *
 * TODO
 * ~~~~
 * [PRIORITY.EFFORT]
 * [3.?] handle name swap (should save both, not just one)
 * [3.5] different view when field not in edit (sometimes the edit view is too noisy)
 * [2.3] visual indication of save-in-progress & save-complete
 * [2.2] server-side only errors / errors on save
 * [1.3] validators with composite types (like new & change password)
 * [?.?] avoid NOP saves
 * [?.?] state date structure help
 * [?.?] add drag/drop ordering to table
 */
object FormStuff {

  trait Renderable[S, G, P, E, V] {
    final def render(eL: SimpleLens[S, E], s2mp: S => P) = renderM[Id](WeirdLens from eL, s2mp) _

    def renderM[M[_] : Bind : Optional2](eL: WeirdLens[M, S, S, E], s2mp: S => M[P])(saveG: (S, G) => IO[S]): ComponentStateFocus[S] => M[V]
  }

  /**
   * Single field attribute: types & validation logic.
   * Single field attribute: +renderToView.
   */
  class AttrSpec[P, V, I, C, O](val p2c: P => C, val v: Validator[I, C, O], val editor: Editor[I, V]) {
    def initial: P => I = v.c2i compose p2c
    //    def savable(i: I) = v.correctAndValidate(i).toOption
    @inline final def toW[S,W](vw: Option[ValidateFnW[S, W, O]]) = new AttrSpecW(this, vw)
  }

  class AttrSpecW[S, W, P, V, I, C, O](s: AttrSpec[P, V, I, C, O], val vw: Option[ValidateFnW[S, W, O]])
      extends AttrSpec(s.p2c, s.v, s.editor)

  trait SpecN[S, W, G, P, I, V] {
    final type II = I
    final type VV = V
    def initial(p: P): I
    def forRow(w: W): Renderable[S, G, P, I, V]
  }

  class TableSpecB[S, DataId, O, P, I, V](PtoI: P => I,
                                          renderable: Option[DataId] => Renderable[S, O, P, I, V],
                                          savedUnsaved: SavedUnsavedL[S, DataId, P, I],
                                          initialState: Seq[(DataId, P)] => S) {

    def saveFn2(saveIO: (Option[P], O) => IO[P], id: P => DataId) =
      saveFn((opx, o) => saveIO(opx.map(_._2), o).map(p => (id(p), p)))

    def saveFn(saveIO: (Option[(DataId, P)], O) => IO[(DataId, P)]) =
      new TableSpec[S, DataId, O, P, I, V](PtoI, renderable, savedUnsaved, initialState, saveIO)
  }

  def newTableSpecB[DataId, G, P, I, V](spec: SpecN[SavedAndUnsaved[DataId, P, I], Option[DataId], G, P, I, V]) = {
    val init = spec.initial _
    val initialState: Seq[(DataId, P)] => SavedAndUnsaved[DataId, P, I] =
      xs => (xs.map(x => x._1 -> (x._2, init(x._2))).toMap, None)

    new TableSpecB[SavedAndUnsaved[DataId, P, I], DataId, G, P, I, V](init, spec.forRow, simpleSavedUnsavedL, initialState)
  }

  type Saved[DataId, P, I] = Map[DataId, (P, I)]
  type Unsaved[I] = Option[I]
  type SavedAndUnsaved[DataId, P, I] = (Saved[DataId, P, I], Unsaved[I])

  def simpleSavedUnsavedL[DataId, P, I] =
    SavedUnsavedL[SavedAndUnsaved[DataId, P, I], DataId, P, I](first, second)

  case class SavedUnsavedL[S, DataId, P, I](
    savedL: SimpleLens[S, Saved[DataId, P, I]],
    unsavedL: SimpleLens[S, Unsaved[I]])

  def SpecAttr[P] = new {
    def apply[C](p2c: P => C) = new {
      def apply[I, O](v: Validator[I, C, O])(e: Editor[I, Modifier]) =
        new AttrSpec(p2c, v, e)
    }
  }

  class TableSpec[S, DataId, O, P, I, V](
      PtoI: P => I,
      renderable: Option[DataId] => Renderable[S,O,P,I,V],
      savedUnsaved: SavedUnsavedL[S, DataId, P, I],
      _initialState: Seq[(DataId, P)] => S,
      saveIO: (Option[(DataId, P)], O) => IO[(DataId, P)]) {

    import savedUnsaved.{savedL, unsavedL}

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

  // ===================================================================================================================
  // rows

  class FullRow[M[_] : Bind, S, V1, V2, RowId](
      renderAttr: RowId => ComponentStateFocus[S] => M[V1],
      renderRow: (ComponentStateFocus[S], RowId, V1) => V2) {

    def render(T: ComponentStateFocus[S]): RowId => M[V2] =
      id => renderAttr(id)(T).map(v1 => renderRow(T, id, v1))
  }

  // ===================================================================================================================
  // util

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

//  def deleteS[S, P, R](getP: S => P, save: P => IO[R], store: (S, P, R) => S) =
//    StateT[IO, S, Unit](s => {
//      val p = getP(s)
//      save(p).map(r => (store(s, p, r), ()))
//    })

  sealed trait DeletionAction
  case object HardDelete extends DeletionAction
  case object SoftDelete extends DeletionAction
  case object Restore extends DeletionAction

  class DeletionThingy[S, P, DataId](
                                      spec: TableSpec[S, DataId, _, P, _, _])(
                                      l: SimpleLens[P, Boolean],
                                      saveIO: DataId => DeletionAction => IO[Unit]
                               ) {
    private type Px = (DataId, P)
    private val hardDelS = spec.deleteSavedS(id => saveIO(id)(HardDelete))
    private val softDeleteL = second[Px, P] composeLens l
    private def aliveS(ls: DeletionAction, alive: Boolean) =
      spec.modAndSaveS(px => saveIO(px._1)(ls).map(_ => softDeleteL.set(px, alive)))
    private val softDelS = aliveS(SoftDelete, false)
    private val restoreS = aliveS(Restore, true)

    def button(T: ComponentStateFocus[S], id: DataId, a: DeletionAction) =
      a match {
        case HardDelete => all.button(onclick ~~> T.runState(hardDelS(id)))("Delete Forever")
        case SoftDelete => all.button(onclick ~~> T.runState(softDelS(id)))("Delete")
        case Restore    => all.button(onclick ~~> T.runState(restoreS(id)))("Restore")
      }

    def buttons(T: ComponentStateFocus[S], id: DataId, as: DeletionAction*) =
      as.map(button(T, id, _))

    def getSaved(T: ComponentStateFocus[S], alive: Boolean): Stream[(DataId, P)] =
      spec.getSaved(T).filter(px => l.get(px._2) == alive)

    def getSavedP(T: ComponentStateFocus[S], alive: Boolean): Stream[P] =
      getSaved(T, alive).map(_._2)
  }
}
