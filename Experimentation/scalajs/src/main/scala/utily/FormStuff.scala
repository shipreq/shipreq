package utily

import japgolly.scalajs.react.vdom.{ReactOutput, ReactVDom, VDomBuilder, ReactFragT}
import org.scalajs.dom
import org.scalajs.dom.extensions.KeyCode
import scala.runtime.{AbstractFunction3, AbstractFunction2}
import scala.scalajs.js

import japgolly.scalajs.react._
import vdom.ReactVDom._
import all._
import ScalazReact._

import scalaz.effect.IO
import scalaz.Scalaz.Id
import scalaz.{Foldable, Bind, \/, \/-, -\/, StateT}
import scalaz.std.option._
import scalaz.syntax.bind._
import scalaz.syntax.foldable._

//import golly.ScalazReact._
import monocle._
import monocle.syntax._
import monocle.function.Field1._
import monocle.function.Field2._

import Lib._
import EditorStuff._

// TODO add drag/drop ordering to table
// TODO state date structure help

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
 * [ priority . effort ]
 *       handle name swap (should save both, not just one)
 * [3.5] different view when field not in edit (sometimes the edit view is too noisy)
 * [2.3] visual indication of save-in-progress & save-complete
 * [2.2] server-side only errors / errors on save
 * [1.3] validators with composite types (like new & change password)
 *
 */
object FormStuff {

  def foldableToOption[M[_]: Foldable, A](m: M[A]): Option[A] =
    foldMapAP(m, None: Option[A])(Some.apply)

  trait Renderable[S, G, P, E, V, VV] {
    final def render(eL: SimpleLens[S, E], s2mp: S => P) = renderM[Id](WierdLens from eL, s2mp) _

    def renderM[M[_] : Bind : Foldable]
    (eL: WierdLens[M, S, S, E], s2mp: S => M[P])
    (saveG: (S, G) => IO[S]): ComponentStateFocus[S] => M[VV]

    //    def contra[T](l: SimpleLens[T, S]) = new ContraRenderable[T, S, G, P, E, V, VV](this, l)
  }

  /*
  class ContraRenderable[T, S, G, P, E, V, VV](
      to: Renderable[S, G, P, E, V, VV]
  , l: SimpleLens[T, S]
      ) extends Renderable[T, G, P, E, V, VV] {

    override def renderM[M[_] : Bind : Foldable]
      (eL: WierdLens[M, T, T, E], s2mp: T => M[P])
      (saveG: (T, G) => IO[T]): ComponentStateFocus[T] => M[VV] =
      T => {
        //def dimap[F, G](f: F => S, g: T => G) =


        use T.state everywhere will wipe out any changes to T not in S

        val S: ComponentStateFocus[S] = T.focusState[S](l.get _)(l.set _)

        val t_me = eL.get
        val t_e_mt = eL.set
        val t_s = l.get _
        val t_s_t = l.set _

        // get
        val s_me: S => M[E] = s => {
          val t: T = t_s_t(T.state, s)
          t_me(t)
        }

        // set
        val s_e_ms: (S,E) => M[S] = (s,e) => {
          val t: T = t_s_t(T.state, s)
          val mt = t_e_mt(t, e)
          implicitly[Bind[M]].map(mt)(t_s)
        }

        val XeL: WierdLens[M, S, S, E] = WierdLens[M, S, S, E](s_me, s_e_ms)
        val Xs2mp: S => M[P] = s => s2mp(t_s_t(T.state, s))
        val XsaveG: (S, G) => IO[S] = (s,g) => saveG(t_s_t(T.state, s), g).map(t_s)
        val x = to.renderM[M](XeL, Xs2mp)(XsaveG)
        x(S)
      }
  }
*/

  def SpecAttr[P] = new {
    import SpecN._

    def apply[C](p2c: P => C) = new {
      def apply[I, O](v: Validator[I, C, O])(e: Editor[I, ReactVDom.Modifier]) =
        SpecSplice(p2c, v).edit(e)
    }
  }

  class TableSpec[S, DataId, O, P, V, I, VV](
                                       renderable: Option[DataId] => Renderable[S,O,P,I,V,VV],
                                       savedL: SimpleLens[S, Map[DataId, (P, I)]],
                                       unsavedL: SimpleLens[S, Option[I]],
                                       PtoI: P => I,
                                       initialState: Seq[(DataId, P)] => S,
                                       saveIO: (Option[(DataId, P)], O) => IO[(DataId, P)]
                                       ) {

//    def contramap[T](l: SimpleLens[T, S], i: S => T) = new TableSpec[T, DataId, O, P, V, I, VV](
//      d => renderable(d).contra(l), l |-> savedL, l |-> unsavedL, PtoI, i compose initialState, saveIO)

    @inline final private def ST = ReactS.Fix[S]

    private type Unsaved = Option[I]
    private type Saved = Map[DataId, (P, I)]
    type Px = (DataId, P)

    def mkPI(p: P): (P,I) = (p, PtoI(p))

    def initialState(xs: Seq[P], id: P => DataId): S =
      initialState(xs.map(x => id(x) -> x))

    // ----------------------------------------------------------------------
    // Unsaved

    private def renderAttrForUnsaved(saveIO: (S, O) => IO[S]) = {
      val s2op: S => Option[P] = _ => None
      def setI(s: S, i: I): Option[S] = unsavedL.get(s).map(_ => unsavedL.set(s, Some(i)))
      val se = WierdLens[Option, S, S, I](unsavedL.get, setI)
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

    def unsavedRow[V2](renderRow: (ComponentStateFocus[S], VV) => V2) =
      _unsavedRow(_renderAttrUnsaved, renderRow)

    private def _unsavedRow[V2](renderAttr: ComponentStateFocus[S] => Option[VV], renderRow: (ComponentStateFocus[S], VV) => V2) =
      new FullRow[Option, S, VV, V2, Unit](_ => renderAttr(_), (T,_,vv) => renderRow(T, vv))

    // ----------------------------------------------------------------------
    // Saved

    private def rowL(id: DataId) = savedL composeLens SimpleLens2[Saved](_(id))((a,b) => a + (id -> b))
    private def rowIL(id: DataId) = rowL(id) |-> _2
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

    def savedRow[V2](renderRow: (ComponentStateFocus[S], DataId, P, VV) => V2) =
      _savedRow(renderAttrForSaved, (t,i,v) => renderRow(t,i,rowL(i).get(t.state)._1,v))

    def savedRow[V2](renderRow: (ComponentStateFocus[S], DataId, VV) => V2) =
      _savedRow(renderAttrForSaved, renderRow)

    private def _savedRow[V2](renderAttr: DataId => ComponentStateFocus[S] => VV, renderRow: (ComponentStateFocus[S], DataId, VV) => V2) =
      new FullRow[Id, S, VV, V2, DataId](renderAttr, renderRow)

    type SavedPs = Stream[(DataId, P)]
    def renderSaved(T: ComponentStateFocus[S], r: FullRow[Id, S, VV, Tag, DataId])(f: SavedPs => SavedPs) = {
      val rr = r.render(T)
      f(getSaved(T)).map(x => rr(x._1)).toJsArray
    }

    def getSaved(T: ComponentStateFocus[S]): SavedPs =
      savedL.get(T.state).toStream.map(x => x._1 -> x._2._1)
  }

  // ===================================================================================================================
  // rows

  class FullRow[M[_] : Bind, S, VV, V, RowId](renderAttr: RowId => ComponentStateFocus[S] => M[VV],
                                          renderRow: (ComponentStateFocus[S], RowId, VV) => V
                                           ) {
    def render(T: ComponentStateFocus[S]): RowId => M[V] =
      id => renderAttr(id)(T).map(vv => renderRow(T, id, vv))
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
                                      spec: TableSpec[S, DataId, _, P, _, _, _])(
                                      l: SimpleLens[P, Boolean],
                                      saveIO: DataId => DeletionAction => IO[Unit]
                               ) {
    private type Px = (DataId, P)
    private val hardDelS = spec.deleteSavedS(id => saveIO(id)(HardDelete))
    private val softDeleteL = _2[Px, P] composeLens l
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
