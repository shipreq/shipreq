
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

// TODO validation: check uniqueness (local)
// TODO compose a row (Spec2) with ctrls (deletion)
// TODO compose homogeneous rows (Spec2's) to create a table
// TODO compose hetrogeneous (unsaved + saved) rows (Spec2's) to create a table
// TODO add drag/drop ordering to table
// TODO state date structure help

/**
 * Done
 * ~~~~
 * [5] create new
 * [5] saves only when entire row is valid
 * [4] validation as you type
 * [4] input correction (valid or not)
 * [2] escape to cancel change
 *
 * TODO
 * ~~~~
 * [ priority . effort ]
 *       handle name swap (should save both, not just one)
 * [5.5] drag to reorder
 * [5.3] delete
 * [5.2] show/hide deleted
 * [3.5] different view when field not in edit (sometimes the edit view is too noisy)
 * [3.5] field validity depending on other fields (in same row)
 * [3.5] field validity depending on other rows
 * [2.3] visual indication of save-in-progress & save-complete
 * [2.2] server-side only errors / errors on save
 * [1.3] validators with composite types (like new & change password)
 *
 */
object FormStuff {

  case class WierdLens[M[_]: Bind, S, T, A](get: S => M[A], set: (S, A) => M[T]) {
    def mod(s: S, f: A => A): M[T] = get(s).flatMap(a => set(s, f(a)))
    def map[B](l: SimpleLens[A, B]) = WierdLens[M, S, T, B](
      s => get(s).map(l.get),
      (s,b) => get(s).flatMap(a => set(s, l.set(a, b))))
  }

  object WierdLens {
    def from[S, A](l: SimpleLens[S, A]) = WierdLens[Id, S, S, A](l.get, l.set)
  }

  private def getOrElseAP[M[_]: Foldable, A](m: M[A], a: => A): A =
    m.foldr(a)(aa => _ => aa)

  private def foldMapAP[M[_]: Foldable, A, B](m: M[A], b: => B)(f: A => B): B =
    m.foldr(b)(a => _ => f(a))

  private def foldableToOption[M[_]: Foldable, A](m: M[A]): Option[A] =
    foldMapAP(m, None: Option[A])(Some.apply)


  type ErrorMsg = String

  trait Validator[I, C, O] {
    def liveCorrect: I => I
    def correct: I => C
    def validate: C => ErrorMsg \/ O
    def c2i: C => I
    final def correctAndValidate = validate compose correct
  }

  type ValidatorX2[S, W, I, C, O] = (S, W) => Validator[I, C, O]

  class CtxValidation[S, W, A](val f: (S, W, A) => Option[ErrorMsg]) extends AbstractFunction3[S, W, A, Option[ErrorMsg]] {
    override def apply(s: S, w: W, a: A) = f(s, w, a)

    def contramap[T](g: T => S) =
      new CtxValidation[T, W, A]((t, w, a) => f(g(t), w, a))
  }

  def ValidatorX2[S, W, I, C, O](norm: Validator[I, C, O], f: CtxValidation[S, W, O], w: W): S => Validator[I, C, O] =
    s => new Validator[I, C, O] {
      def liveCorrect = norm.liveCorrect
      def correct = norm.correct
      def validate = c => {
        val orig = norm.validate(c)
        orig.flatMap(o => f(s, w, o).fold(orig)(-\/.apply))
      }
      def c2i = norm.c2i
    }

  trait Editor[D, V] {
    def apply(data: D
              , error: Option[ErrorMsg]
              , onChange: D => IO[Unit]
              , onCancel: IO[Unit] => IO[Unit]
              , onEditEnd: IO[Unit]
               ): V
  }

  // ===================================================================================================================
  // Field attr editor

  class FormAttrShit[S, I, C, O, M[_] : Bind : Foldable](
                                  vs: S => Validator[I, C, O] // Conflation. S not required for i↔c
                                  , s2mc: S => M[C]
                                  , iL: WierdLens[M, S, S, I]
                                  , trySave: S => IO[S]
                                  ) {

    private def change(i: I) = (s: S) => getOrElseAP(iL.set(s, vs(s).liveCorrect(i) ), s)

    private def cancelChange(T: ComponentScope_SS[S])(callback: IO[Unit]): IO[Unit] =
      T.stateIO.flatMap(s =>
        foldMapAP(s2mc(s), IO(()))(c => T.modStateIO(change(vs(s) c2i c), callback)))

    private def editEnd(T: ComponentScope_SS[S]): IO[Unit] =
      T.stateIO.flatMap(s => {
        val v = vs(s)
        // optimisation: compare I & I here, don't try to save if equal
        // correctness, don't try to save if invalid
        val m = iL.mod(s, v.c2i compose v.correct).map(trySave(_) >>= T.setStateIO)
        getOrElseAP(m, IO(()))
      })

    def render[V](editor: Editor[I, V], T: ComponentScope_SS[S]): M[V] = {
      val s = T.state
      iL.get(s).map(i => {
        val e = vs(s).correctAndValidate(i).swap.toOption
        editor(i, e, i => T modStateIO change(i), cancelChange(T), editEnd(T))
      })
    }
  }

  // ===================================================================================================================
  // Field rules and composition

  /**
   * Single field attribute: types & validation logic.
   */
  case class SpecSplice[P, I, C, O](p2c: P => C, v: Validator[I, C, O]) {
    def initial: P => I = v.c2i compose p2c
//    def savable(i: I) = v.correctAndValidate(i).toOption
    def edit[V](e: Editor[I, V]) = SpecSpliceE(this, e)
  }

  /**
   * Single field attribute: +renderToView.
   */
  case class SpecSpliceE[P, V, I, C, O](s: SpecSplice[P, I, C, O], editor: Editor[I, V])

  /**
   * This is actually just field attribute composition.
   * Single row/record.
   *
   * @tparam G "Good", meaning entire row has passed validation, row ready to be saved.
   * @tparam P "Persisted", the last saved copy of the row.
   * @tparam V "View", the type of the DOM representation.
   */
  case class Spec2[G, P, V, I1, C1, O1, I2, C2, O2](s1: SpecSpliceE[P,V,I1,C1,O1], s2: SpecSpliceE[P,V,I2,C2,O2]
                                                    , oo2g: ((O1, O2)) => G
                                                     ) {
    type E = (I1,I2)
    type OO = (O1, O2)
    type VV = (V, V)

    def initial(p: P): E = (s1.s initial p, s2.s initial p)

    def forState[S] =
      Spec2X[S, Unit, G, P, V, I1, C1, O1, I2, C2, O2](this, None, None).forRow(())
  }

  /*
    Full table:
    S - full state
    W - row id
    find rows matching criteria, return W
    use W to modify and save rows
   */

  /**
   * Field attributes + TABLE ROW-ID + [ROW-ID & STATE AWARE VALIDATORS]
   */
  case class Spec2X[S, W, G, P, V, I1, C1, O1, I2, C2, O2](
      spec: Spec2[G, P, V, I1, C1, O1, I2, C2, O2],
      ctxV1: Option[CtxValidation[S, W, O1]],
      ctxV2: Option[CtxValidation[S, W, O2]]
  ) {

    type E = spec.E
    type VV = spec.VV

    import spec.{OO, s1, s2, oo2g}

    private def fieldRenderers[M[_] : Bind : Foldable](s2mp: S => M[P],
                                                          w: W,
                                                          saveG: (S, G) => IO[S],
                                                          eL: WierdLens[M, S, S, E]) = {

      val v1 = ctxV1.fold[S => Validator[I1,C1,O1]]( _ => s1.s.v )( c=> ValidatorX2(s1.s.v, c, w) )
      val v2 = ctxV2.fold[S => Validator[I2,C2,O2]]( _ => s2.s.v )( c=> ValidatorX2(s2.s.v, c, w) )

      //def savable1(i: I) = v.correctAndValidate(i).toOption
      def savable(s: S, e: E): Option[OO] = for {
        o1 <- v1(s).correctAndValidate(e._1).toOption
        o2 <- v2(s).correctAndValidate(e._2).toOption
      } yield (o1,o2)

      // TODO This isn't able to traverse other rows, or access them by W
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S => IO[S] = s =>
        foldableToOption(eL.get(s)).flatMap(savable(s,_)).fold(IO(s))(oo => saveG(s, oo2g(oo)))

      (
        new FormAttrShit[S, I1, C1, O1, M](v1, s2mp.andThen(_ map s1.s.p2c), eL map _1[E, I1], sf),
        new FormAttrShit[S, I2, C2, O2, M](v2, s2mp.andThen(_ map s2.s.p2c), eL map _2[E, I2], sf)
      )
    }

    def forRow(w: W): Renderable[S, G, P, E, V, VV] = new Renderable[S, G, P, E, V, VV] {
      override def renderM[M[_] : Bind : Foldable]
        (eL: WierdLens[M, S, S, E], s2mp: S => M[P])
        (saveG: (S, G) => IO[S]): ComponentScope_SS[S] => M[VV] = T => {
          val s = fieldRenderers(s2mp, w, saveG, eL)
          for {
            v1 <- s._1.render(s1.editor, T)
            v2 <- s._2.render(s2.editor, T)
          } yield (v1,v2)
        }
    }
  }

  trait Renderable[S, G, P, E, V, VV] {
    final def render(eL: SimpleLens[S, E], s2mp: S => P) = renderM[Id](WierdLens from eL, s2mp) _

    def renderM[M[_] : Bind : Foldable]
      (eL: WierdLens[M, S, S, E], s2mp: S => M[P])
      (saveG: (S, G) => IO[S]): ComponentScope_SS[S] => M[VV]
  }

  // ===================================================================================================================
  // easier spec building
  
  def SpecAttr[P] = new {
    def apply[C](p2c: P => C) = new {
      def apply[I, O](v: Validator[I, C, O])(e: Editor[I, ReactVDom.Modifier]) =
        SpecSplice(p2c, v).edit(e)
    }
  }

  def SpecBuilder[P] = new {
    def apply[V, I1, C1, O1, I2, C2, O2](s1: SpecSpliceE[P,V,I1,C1,O1], s2: SpecSpliceE[P,V,I2,C2,O2]) =
      new SpecBuilder2[P, (O1,O2), V, I1, C1, O1, I2, C2, O2](s1, s2, oo=>oo)
  }

  class SpecBuilder2[P, O, V, I1, C1, O1, I2, C2, O2](
      s1: SpecSpliceE[P,V,I1,C1,O1], s2: SpecSpliceE[P,V,I2,C2,O2], buildO: ((O1,O2)) => O) {

    type I = (I1,I2)

    def mapO[OO](f: O => OO) = new SpecBuilder2(s1,s2, f compose buildO)
    def buildO[OO](f: (O1,O2) => OO) = new SpecBuilder2(s1,s2, f.tupled)
    
    def rowId[W] = new B2[W]

    class B2[DataId] {
      type RowId = Option[DataId]
      private type Unsaved = Option[I]
      private type Saved = Map[DataId, (P, I)]
      type S = (Saved, Unsaved)
      private val savedL = _1[S, Saved]
      private val unsavedL = _2[S, Unsaved]
      private def savedIL(id: DataId) = savedL composeLens SimpleLens2[Saved](_(id))((a,b) => a + (id -> b))
      type Px = (DataId, P)

      def uniquenessCheck[A](f: P => A) = uniqueness[S, RowId, (DataId, (P, I)), A](
        (s,ow) => savedL.get(s).toStream.filterNot(wpi => ow.fold(false)(_ == wpi._1)),
        (wpi,a) => a == f(wpi._2._1)
      )

      def ctxAwareValidators(cv1: Option[CtxValidation[S, RowId, O1]], cv2: Option[CtxValidation[S, RowId, O2]]) = new {
        def saveFn(f: (Option[Px], O) => IO[Px]) =
          new B3(Spec2X(Spec2(s1, s2, buildO), cv1, cv2), f)
      }


      class B3(val spec: Spec2X[S, RowId, O, P, V, I1, C1, O1, I2, C2, O2]
                  ,saveIO: (Option[Px], O) => IO[Px]
                  ) {
        type I = SpecBuilder2.this.I
//        type O = this.O
        type S = B2.this.S
        type VV = spec.VV

        private def mkPI(p: P) = (p, spec.spec initial p)

        def initialState(xs: Seq[(DataId, P)]): S =
          (xs.map(x => x._1 -> mkPI(x._2)).toMap, None)

        def initialState(xs: Seq[P], id: P => DataId): S =
          initialState(xs.map(x => id(x) -> x))

        private def renderAttrForUnsaved(saveIO: (S, O) => IO[S]) = {
          val s2op: S => Option[P] = _ => None
          def setI(s: S, i: I): Option[S] = unsavedL.get(s).map(_ => unsavedL.set(s, Some(i)))
          val se = WierdLens[Option, S, S, I](unsavedL.get, setI)
          spec.forRow(None).renderM(se, s2op)(saveIO)
        }

        def createUnsaved(empty: I) = scalaz.State.modify[S](unsavedL.modifyF(_ orElse Some(empty)))

        val removeUnsaved = unsavedL setF None
//        val cancelUnsaved = scalaz.State.modify[S](unsavedL setF None)

        def insertUnsaved(px: Px)          : S => S = insertUnsaved(px._1, px._2)
        def insertUnsaved(id: DataId, p: P): S => S = updateSaved(id, p) compose removeUnsaved

        private val _renderAttrUnsaved =
          renderAttrForUnsaved((s,g) => saveIO(None, g).map(insertUnsaved(_)(s)))

        def unsavedRow[V2](renderRow: (ComponentScope_SS[S], VV) => V2) =
          _unsavedRow(_renderAttrUnsaved, renderRow)

        private def _unsavedRow[V2](renderAttr: ComponentScope_SS[S] => Option[VV], renderRow: (ComponentScope_SS[S], VV) => V2) =
          new FullRow[Option, S, VV, V2, Unit](_ => renderAttr(_), (T,_,vv) => renderRow(T, vv))

        private def renderAttrForSaved(id: DataId, saveIO: (Option[Px], O) => IO[Px]) = {
          val l: SimpleLens[S, (P, I)] = savedIL(id)
          val sp: SimpleLens[S, P] = l |-> _1
          val si: SimpleLens[S, I] = l |-> _2

          val save = saveHelper[S, O, Px, Px, Px](
            s => (id, sp get s),
            (px,g) => if (px._2 == g) None else Some(px),
            (px,g) => saveIO(Some(px), g),
            (s,px) => updateSaved(px)(s))

          spec.forRow(Some(id)).render(si, sp.get)(save)
        }

        def removeSaved(id: DataId) = savedL.modifyF(m => m - id)

        def deleteSavedFn(f: DataId => IO[Unit]) = (id: DataId) =>
          runStoreU(f(id), removeSaved(id))


        def updateSaved(px: Px)          : S => S = updateSaved(px._1, px._2)
        def updateSaved(id: DataId, p: P): S => S = savedL.modifyF(_ + (id -> mkPI(p)))

        def savedRow[V2](renderRow: (ComponentScope_SS[S], DataId, P, VV) => V2) =
          _savedRow(renderAttrForSaved(_, saveIO), (t,i,v) => renderRow(t,i,savedIL(i).get(t.state)._1,v))

        def savedRow[V2](renderRow: (ComponentScope_SS[S], DataId, VV) => V2) =
          _savedRow(renderAttrForSaved(_, saveIO), renderRow)

        private def _savedRow[V2](renderAttr: DataId => ComponentScope_SS[S] => VV, renderRow: (ComponentScope_SS[S], DataId, VV) => V2) =
            new FullRow[Id, S, VV, V2, DataId](renderAttr, renderRow)

        type SavedL = List[(DataId, (P,I))]
        def renderSaved(T: ComponentScope_SS[S], r: FullRow[Id, S, VV, Tag, DataId])(f: SavedL => SavedL) = {
          val rr = r.render(T)
          f(savedL.get(T.state).toList).map(x => rr(x._1)).toJsArray
        }
      }
    }
  }
  


  // ===================================================================================================================
  // rows

  class FullRow[M[_] : Bind, S, VV, V, RowId](renderAttr: RowId => ComponentScope_SS[S] => M[VV],
                                          renderRow: (ComponentScope_SS[S], RowId, VV) => V
                                           ) {
    def render(T: ComponentScope_SS[S]): RowId => M[V] =
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

  def deleteS[S, P, R](getP: S => P, save: P => IO[R], store: (S, P, R) => S) =
    StateT[IO, S, Unit](s => {
      val p = getP(s)
      save(p).map(r => (store(s, p, r), ()))
    })

  def runStore[S, R](io: IO[R], store: (S, R) => S) =
    StateT[IO, S, Unit](s => io.map(r => (store(s, r), ())))

  def runStoreU[S](io: IO[Unit], store: S => S) =
    runStore[S, Unit](io, (s, _) => store(s))

  // ===================================================================================================================
  // Impl

  object KeyValidator extends Validator[String, String, String] {
    override def liveCorrect = _.toUpperCase()
    override def correct = _.trim.toUpperCase()
    override def validate = {
      case "" => -\/("It's blank!")
      case s if !s.matches("^[A-Z]+$") => -\/("One word, A-Z only.")
      case s => \/-(s)
    }
    override def c2i = identity
  }

  object MnemonicValidator extends Validator[String, String, String] {
    override def liveCorrect = _.toUpperCase().replaceAll("[^A-Z]+","").replaceAll("^(.{6}).+","$1")
    override def correct = _.trim.toUpperCase()
    override def validate = {
      case "" => -\/("It's blank!")
      case s if !s.matches("^[A-Z]{1,6}$") => -\/("A-Z only, 1-6 letters.")
      case s => \/-(s)
    }
    override def c2i = identity
  }

  def NopValidator[I] : Validator[I,I,I] = new Validator[I,I,I] {
    override def liveCorrect = identity
    override def correct = identity
    override def validate = \/-.apply
    override def c2i = identity
  }

  def uniqueness[S, W, A, I](extract: (S,W) => Stream[A], cmp: (A, I) => Boolean, errorMsg: ErrorMsg = "Already in use. Duplicate.") =
    new CtxValidation[S, W, I]((s, w, i) => {
      val dupFound = extract(s, w).exists(cmp(_,i))
        //.foldLeft(0)((j, a) => if (j <= 1 && cmp(a,i)) j + 1 else j) // TODO effeciency, too eager
      if (dupFound) Some(errorMsg) else None
    })

  object DescValidator extends Validator[String, Option[String], Option[String]] {
    override def liveCorrect = identity
    override def c2i = _ getOrElse ""
    override def correct = s => {
      val j = s.trim
      if (j.isEmpty) None else Some(j)
    }
    override def validate = \/-(_)
  }

  class TextEditor(node: ReactVDom.Tag) extends Editor[String, ReactVDom.Modifier] {
    override def apply(data: String
                       , error: Option[ErrorMsg]
                       , onChange: String => IO[Unit]
                       , onCancel: IO[Unit] => IO[Unit]
                       , onEditEnd: IO[Unit]
                        ) = {

      val cancelOnEscape: InputEvent => IO[Unit] =
        e => e.keyboardEvent
          .filter(_.keyCode == KeyCode.escape)
          .fold(IO(()))(_ => {
          val t = e.target
          e.preventDefaultIO >> e.stopPropagationIO >> onCancel(IO(t.blur()))
        })

      div(
        node(
          value := data
          , error.isDefined && (cls := "error")
          , onchange  ~~> textChangeRecvIO(onChange)
          , onblur    ~~> onEditEnd
          , onkeydown ~~> cancelOnEscape
        )
        , error.fold(Nop)(e => div(cls := "errorMsg")(e))
      )
    }
  }

  object CheckboxEditor extends Editor[Boolean, ReactVDom.Modifier] {
    override def apply(data: Boolean
                       , error: Option[ErrorMsg]
                       , onChange: Boolean => IO[Unit]
                       , onCancel: IO[Unit] => IO[Unit]
                       , onEditEnd: IO[Unit]
                        ) = {
      val ch: InputEvent => IO[Unit] = e => {
        val v = e.target.checked
        onChange(v) >> onEditEnd
      }

      div(
        input(
          `type` := "checkbox"
          //, value := 1
          , onchange ~~> ch
          , data && (checked := "checked")
          , error.isDefined && (cls := "error")
        )
        , error.fold(Nop)(e => div(cls := "errorMsg")(e))
      )
    }
  }


  val TextInputEditor = new TextEditor(input)
  val TextareaEditor = new TextEditor(textarea)
}
