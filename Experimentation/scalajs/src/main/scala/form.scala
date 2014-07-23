
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

  // get: S => M[A], set: (S, A) => M[T], mod: (S, A => A) => M[T] = derived

  case class WierdLens[M[_]: Bind, S, T, A](get: S => M[A], set: (S, A) => M[T]) {
    def mod(s: S, f: A => A): M[T] = get(s).flatMap(a => set(s, f(a)))
    def map[B](l: SimpleLens[A, B]) = WierdLens[M, S, T, B](
      s => get(s).map(l.get),
      (s,b) => get(s).flatMap(a => set(s, l.set(a, b))))
  }

  object WierdLens {
    def from[S, A](l: SimpleLens[S, A]) = WierdLens[Id, S, S, A](l.get, l.set)
  }
//  type WierdLens[M[_]: Bind, S, A] = Lens[S, M[S], M[A], A]

  private def getOrElseAP[M[_]: Foldable, A](m: M[A], a: => A): A =
    m.foldr(a)(aa => _ => aa)

  private def foldMapAP[M[_]: Foldable, A, B](m: M[A], b: => B)(f: A => B): B =
    m.foldr(b)(a => _ => f(a))

  private def foldableToOption[M[_]: Foldable, A](m: M[A]): Option[A] =
    foldMapAP(m, None: Option[A])(Some.apply)


  type ErrorMsg = String

  trait Validator[I, C, O] {
    def correct: I => C
    def validate: C => ErrorMsg \/ O
    def c2i: C => I
    final def correctAndValidate = validate compose correct
  }

  // Option #1
//  trait ValidatorX1[I, S, C, O] {
//    def correct: I => C
//    def validate: (C, S) => ErrorMsg \/ O
//    def c2i: C => I
//    def correctAndValidate(i: I, s: S) = validate(correct(i), s)
//  }

  // Option #2
  type ValidatorX2[S, W, I, C, O] = (S, W) => Validator[I, C, O]

//  class ValidatorS[S, I, C, O](v: Validator[I, C, O], f: (S, C) => Option[ErrorMsg]) extends Validator[I, C, O] {
//    def correct = v.correct
//    def c2i = v.c2i
//    def validate = c => f(s, c).fold(v validate c)(-\/.apply)
//  }

  class CtxValidation[S, W, A](val f: (S, W, A) => Option[ErrorMsg]) extends AbstractFunction3[S, W, A, Option[ErrorMsg]] {
    override def apply(s: S, w: W, a: A) = f(s, w, a)

//    def &(g: CtxValidation[S, A]): CtxValidation[S, A] =
//      &(g.f)
//
//    def &(g: (S, A) => Option[ErrorMsg]): CtxValidation[S, A] =
//      new CtxValidation[S, A]((s, a) => f(s, a).orElse(g(s, a)))

    def contramap[T](g: T => S) =
      new CtxValidation[T, W, A]((t, w, a) => f(g(t), w, a))
  }

//  class ValidatorS[S, I, C, O](v: Validator[I, C, O], f: (S, C) => Option[ErrorMsg]) {
//    def apply(s: S): Validator[I, C, O] =
//  }

  // def ValidatorX2_lift[S, I, C, O](norm: Validator[I, C, O]): ValidatorX2[S, I, C, O] = s => norm
//  def ValidatorX2[S, I, C, O](norm: Validator[I, C, O], f: S => C => Option[ErrorMsg]): ValidatorX2[S, I, C, O] =
//    s => new Validator[I, C, O] {
//      def correct = norm.correct
//      def validate = c => f(s)(c).flatMap(norm.validate)
//      def c2i = norm.c2i
//    }
//def ValidatorX2[S, I, C, O](norm: Validator[I, C, O], f: (S, C) => Option[ErrorMsg]): ValidatorX2[S, I, C, O] =
//  s => new Validator[I, C, O] {
//    def correct = norm.correct
//    def validate = c => f(s, c).fold(norm validate c)(-\/.apply)
//    def c2i = norm.c2i
//  }
  def ValidatorX2[S, W, I, C, O](norm: Validator[I, C, O], f: CtxValidation[S, W, O], w: W): S => Validator[I, C, O] =
    s => new Validator[I, C, O] {
      def correct = norm.correct
      def validate = c => {
        val orig = norm.validate(c)
        orig.flatMap(o => f(s, w, o).fold(orig)(-\/.apply))
      }
      def c2i = norm.c2i
    }
//  def ValidatorX2_b[S, I, C, O](x2: ValidatorX2[S, I, C, O], f: CtxValidation[S, O]): ValidatorX2[S, I, C, O] =
//    s => new Validator[I, C, O] {
//      private[this] val norm = x2(s)
//      def correct = norm.correct
//      def validate = c => {
//        val orig = norm.validate(c)
//        orig.flatMap(o => f(s, o).fold(orig)(-\/.apply))
//      }
//      def c2i = norm.c2i
//    }

  trait Editor[D, V] {
    def apply(data: D
              , error: Option[ErrorMsg]
              , onChange: D => IO[Unit]
              , onCancel: IO[Unit] => IO[Unit]
              , onEditEnd: IO[Unit]
               ): V
  }

  // TODO create event handling monad?

  // ===================================================================================================================
  // Field attrs

  class FormAttrShit[S, I, C, O, M[_] : Bind : Foldable](
                                  vs: S => Validator[I, C, O] // Conflation. S not required for i↔c
                                  , s2mc: S => M[C]
                                  , iL: WierdLens[M, S, S, I]
                                  , trySave: S => IO[S]
                                  ) {

    private def change(i: I) = (s: S) => getOrElseAP(iL.set(s,i), s)

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
  case class SpecSpliceE[P, V, I, C, O](s: SpecSplice[P, I, C, O], editor: Editor[I, V]) {
//    def x[X](x: CtxValidation[X, O]) = SpecSpliceEX(s, editor, Some(x))
//    def noX[X] = SpecSpliceEX[P, V, I, C, O, X](s, editor, None)
  }

  trait SpecLike[G, P, V] {
    type E
    //type OO
    type VV

//
//    def renderM[S, M[_] : Bind : Foldable](eL: WierdLens[M, S, S, E],
//                                           saveG: (S, G) => IO[S],
//                                           s2mp: S => M[P]
//                                            )(T: ComponentScope_SS[S]): M[VV]

    }

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
                                                     ) extends SpecLike[G,P,V] {
    type E = (I1,I2)
    type OO = (O1, O2)
    type VV = (V, V)

    def initial(p: P): E = (s1.s initial p, s2.s initial p)

//    private def savable(e: E): Option[OO] = for {
//      o1 <- s1.s.savable(e._1)
//      o2 <- s2.s.savable(e._2)
//    } yield (o1,o2)
//
//    private def fieldRenderers[S, M[_] : Bind : Foldable](s2mp: S => M[P],
//                                                  spp: (S, OO) => IO[S],
//                                                  eL: WierdLens[M, S, S, E]) = {
//      val sf: S => IO[S] = s =>
//        foldableToOption(eL.get(s)).flatMap(savable).fold(IO(s))(oo => spp(s, oo))
//      (
//        new FormAttrShit[S, I1, C1, O1, M](_=> s1.s.v, s2mp.andThen(_ map s1.s.p2c), eL map _1[E, I1], sf),
//        new FormAttrShit[S, I2, C2, O2, M](_=> s2.s.v, s2mp.andThen(_ map s2.s.p2c), eL map _2[E, I2], sf)
//        )
//    }
//
//    final def render[S](eL: SimpleLens[S, E],
//                        saveG: (S, G) => IO[S],
//                        s2mp: S => P
//                         )(T: ComponentScope_SS[S]): VV = renderM[S, Id](WierdLens from eL, saveG, s2mp)(T)
//
//    def renderM[S, M[_] : Bind : Foldable](eL: WierdLens[M, S, S, E],
//                                           saveG: (S, G) => IO[S],
//                                           s2mp: S => M[P]
//                                            )(T: ComponentScope_SS[S]): M[VV] = {
//
//      def spp(s: S, oo: OO): IO[S] = saveG(s, oo2g(oo))
//
//      val s = fieldRenderers(s2mp, spp, eL)
//      for {
//        v1 <- s._1.render(s1.editor, T)
//        v2 <- s._2.render(s2.editor, T)
//      } yield (v1,v2)
//    }
  }

  /* **************** */ /* **************** */ /* **************** */
  /* **************** */ /* **************** */ /* **************** */
  /* **************** */ /* **************** */ /* **************** */

//  case class TableSpec2[G, P, V, I1, C1, O1, I2, C2, O2](
//    spec: Spec2[G, P, V, I1, C1, O1, I2, C2, O2],

  /**
   * Field attributes + TABLE STATE + TABLE ROW-ID
   */
  case class Spec2X[S, W, G, P, V, I1, C1, O1, I2, C2, O2](
      spec: Spec2[G, P, V, I1, C1, O1, I2, C2, O2],
      ctxV1: Option[CtxValidation[S, W, O1]],
      ctxV2: Option[CtxValidation[S, W, O2]]
  ) extends SpecLike[G,P,V] {

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

//      def saveOO(s: S, oo: OO): IO[S] = saveG(s, oo2g(oo))

      // TODO This isn't able to traverse other rows, or access them by W
      // Cannot save rows that were invalid due to this one, that have just become valid.

      val sf: S => IO[S] = s =>
        foldableToOption(eL.get(s)).flatMap(savable(s,_)).fold(IO(s))(oo => saveG(s, oo2g(oo)))
      (
        new FormAttrShit[S, I1, C1, O1, M](v1, s2mp.andThen(_ map s1.s.p2c), eL map _1[E, I1], sf),
        new FormAttrShit[S, I2, C2, O2, M](v2, s2mp.andThen(_ map s2.s.p2c), eL map _2[E, I2], sf)
        )
    }

    final def render(eL: SimpleLens[S, E],
                        saveG: (S, G) => IO[S],
                        s2mp: S => P,
                          w: W
                         )(T: ComponentScope_SS[S]): VV = renderM[Id](WierdLens from eL, saveG, s2mp, w)(T)

    def renderM[M[_] : Bind : Foldable](eL: WierdLens[M, S, S, E],
                                           saveG: (S, G) => IO[S],
                                           s2mp: S => M[P],
                                           w: W
                                            )(T: ComponentScope_SS[S]): M[VV] = {

      val s = fieldRenderers(s2mp, w, saveG, eL)
      for {
        v1 <- s._1.render(s1.editor, T)
        v2 <- s._2.render(s2.editor, T)
      } yield (v1,v2)
    }


  }

//  case class TableSpec[M[_] : Bind, S, VV, V, RowId](
//    row: FullRow[M, S, VV, V, RowId],
//    ctxV: Option[CtxValidation[S, O1]]
//                                                      )

//  case class SpecSpliceEX[P, V, I, C, O, X](s: SpecSplice[P, I, C, O], editor: Editor[I, V], ctxV: Option[CtxValidation[X, O]])
//  case class Spec2X[G, P, V, I1, C1, O1, I2, C2, O2, S1, S2, X](
//       s1: SpecSpliceEX[P,V,I1,C1,O1,X], s2: SpecSpliceEX[P,V,I2,C2,O2,X]
//      , oo2g: ((O1, O2)) => G) {
//    type E = (I1, I2)
//    type OO = (O1, O2)
//    type VV = (V, V)
//
//    def initial(p: P): E = (s1.s initial p, s2.s initial p)
//
//    def savable(e: E): Option[OO] = for {
//      o1 <- s1.s.savable(e._1)
//      o2 <- s2.s.savable(e._2)
//    } yield (o1,o2)
//
//    def fieldRenderers[S, M[_] : Bind : Foldable](s2mp: S => M[P],
//                                                  s2x: S => X,
//                                                  spp: (S, OO) => IO[S],
//                                                  eL: WierdLens[M, S, S, E]) = {
//      val sf: S => IO[S] = s =>
//        foldableToOption(eL.get(s)).flatMap(savable).fold(IO(s))(oo => spp(s, oo))
//
//      val v1 = s1.ctxV.fold[ValidatorX2[S,I1,C1,O1]]( _ => s1.s.v )( c=> ValidatorX2(s1.s.v, c contramap s2x) )
//      val v2 = s2.ctxV.fold[ValidatorX2[S,I2,C2,O2]]( _ => s2.s.v )( c=> ValidatorX2(s2.s.v, c contramap s2x) )
//      (
//        new FormAttrShit[S, I1, C1, O1, M](v1, s2mp.andThen(_ map s1.s.p2c), eL map _1[E, I1], sf),
//        new FormAttrShit[S, I2, C2, O2, M](v2, s2mp.andThen(_ map s2.s.p2c), eL map _2[E, I2], sf)
//        )
//    }
//
//    def render[S](eL: SimpleLens[S, E],
//                  saveG: (S, G) => IO[S],
//                  s2mp: S => P,
//                  s2x: S => X
//                   )(T: ComponentScope_SS[S]): VV = renderM[S, Id](WierdLens from eL, saveG, s2mp, s2x)(T)
//
//    def renderM[S, M[_] : Bind : Foldable](eL: WierdLens[M, S, S, E],
//                                           saveG: (S, G) => IO[S],
//                                           s2mp: S => M[P],
//                                           s2x: S => X
//                                            )(T: ComponentScope_SS[S]): M[VV] = {
//
//      def spp(s: S, oo: OO): IO[S] = saveG(s, oo2g(oo))
//
//      val s = fieldRenderers(s2mp, s2x, spp, eL)
//      for {
//        v1 <- s._1.render(s1.editor, T)
//        v2 <- s._2.render(s2.editor, T)
//      } yield (v1,v2)
//    }
//  }

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

  case class SavingThingy[S, G, L, L2, Px](getLast: S => L,
                                           needSave: (L, G) => Option[L2],
                                           saveIO: (L2, G) => IO[Px],
                                           storeSaved: Px => S => S) {
    def save(s: S, g: G): IO[S] = {
      val last = getLast(s)
      needSave(last, g) match {
        case Some(l2) => saveIO(l2, g).map(storeSaved(_)(s))
        case None     => IO(s)
      }
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
    override def correct = _.trim.toUpperCase()
    override def validate = {
      case "" => -\/("It's blank!")
      case s if !s.matches("^[A-Z]+$") => -\/("One word, A-Z only.")
      case s => \/-(s)
    }
    override def c2i = identity
  }

  def uniqueness[S, W, A, I](extract: S => Stream[A], cur: (A, W) => Boolean, cmp: (A, I) => Boolean, errorMsg: ErrorMsg = "Already in use. Duplicate.") =
    new CtxValidation[S, W, I]((s, w, i) => {
      val dupFound = extract(s)
        .filterNot(cur(_, w))
        .exists(cmp(_,i))
        //.foldLeft(0)((j, a) => if (j <= 1 && cmp(a,i)) j + 1 else j) // TODO effeciency, too eager
      if (dupFound) Some(errorMsg) else None
    })

//  def uniquenessRefl[S, A](extract: S => Stream[A]) =
//    uniqueness[S, A, A](extract, (a,b) => a == b)

  object DescValidator extends Validator[String, Option[String], Option[String]] {
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

  val TextInputEditor = new TextEditor(input)
  val TextareaEditor = new TextEditor(textarea)
}
