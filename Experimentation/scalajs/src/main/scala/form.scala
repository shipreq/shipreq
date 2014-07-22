
import japgolly.scalajs.react.vdom.{ReactOutput, ReactVDom, VDomBuilder, ReactFragT}
import org.scalajs.dom
import org.scalajs.dom.extensions.KeyCode
import scala.scalajs.js

import japgolly.scalajs.react._
import vdom.ReactVDom._
import all._
import ScalazReact._

import scalaz.effect.IO
import scalaz.Scalaz.Id
import scalaz.{Foldable, Bind, \/, \/-, -\/}
import scalaz.syntax.bind._
import scalaz.syntax.foldable._

//import golly.ScalazReact._
import monocle._
import monocle.syntax._
import monocle.function.Field1._
import monocle.function.Field2._

import Lib._

/**
 * Done
 * ~~~~
 * [2] escape to cancel change
 * [4] validation as you type
 * [4] input correction (valid or not)
 * [5] saves only when entire row is valid
 * [5] create new
 *
 * TODO
 * ~~~~
 * [ priority . effort ]
 * [5.5] drag to reorder
 * [5.3] delete
 * [5.2] show/hide deleted
 * [3.5] different view when field not in edit (sometimes the edit view is too noisy)
 * [3.5] field validity depending on other fields
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

  private def getOrElseAP[M[_]: Foldable, A, B](m: M[A], b: => B, f: A => B): B =
    m.foldr(b)(a => _ => f(a))

  private def foldableToOption[M[_]: Foldable, A](m: M[A]): Option[A] =
    getOrElseAP[M, A, Option[A]](m, None, Some(_))


  type ErrorMsg = String

  trait Validator[I, C, O] {
    def correct: I => C
    def validate: C => ErrorMsg \/ O
    final def correctAndValidate = validate compose correct
    def c2i: C => I
  }

  trait Editor[D, V] {
    def apply(data: D
              , error: Option[ErrorMsg]
              , onChange: D => IO[Unit]
              , onCancel: IO[Unit] => IO[Unit]
              , onEditEnd: IO[Unit]
               ): V
  }

  // TODO create event handling monad?

  class FormAttrShit[S, I, C, O, M[_] : Bind : Foldable](
                                  v: Validator[I, C, O]
                                  , s2oc: S => Option[C]
                                  , iL: WierdLens[M, S, S, I]
                                  , trySave: S => IO[S]
                                  ) {

    private def change(i: I) = (s: S) => getOrElseAP(iL.set(s,i), s)

    private def cancelChange(T: ComponentScope_SS[S])(c: IO[Unit]): IO[Unit] =
      T.stateIO.flatMap(s =>
        s2oc(s).map(v.c2i) match {
          case None => IO(())
          case Some(i) => T.modStateIO(change(i), c)
        }
      )

    private def editEnd(T: ComponentScope_SS[S]): IO[Unit] =
      T.stateIO.flatMap(s => {
        // optimisation: compare I & I here, don't try to save if equal
        val m = iL.mod(s, v.c2i compose v.correct).map(trySave(_) >>= T.setStateIO)
        getOrElseAP(m, IO(()))
      })

    def render[V](editor: Editor[I, V], T: ComponentScope_SS[S]): M[V] =
      iL.get(T.state).map(i => {
          val e = v.correctAndValidate(i).swap.toOption
          editor(i, e, i => T modStateIO change(i), cancelChange(T), editEnd(T))
      })
  }

  // ===================================================================================================================
  // Spec

  case class SpecSplice[P, I, C, O](p2c: P => C, v: Validator[I, C, O]) {
    def initial: P => I = v.c2i compose p2c
    def savable(i: I) = v.correctAndValidate(i).toOption
    def edit[V](e: Editor[I, V]) = SpecSpliceE(this, e)
  }

  case class SpecSpliceE[P, V, I, C, O](s: SpecSplice[P, I, C, O], editor: Editor[I, V])

  /**
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

    def savable(e: E): Option[OO] = for {
      o1 <- s1.s.savable(e._1)
      o2 <- s2.s.savable(e._2)
    } yield (o1,o2)

    def fieldRenderers[S, M[_] : Bind : Foldable](s2op: S => Option[P],
                                                  spp: (S, OO) => IO[S],
                                                  eL: WierdLens[M, S, S, E]) = {
      val sf: S => IO[S] = s =>
        foldableToOption(eL.get(s)).flatMap(savable).fold(IO(s))(oo => spp(s, oo))
      (
        new FormAttrShit[S, I1, C1, O1, M](s1.s.v, s2op.andThen(_ map s1.s.p2c), eL map _1[E, I1], sf),
        new FormAttrShit[S, I2, C2, O2, M](s2.s.v, s2op.andThen(_ map s2.s.p2c), eL map _2[E, I2], sf)
        )
    }

    def render[S](eL: SimpleLens[S, E],
                  saveG: (S, G) => IO[S],
                  s2op: S => Option[P]
                   )(T: ComponentScope_SS[S]): VV = renderM[S, Id](WierdLens from eL, saveG, s2op)(T)

    def renderM[S, M[_] : Bind : Foldable](eL: WierdLens[M, S, S, E],
                                           saveG: (S, G) => IO[S],
                                           s2op: S => Option[P]
                                            )(T: ComponentScope_SS[S]): M[VV] = {

      def spp(s: S, oo: OO): IO[S] = saveG(s, oo2g(oo))

      val s = fieldRenderers(s2op, spp, eL)
      for {
        v1 <- s._1.render(s1.editor, T)
        v2 <- s._2.render(s2.editor, T)
      } yield (v1,v2)
    }
  }

  // ===================================================================================================================

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
