package utily

import org.scalajs.dom.extensions.KeyCode
import scala.runtime.AbstractFunction3
import scalaz.effect.IO
import scalaz.Scalaz.Id
import scalaz.{Foldable, Bind, \/, \/-, -\/}
import scalaz.std.option._
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import monocle._

import Lib._
import japgolly.scalajs.react._
import vdom.ReactVDom._
import all._
import ScalazReact._

object EditorStuff {

  class FormAttrShit[S, I, C, O, M[_] : Bind : Foldable](
      vs: S => Validator[I, C, O] // Conflation. S not required for i↔c
      , s2mc: S => M[C]
      , iL: WierdLens[M, S, S, I]
      , trySave: S => IO[S]
      ) {

    def resolvem[A](m: M[A])(f: A => ReactS[S, Unit]): ReactS[S, Unit] =
      foldMapAP(m, ReactS.ret[S, Unit](()))(f)

    def resolvemio[A](m: M[A])(f: A => ReactST[IO, S, Unit]): ReactST[IO, S, Unit] =
      foldMapAP(m, ReactS.retT[IO, S, Unit](()))(f)

    private def change(i: I) = ReactS.mod((s:S) => getOrElseAP(iL.set(s, vs(s).liveCorrect(i)), s))

    private def cancelChange(callback: IO[Unit]) =
      ReactS.get[S].flatMap(s =>
        resolvem(s2mc(s))(c => change(vs(s) c2i c) addCallback callback)
      )

    private def editEnd =
      ReactS.getT[IO, S].flatMap(s => {
        val v = vs(s)
        // optimisation: compare I & I here, don't try to save if equal
        // correctness, don't try to save if invalid
        val ms = iL.mod(s, v.c2i compose v.correct)
        resolvemio(ms)(s => ReactS.modT(trySave))
      })

    def render[V](editor: Editor[I, V], T: ComponentStateFocus[S]): M[V] = {
      val s = T.state
      iL.get(s).map(i => {
        val e = vs(s).correctAndValidate(i).swap.toOption
        editor(i, e, change, cancelChange, editEnd, T)
      })
    }
  }

  def getOrElseAP[M[_]: Foldable, A](m: M[A], a: => A): A =
    m.foldr(a)(aa => _ => aa)

  def foldMapAP[M[_]: Foldable, A, B](m: M[A], b: => B)(f: A => B): B =
    m.foldr(b)(a => _ => f(a))

  case class WierdLens[M[_]: Bind, S, T, A](get: S => M[A], set: (S, A) => M[T]) {
    def mod(s: S, f: A => A): M[T] = get(s).flatMap(a => set(s, f(a)))
    def map[B](l: SimpleLens[A, B]) = WierdLens[M, S, T, B](
      s => get(s).map(l.get),
      (s,b) => get(s).flatMap(a => set(s, l.set(a, b))))

    def dimap[F, G](f: F => S, g: T=> G) =
      WierdLens[M, F, G, A](get compose f, (b, a) => implicitly[Bind[M]].map(set(f(b), a))(g))
  }

  object WierdLens {
    def from[S, A](l: SimpleLens[S, A]) = WierdLens[Id, S, S, A](l.get, l.set)
  }

  // ===================================================================================================================
  // Validation

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

  // ===================================================================================================================
  // Editors

  trait Editor[D, V] {
    def apply[S](data: D
                 , error: Option[ErrorMsg]
                 , onChange: D => ReactST[IO, S, Unit]
                 , onCancel: IO[Unit] => ReactST[IO, S, Unit]
                 , onEditEnd: ReactST[IO, S, Unit]
                 , T: ComponentStateFocus[S]
                  ): V
  }


  class TextEditor(node: Tag) extends Editor[String, Modifier] {
    override def apply[S](data: String
                          , error: Option[ErrorMsg]
                          , onChange: String => ReactST[IO, S, Unit]
                          , onCancel: IO[Unit] => ReactST[IO, S, Unit]
                          , onEditEnd: ReactST[IO, S, Unit]
                          , T: ComponentStateFocus[S]
                           ) = {

      val cancelOnEscape: InputEvent => ReactST[IO, S, Unit] =
        e => e.keyboardEvent
          .filter(_.keyCode == KeyCode.escape)
          .fold(ReactS.retT[IO,S,Unit](()))(_ => {
          val t = e.target
          ReactS.retM[IO, S, Unit](e.preventDefaultIO >> e.stopPropagationIO) >> onCancel(IO(t.blur()))
        })

      div(
        node(
          value := data
          , error.isDefined && (cls := "error")
          , onchange  ~~> T._runState(textChangeRecvX(onChange))
          , onkeydown ~~> T._runState(cancelOnEscape)
          , onblur    ~~> T.runState(onEditEnd)
        )
        , error.fold(Nop)(e => div(cls := "errorMsg")(e))
      )
    }
  }

  object CheckboxEditor extends Editor[Boolean, Modifier] {
    override def apply[S](data: Boolean
                          , error: Option[ErrorMsg]
                          , onChange: Boolean => ReactST[IO, S, Unit]
                          , onCancel: IO[Unit] => ReactST[IO, S, Unit]
                          , onEditEnd: ReactST[IO, S, Unit]
                          , T: ComponentStateFocus[S]
                           ) = {
      def ch(e: InputEvent) = {
        val v = e.target.checked
        onChange(v) >> onEditEnd
      }

      div(
        checkbox(data)(
          onchange ~~> T._runState(ch),
          error.isDefined && (cls := "error")
        ),
        error.fold(Nop)(e => div(cls := "errorMsg")(e))
      )
    }
  }


  val TextInputEditor = new TextEditor(input)
  val TextareaEditor = new TextEditor(textarea)
}
