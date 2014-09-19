package utily

import org.scalajs.dom.{HTMLInputElement, console}
import org.scalajs.dom.extensions.KeyCode
import scala.runtime.AbstractFunction3
import scalaz.effect.IO
import scalaz.Scalaz.Id
import scalaz.{Foldable, Bind, \/, \/-, -\/, Equal}
import scalaz.std.option._
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import monocle._

import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.client.ui.Util._
import shipreq.webapp.client.ui.{ErrorMsg, InputEvent, Editor}
import japgolly.scalajs.react._
import vdom.ReactVDom._
import all._
import ScalazReact._


/**
 * S = State. Where the subject data lives.
 * W = Row ID.
 */
object EditorStuff {

  class WiredEditor[S, I : Equal, C, O, M[_] : Bind : Foldable](
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

    private def correctInput =
      ReactS.mod[S](s1 => {
        val v = vs(s1)
        val r = for {
          i1 <- iL.getO(s1)
          c = v.correct(i1)
          i2 = v.c2i(c)
          s2 <- iL.setO(s1, i2) if !implicitly[Equal[I]].equal(i1, i2)
        } yield s2
        r getOrElse s1
      })

    val editEnd =
      correctInput.liftIO >> ReactS.modT(trySave)

    def render[V](editor: Editor[I, V], T: ComponentStateFocus[S]): M[V] = {
      val s = T.state
      iL.get(s).map(i => {
        val e = vs(s).correctAndValidate(i).swap.toOption
        editor.render(i, e, change, cancelChange, editEnd, T)
      })
    }
  }

  def getOrElseAP[M[_]: Foldable, A](m: M[A], a: => A): A =
    m.foldr(a)(aa => _ => aa)

  def foldMapAP[M[_]: Foldable, A, B](m: M[A], b: => B)(f: A => B): B =
    m.foldr(b)(a => _ => f(a))

  def foldableToOption[M[_]: Foldable, A](m: M[A]): Option[A] =
    foldMapAP(m, None: Option[A])(Some.apply)

  case class WierdLens[M[_]: Bind : Foldable, S, T, A](get: S => M[A], set: (S, A) => M[T]) {
    def mod(s: S, f: A => A): M[T] = get(s).flatMap(a => set(s, f(a)))
    def map[B](l: SimpleLens[A, B]) = WierdLens[M, S, T, B](
      s => get(s).map(l.get),
      (s,b) => get(s).flatMap(a => set(s, l.set(a, b))))

    def dimap[F, G](f: F => S, g: T=> G) =
      WierdLens[M, F, G, A](get compose f, (b, a) => implicitly[Bind[M]].map(set(f(b), a))(g))

    def getO: S => Option[A] = s => foldableToOption(get(s))
    def setO: (S,A) => Option[T] = (s,a) => foldableToOption(set(s,a))
  }

  object WierdLens {
    def from[S, A](l: SimpleLens[S, A]) = WierdLens[Id, S, S, A](l.get, l.set)
  }

  // ===================================================================================================================
  // Validation

  // TODO not really a validator, data input/recv pipeline/receiver/valve/protocol/rules/gateway/enforcer
  trait Validator[I, C, O] {
    def liveCorrect: I => I
    def correct: I => C
    def validate: C => ErrorMsg \/ O
    def c2i: C => I
    final def correctAndValidate = validate compose correct
  }

  type ValidatorW[S, W, I, C, O] = (S, W) => Validator[I, C, O]

  type ValidateFnW[S, W, O] = (S, W, O) => Option[ErrorMsg]

  def rowValidator[S, W, I, C, O](norm: Validator[I, C, O], f: ValidateFnW[S, W, O], w: W): S => Validator[I, C, O] =
    s => new Validator[I, C, O] {
      def liveCorrect = norm.liveCorrect
      def correct = norm.correct
      def c2i = norm.c2i
      def validate = c => {
        val orig = norm.validate(c)
        orig.flatMap(o => f(s, w, o).fold(orig)(-\/.apply))
      }
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

  object ReqNameValidator extends Validator[String, String, String] {
    override def liveCorrect = identity
    override def correct = _.trim
    override def validate = {
      case "" => -\/("It's blank!")
      case s => \/-(s)
    }
    override def c2i = identity
  }

  class NopValidator[I] extends Validator[I,I,I] {
    override def liveCorrect = identity
    override def correct = identity
    override def validate = \/-.apply
    override def c2i = identity
  }
  def NopValidator[I] = new NopValidator[I]

  // TODO Delete this? If keeping add a version where A=B and Equal is used
  def uniqueness[S, W, A, B](extract: (S,W) => Stream[A], cmp: (A, B) => Boolean, errorMsg: ErrorMsg = "Already in use. Duplicate."): ValidateFnW[S,W,B] =
    (s, w, b) => {
      val dupFound = extract(s, w).exists(cmp(_,b))
      //.foldLeft(0)((j, a) => if (j <= 1 && cmp(a,i)) j + 1 else j) // TODO effeciency, too eager
      if (dupFound) Some(errorMsg) else None
    }

  object DescValidator extends Validator[String, Option[String], Option[String]] {
    override def liveCorrect = identity
    override def c2i = _ getOrElse ""
    override def correct = s => {
      val j = s.trim
      if (j.isEmpty) None else Some(j)
    }
    override def validate = \/-(_)
  }

}
