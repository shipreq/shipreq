
import japgolly.scalajs.react.vdom.{ReactOutput, ReactVDom, VDomBuilder, ReactFragT}
import org.scalajs.dom
import org.scalajs.dom.extensions.KeyCode
import scala.scalajs.js

import japgolly.scalajs.react._
import vdom.ReactVDom._
import all._
import ScalazReact._

import scalaz.effect.IO
import scalaz.{-\/, \/-, \/, State, StateT, Scalaz}
import Scalaz.Id
import scalaz.syntax.bind._

//import golly.ScalazReact._
import monocle._
import monocle.syntax._
import monocle.function.Field1._
import monocle.function.Field2._

import Lib._

/**
 * Done
 * ~~~~
 * escape to cancel change
 * validation as you type
 * input correction (valid or not)
 * saves only when entire row is valid
 *
 * TODO
 * ~~~~
 * [ priority . effort ]
 * [5.5] drag to reorder
 * [5.5] create new
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

  class FormAttrShit[S, I, C, O](
                                  v: Validator[I, C, O]
                                  , s2c: S => C
                                  , siL: SimpleLens[S, I]
                                  , trySave: S => IO[S]
                                  ) {

    private val SM = new StateHelper[S]

    def getSaved = SM.gets(v.c2i compose s2c)

    def change(i: I) = SM.modify(siL.setF(i))

    def cancelChange = getSaved >>= change

    def editEnd =
      StateT[IO, S, Unit](s => {
        val s2 = siL.modify(s, v.c2i compose v.correct)
        trySave(s2).map((_,()))
      })

    def render[V](editor: Editor[I, V], T: ComponentScope_SS[S]): V = {
      val i = siL get T.state
      val e = v.correctAndValidate(i).swap.toOption
      editor(i, e, i => T runStateIO change(i), T runStateIOC cancelChange, T runStateIO editEnd)
    }
  }

  // ===================================================================================================================
  // Spec

  case class SpecSplice[P, I, C, O](p2c: P => C, v: Validator[I, C, O]) {
    def initial: P => I = v.c2i compose p2c
    def savable(i: I) = v.correctAndValidate(i).toOption
    def edit[V](e: Editor[I, V]) = SpecSpliceE(this, e)
  }

  case class SpecSpliceE[P, V, I, C, O](s: SpecSplice[P, I, C, O], editor: Editor[I, V])

  case class Spec2[G, P, V, I1, C1, O1, I2, C2, O2](s1: SpecSpliceE[P,V,I1,C1,O1], s2: SpecSpliceE[P,V,I2,C2,O2]
                                                    , oo2g: ((O1, O2)) => G
                                                    , g2p: (Option[P], G) => IO[P]
                                                     ) {
    type E = (I1,I2)
    type OO = (O1, O2)
    type VV = (V, V)

    def initial(p: P): E = (s1.s initial p, s2.s initial p)

    def savable(e: E): Option[OO] = for {
      o1 <- s1.s.savable(e._1)
      o2 <- s2.s.savable(e._2)
    } yield (o1,o2)

    def shit[S](sp: S => P, spp: (S, OO) => IO[S], se: SimpleLens[S, E]) = {
      val sf: S => IO[S] = s =>
        savable(se get s).fold(IO(s))(oo => spp(s, oo))
      (
        new FormAttrShit[S, I1, C1, O1](s1.s.v, s1.s.p2c compose sp, se |-> _1[E, I1], sf),
        new FormAttrShit[S, I2, C2, O2](s2.s.v, s2.s.p2c compose sp, se |-> _2[E, I2], sf)
      )
    }

    def render[S](x: SimpleLens[S, (P, E)])(T: ComponentScope_SS[S]): VV = {
      val sp = x composeLens _1
      val se = x |-> _2

      def spp(s: S, oo: OO): IO[S] = {
        val g = oo2g(oo)
        val op = sp.getOption(s)
        val iop = g2p(op, g)
        iop.map(p => sp.set(s, p))
      }

      //    def render[S](sp: Lens[S, S, P, OO], se: SimpleLens[S, E])(T: ComponentScope_SS[S]): VV = {
      val s = shit(sp.get _, spp , se)
      (
        s._1.render(s1.editor, T)
        ,s._2.render(s2.editor, T)
        )
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
