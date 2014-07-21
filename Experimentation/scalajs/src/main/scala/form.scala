
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

// saves only when entire row is valid
// escape to cancel change
// validation as you type
// correction

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

  // TODO This should validate the entire row and correct its input. (optionally)
  class FormAttrShit[S, I, C, O](
                                  v: Validator[I, C, O]
                                  , pL: S => C
                                  , eL: SimpleLens[S, I]
                                  , trySave: S => IO[S]
                                  ) {

    private val SM = new StateHelper[S]

    def getSaved = SM.gets(v.c2i compose pL)

    def change(i: I) = SM.modify(eL.setF(i))

    def cancelChange = getSaved >>= change

    def editEnd =
      StateT[IO, S, Unit](s => {
        val c = v.correct(eL.get(s))
        val mod1 = eL.setF(v.c2i(c))

        trySave(mod1(s)).map((_,()))
        //        val mod2 = v.validate(c) match {
        //          case -\/(_) => identity[S]_
        //          case \/-(o) => pL.setF(o)
        //        }
        //        (mod1 compose mod2)(s)
      })

    def render[V](editor: Editor[I, V], T: ComponentScope_SS[S]): V = {
      val i = eL get T.state
      val e = v.correctAndValidate(i).swap.toOption
      editor(i, e, i => T runStateIO change(i), T runStateIOC cancelChange, T runStateIO editEnd)
    }
  }

  // ===================================================================================================================
  // Spec

  case class SpecSplice[P, V, I, C, O](pL: P => C, v: Validator[I, C, O], editor: Editor[I, V]) {
    def initial: P => I = v.c2i compose pL
    def savable(i: I) = v.correctAndValidate(i).toOption
  }

  case class Spec2[G, P, V, I1, C1, O1, I2, C2, O2](s1: SpecSplice[P,V,I1,C1,O1], s2: SpecSplice[P,V,I2,C2,O2]
                                                    , o2g: (O1, O2) => G
                                                    , g2p: (Option[P], G) => IO[P]
                                                     ) {
    type E = (I1,I2)
    type OO = (O1, O2)
    type VV = (V, V)

    def initial(p: P): E = (s1 initial p, s2 initial p)

    def savable(e: E): Option[OO] = for {
      o1 <- s1.savable(e._1)
      o2 <- s2.savable(e._2)
    } yield (o1,o2)

    //    def trySave[S](sp: S => IO[SSetter[S, OO], se: SimpleLens[S, E])(S: S): Option[S] =
    //      savable(se get S).map(oo => sp.set(S, oo))

    def shit[S](sp: S => P, spp: (S, OO) => IO[S], se: SimpleLens[S, E]) = {
      val sf: S => IO[S] = s =>
        savable(se get s).fold(IO(s))(oo => spp(s, oo))

      (
        new FormAttrShit[S, I1, C1, O1](s1.v, s1.pL compose sp, se |-> _1[E, I1], sf),
        new FormAttrShit[S, I2, C2, O2](s2.v, s2.pL compose sp, se |-> _2[E, I2], sf)
        )
    }

    val oo2g: OO => G = o => o2g(o._1, o._2)

    def render[S](x: SimpleLens[S, (P, E)])(T: ComponentScope_SS[S]): VV = {
      //      val x2 = Lens[P, P, P, OO](p => p, (p,o) => g2p(Some(p), oo2g(o)))
      //      render(x composeLens _1 composeLens x2, x |-> _2)(T)
      //    }
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
