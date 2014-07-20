
import japgolly.scalajs.react.vdom.{ReactOutput, ReactVDom, VDomBuilder, ReactFragT}
import org.scalajs.dom
import org.scalajs.dom.extensions.KeyCode
import scala.scalajs.js

import japgolly.scalajs.react._
import vdom.ReactVDom._
import all._

import scalaz.effect.IO
import scalaz.{-\/, \/-, \/, State, StateT, Scalaz}
import Scalaz.Id
import scalaz.syntax.bind._

//import golly.ScalazReact._
import monocle._
import monocle.syntax._
import monocle.function.Field1._
import monocle.function.Field2._

object Xxx {
  type SSetter[S, A] = Setter[S, S, _, A]

  implicit final class ComponentScope_SS_Ext3[S](val u: ComponentScope_SS[S]) extends AnyVal {

//    @inline def setStateL [V](l: Setter[S, S, _, V])(v: V)                    = u.modState((s: S) => l.set(s, v))
//    @inline def setStateLC[V](l: Setter[S, S, _, V])(v: V)(callback: => Unit) = u.modState((s: S) => l.set(s, v), callback)
    @inline def setStateL[V](l: Setter[S, S, _, V])(v: V)                    = u.modState(l.set(_, v))
    @inline def setStateL[V](l: Setter[S, S, _, V], callback: => Unit)(v: V) = u.modState(l.set(_, v), callback)

    // Using StateT[Id] instead of State so that Intellij doesn't paint the entire screen red
    @inline def runState(m: StateT[Id, S, _])                    = u.modState(m(_)._1)
    @inline def runState(m: StateT[Id, S, _], callback: => Unit) = u.modState(m(_)._1, callback)
    @inline def runStateC(m: StateT[Id, S, _])(callback: () => Unit) = runState(m, callback())
    @inline def runStateF[V](f: V => StateT[Id, S, _])                    = (v: V) => u.runState(f(v))
    @inline def runStateF[V](f: V => StateT[Id, S, _], callback: => Unit) = (v: V) => u.runState(f(v), callback)

    @inline def runStateIO(m: StateT[IO, S, _]) = u.modState(s => m.run(s).unsafePerformIO()._1)
  }

  def textChangeRecv(f: String => Unit): SyntheticEvent[dom.HTMLInputElement] => Unit = e => f(e.target.value)
  def textChangeRecvL[State](t: ComponentScope_SS[State], l: Setter[State, State, _, String]) =
    textChangeRecv(t setStateL l)

  def SimpleLens2[T] = new {
    def apply[A](f: T => A) = new {
      def apply(g: (T, A) => T) = SimpleLens[T, A](f, g)
    }
  }

//  implicit class MonOptionalExt[S, T, A, B](val o: Optional[S, T, A, B]) extends AnyVal {
//    def modifyOptionF(f: A => B)(from: S) = o.modifyOption(from, f)
//    def setOptionF(newValue: B)(from: S) = o.setOption(from, newValue)
//  }
  implicit class MonSetterExt[S, T, A, B](val o: Setter[S, T, A, B]) extends AnyVal {
    final def setF(newValue: B) = (from: S) => o.set(from, newValue)
    final def modifyF(f: A => B) = (from: S) => o.modify(from, f)
  }

  class StateHelper[S] {
    @inline final def apply[A](f: S => (S, A))        = State.apply(f)
    @inline final def constantState[A](a: A, s: => S) = State.constantState(a, s)
    @inline final def state[A](a: A)                  = State.state(a)
    @inline final def init                            = State.init[S]
    @inline final def get                             = State.get[S]
    @inline final def gets[A](f: S => A)              = State.gets(f)
    @inline final def put(s: S)                       = State.put(s)
    @inline final def modify(f: S => S)               = State.modify(f)
  }

}
import Xxx._

object Phase2 extends js.JSApp {
  override def main(): Unit = {
    import IssueConfig._
    IssueTypeTable(List(
      1L -> UserDefIssueType("TODO", None)
      ,2L -> UserDefIssueType("TBD", Some("To Be Decided."))
    )) render dom.document.getElementById("target")
  }

  import dom.console

  //    case class Dot(name: String)
  //    case class Example(map: Map[Long, Dot])
  //    val mapL = SimpleLens[Example, Map[Long, Dot]](_.map, (a,b) => a.copy(map = b))
  //    val dotNameL = SimpleLens[Dot, String](_.name, (a,b) => a.copy(name = b))
  //    val z1 = mapL composeOptional index(1234L)
  //    val z2 = mapL |-? index(1234L)

  // ===================================================================================================================
  // Field stuff

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
              , onChange: D => Unit
              , onCancel: (() => Unit) => Unit
              , onEditEnd: => Unit
              //, validator: Validator[D, _] // or just
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
      editor(i, e, T runStateF change, T runStateC cancelChange, T runStateIO editEnd)
    }
  }

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

  // saves only when entire row is valid
  // escape to cancel change
  // validation as you type
  // correction

  // ===================================================================================================================

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
                       , onChange: String => Unit
                       , onCancel: (() => Unit) => Unit
                       , onEditEnd: => Unit
                        ) = {

      val cancelOnEscape: SyntheticEvent[dom.HTMLInputElement] => Unit =
        e => if (e.keyboardEvent.keyCode == KeyCode.escape) {
          e.preventDefault()
          e.stopPropagation()
          val t = e.target
          onCancel(() => t.blur())
        }

      div(
        node(
          value := data
          , error.isDefined && (cls := "error")
          , onchange ==> textChangeRecv(onChange)
          , onblur --> onEditEnd
          , onkeydown ==> cancelOnEscape
        )
        , error.fold(Nop)(e => div(cls := "errorMsg")(e))
      )
    }
  }

  val TextInputEditor = new TextEditor(input)
  val TextareaEditor = new TextEditor(textarea)

  // ===================================================================================================================
  // StateMonad

  object IssueConfig {

    type UserDefIssueTypeId = Long

    case class UserDefIssueType(key: String, desc: Option[String])
    //type UserDefIssueTypeWithId = (UserDefIssueTypeId, UserDefIssueType)
    val keyL = SimpleLens2[UserDefIssueType](_.key)((a, b) => a.copy(key = b))
    val descL = SimpleLens2[UserDefIssueType](_.desc)((a, b) => a.copy(desc = b))

    val SPEC = Spec2(
      SpecSplice(keyL.get _, KeyValidator, TextInputEditor)
      , SpecSplice(descL.get _, DescValidator, TextareaEditor)
      , UserDefIssueType.apply, fakeSave
    )

    def fakeSave(p: Option[UserDefIssueType], g: UserDefIssueType) = IO {
      console.log(s"SAVING $p ⇒ $g")
      g
    }

    type IssueTypeTableS = Map[UserDefIssueTypeId, (UserDefIssueType, SPEC.E)]
    def rowL(id: UserDefIssueTypeId) = SimpleLens2[IssueTypeTableS](_(id))((a,b) => a + (id -> b))

    val IssueTypeTable = ReactComponentB[List[(UserDefIssueTypeId, UserDefIssueType)]]("IssueTypeTable")
      .getInitialState[IssueTypeTableS](_.map(x => x._1 -> (x._2, SPEC.initial(x._2))).toMap)
      .render(T => {
        val S = T.state
        console.log(s"State = $S")

        def row(id: UserDefIssueTypeId, s: UserDefIssueType) = {
          val (key, desc) = SPEC.render(rowL(id))(T)
          val ctrls = raw(s"${s.key} | ${s.desc}")
          tr(keyAttr := id)(td(key), td(desc), td(ctrls))
        }

        table(tbody(
          tr(th("Name"), th("Description"), th("Ctrls"))
          , S.toList.sortBy(_._2._1.key).map(x => row(x._1, x._2._1)).toJsArray
        ))
      }).create
    }
}