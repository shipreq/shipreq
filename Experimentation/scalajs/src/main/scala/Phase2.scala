
import japgolly.scalajs.react.vdom.{ReactOutput, ReactVDom, VDomBuilder, ReactFragT}
import org.scalajs.dom
import scala.scalajs.js

import japgolly.scalajs.react._
import vdom.ReactVDom._
import all._

import scalatags.generic.AttrValue
import scalaz.{-\/, \/-, \/, State, StateT, Scalaz}
import Scalaz.Id
import scalaz.syntax.bind._

//import golly.ScalazReact._
import monocle._
import monocle.syntax._
import monocle.function.Index._
import monocle.function.Field1._
import monocle.function.Field2._
import monocle.function.HeadOption.{headOption, optionHeadOption} // to use headOption

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
  }

  val nonEmptyStringIso = Iso((_: Option[String]) getOrElse "", (s: String) => {
    val j = s.trim
    if (j.isEmpty) None else Some(j)
  })

//    def nonEmptyStringLens[A](l: SimpleLens[A, Option[String]]): SimpleLens[A, String] =
//      l |-> nonEmptyStringIso
//    l.xmapB(_ getOrElse "")((i: String) => {
//      val j = i.trim
//      if (j.isEmpty) None else Some(j)
//    })

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
      UserDefIssueType(1, "TODO", None)
      ,UserDefIssueType(2, "TBD", Some("To Be Decided."))
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

  object TextInputEditor extends Editor[String, ReactVDom.Modifier] {
    override def apply(data: String
                       , error: Option[ErrorMsg]
                       , onChange: String => Unit
                       , onCancel: (() => Unit) => Unit
                       , onEditEnd: => Unit
                        ) = {

      val cancelOnEscape: SyntheticEvent[dom.HTMLInputElement] => Unit =
        e => if (e.keyboardEvent.keyCode == 27) {
          e.preventDefault()
          e.stopPropagation()
          val t = e.target
          onCancel(() => t.blur())
        }

      div(
        input(
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

  // ===================================================================================================================
  // StateMonad

  object IssueConfig {

    type UserDefIssueTypeId = Long

    case class UserDefIssueType(id: UserDefIssueTypeId, key: String, desc: Option[String])
    val keyL = SimpleLens2[UserDefIssueType](_.key)((a, b) => a.copy(key = b))
    val descL = SimpleLens2[UserDefIssueType](_.desc)((a, b) => a.copy(desc = b))

//    val keyS = SpecSplice(keyL, KeyValidator, TextInputEditor)
//    val descS = SpecSplice(descL, DescValidator, TextInputEditor)
//    val SPEC = Spec2(keyS, descS)

    val SPEC = Spec2(
      SpecSplice(keyL, KeyValidator, TextInputEditor)
      , SpecSplice(descL, DescValidator, TextInputEditor)
    )

    type IssueTypeTableS = Map[UserDefIssueTypeId, (UserDefIssueType, SPEC.E)]
    def _ind(id: UserDefIssueTypeId) = SimpleLens2[IssueTypeTableS](_(id))((a,b) => a + (id -> b))
    def saveL(id: UserDefIssueTypeId) = _ind(id) composeLens _1
    def editL(id: UserDefIssueTypeId) = _ind(id) composeLens _2

    // TODO This should validate the entire row and correct its input. (optionally)
    class FormAttrShit[S, I, C, O](
          v: Validator[I, C, O]
        , pL: Lens[S, S, C, O]
        , eL: SimpleLens[S, I]
        ) {

      private val SM = new StateHelper[S]

      def getSaved = SM.gets(v.c2i compose pL.get)

      def change(i: I) = SM.modify(eL.setF(i))

      def cancelChange = getSaved >>= change

      def editEnd =
        SM.modify(s => {
          val c = v.correct(eL.get(s))
          val mod1 = eL.setF(v.c2i(c))
          val mod2 = v.validate(c) match {
              case -\/(_) => identity[S]_
              case \/-(o) => pL.setF(o)
            }
          (mod1 compose mod2)(s)
        })

      def render[V](editor: Editor[I, V], T: ComponentScope_SS[S]): V = {
        val S = T.state
        val i = eL.get(S)
        val e = v.correctAndValidate(i).swap.toOption
        editor(i, e, T runStateF change, T runStateC cancelChange, T runState editEnd)
      }
    }

    // ============================================================================================================

    // P -> [Cₙ -> Iₙ]
    // ↑ ↑ ↑
    // Cₙ -> Iₙ
    // P -> Cₙ

//    case class SpecSplice[P, C, I](pL: Getter[P, C], c2i: C => I) {
    case class SpecSplice[P, V, I, C, O](pL: Lens[P, P, C, O], v: Validator[I, C, O], editor: Editor[I, V]) {
      def initial: P => I = v.c2i compose pL.get
  }
    
    case class Spec2[P, V, I1, C1, O1, P2, I2, C2, O2](s1: SpecSplice[P,V,I1,C1,O1], s2: SpecSplice[P,V,I2,C2,O2]) {
      type E = (I1,I2)
      def initial(p: P): E = (s1 initial p, s2 initial p)

      def shit[S](sp: SimpleLens[S, P], se: SimpleLens[S, E]) =
        (
          new FormAttrShit[S, I1, C1, O1](s1.v, sp |-> s1.pL, se |-> _1[E, I1]),
          new FormAttrShit[S, I2, C2, O2](s2.v, sp |-> s2.pL, se |-> _2[E, I2])
          )

      def render[S](sp: SimpleLens[S, P], se: SimpleLens[S, E])(T: ComponentScope_SS[S]) = {
        val s = shit(sp, se)
        (
          s._1.render(s1.editor, T)
          ,s._2.render(s2.editor, T)
          )
      }
    }

    val IssueTypeTable = ReactComponentB[List[UserDefIssueType]]("IssueTypeTable")
      .getInitialState[IssueTypeTableS](_.map(x => x.id -> (x, SPEC.initial(x))).toMap)
      .render(T => {
        val S = T.state
        console.log(s"State = $S")

        type Row = (Modifier, Modifier, Modifier)
        def row(s: UserDefIssueType) = {

          // order dependent, splice.get(spec) should return each
          val (key, desc) = SPEC.render(saveL(s.id), editL(s.id))(T)

          val ctrls = raw(s"${s.key} | ${s.desc}")

          tr(keyAttr := s.id)(td(key), td(desc), td(ctrls))
        }

        table(tbody(
          tr(th("Name"), th("Description"), th("Ctrls"))
          , S.values.toList.map(_._1).sortBy(_.key).map(row).toJsArray
        ))
      }).create
    }


}