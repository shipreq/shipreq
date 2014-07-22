import monocle._
import monocle.syntax._
import monocle.function.Field1._
import monocle.function.Field2._
import org.scalajs.dom
import org.scalajs.dom.console
import scala.scalajs.js
import scalaz.{State, StateT, Scalaz}
import scalaz.std.option.optionInstance
import Scalaz.Id
import scalaz.effect.IO
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._
import japgolly.scalajs.react.ScalazReact._
import FormStuff._
import Lib._

object Phase2 extends js.JSApp {
  override def main(): Unit = {
    import Phase2.IssueConfig._
    IssueTypeTable(List(
      1L -> UserDefIssueType("TODO", None)
      ,2L -> UserDefIssueType("TBD", Some("To Be Decided."))
    )) render dom.document.getElementById("target")
  }


  object IssueConfig {

    type UserDefIssueTypeId = Long

    type S = FormState
    type E = SPEC.E
    type P = UserDefIssueType
    type G = UserDefIssueType
    type Px = (UserDefIssueTypeId, P)

    case class UserDefIssueType(key: String, desc: Option[String])
    //type UserDefIssueTypeWithId = (UserDefIssueTypeId, UserDefIssueType)
    val keyL = SimpleLens2[UserDefIssueType](_.key)((a, b) => a.copy(key = b))
    val descL = SimpleLens2[UserDefIssueType](_.desc)((a, b) => a.copy(desc = b))

    val SPEC = Spec2(
      SpecSplice(keyL.get _, KeyValidator).edit(TextInputEditor),
      SpecSplice(descL.get _, DescValidator).edit(TextareaEditor),
      (UserDefIssueType.apply _).tupled)

    def fakeSave(p: Option[Px], g: UserDefIssueType) = IO[Px] {
      console.log(s"SAVING $p ⇒ $g")
      val newId = p.fold[UserDefIssueTypeId](666L)(_._1)
      (newId, g)
    }

    def empty: SPEC.E = ("","")

    def createS = State.modify[FormState](unsavedL.modifyF(_ orElse Some(empty)))

    type Unsaved = Option[E]
    type SaveMap = Map[UserDefIssueTypeId, (P, E)]

    case class FormState(saved: SaveMap, unsaved: Unsaved)
    val savedL = SimpleLens2[FormState](_.saved)((a,b) => a.copy(saved = b))
    val unsavedL = SimpleLens2[FormState](_.unsaved)((a,b) => a.copy(unsaved = b))

    def mkPE(p: P) = (p, SPEC initial p)
    def storePx(s: S, prev: Option[Px], px: Px): S = {
      var f = savedL.modifyF(_ + (px._1 -> mkPE(px._2)))
      if (prev.isEmpty) f = f compose unsavedL.setF(None)
      f(s)
    }

    def rowL(id: UserDefIssueTypeId) = savedL composeLens SimpleLens2[SaveMap](_(id))((a,b) => a + (id -> b))

    val newRowRenderer = {
      val s2op: S => Option[P] = _ => None
      def setE(s:S, e:E): Option[S] = unsavedL.get(s).map(_ => unsavedL.set(s, Some(e)))
      //            unsavedL.get(s).map(_ => unsavedL.modify(s, _.map(_ => e)))
      val se = WierdLens[Option, S, S, E](unsavedL.get, setE)
      val saverr = SavingThingy[S, G, Option[Px], Px]((a,b) => true, fakeSave, _=>None, storePx)
      SPEC.renderM(se, saverr.save, s2op) _
    }

    val IssueTypeTable = ReactComponentB[List[(UserDefIssueTypeId, UserDefIssueType)]]("IssueTypeTable")
      .getInitialState(p => FormState(p.map(x => x._1 -> mkPE(x._2)).toMap, None))
      .render(T => {
        val S = T.state
        console.log(s"State = $S")

        def row(id: UserDefIssueTypeId, s: UserDefIssueType) = {
          val l: SimpleLens[S, (P, E)] = rowL(id)
          val sp: SimpleLens[S, P] = l |-> _1
          val se: SimpleLens[S, E] = l |-> _2
          val getPx: S => Option[Px] = s => Some(id, sp get s)
          val saverr = SavingThingy[S, G, Option[Px], Px]((a,b) => a.fold(true)(_._2 != b), fakeSave, getPx, storePx)

          val (key, desc) = SPEC.render(se, saverr.save, sp.getOption)(T)
          val ctrls = raw(s"${s.key} | ${s.desc}")
          tr(keyAttr := id)(td(key), td(desc), td(ctrls))
        }

        def newRow =
          newRowRenderer(T).map {
            case (key, desc) =>
              val ctrls = raw(S.unsaved.toString)
              tr(keyAttr := "new")(td(key), td(desc), td(ctrls))
          }

        val rows = S.saved.toList.sortBy(_._2._1.key)

        div(
          button(onclick ~~> T.runStateIO(createS))("Create"),
          table(tbody(
            tr(th("Name"), th("Description"), th("Ctrls"))
            , newRow
            , rows.map(x => row(x._1, x._2._1)).toJsArray
          ))
        )
      }).create
    }
}