import org.scalajs.dom
import org.scalajs.dom.console
import scala.scalajs.js
import scalaz.effect.IO
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._
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