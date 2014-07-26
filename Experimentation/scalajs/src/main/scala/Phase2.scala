import monocle._
import monocle.syntax._
import monocle.function.Field1._
import monocle.function.Field2._
import org.scalajs.dom
import org.scalajs.dom.console
import scala.scalajs.js
import scalaz.{State, StateT, Scalaz, Bind}
import scalaz.syntax.bind._
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

    {
      import Phase2.IssueConfig._
      IssueTypeTable(List(
        1L -> CustomIssueType("TODO", None)
        , 2L -> CustomIssueType("TBD", Some("To Be Decided."))
      )) render dom.document.getElementById("target")
    }

    {
      import Phase2.ReqTypes._
      ReqTypeTableComp(List(
        CustomReqType(1, "CO", "Constraint", Set.empty, false, true)
        , CustomReqType(2, "MF", "Major Feature", Set.empty, false, true)
        , CustomReqType(3, "FR", "Functional Requirement", Set.empty, false, true)
        , CustomReqType(4, "BR", "Business Rule", Set.empty, false, true)
        , CustomReqType(5, "DD", "Data Definition", Set("DA","DDF"), false, true)
      )) render dom.document.getElementById("target2")
    }

    DragAndDrop.Component(List(
      DragAndDrop.Item(10, "Ten")
      ,DragAndDrop.Item(20, "Two Zero")
      ,DragAndDrop.Item(30, "Firty")
      ,DragAndDrop.Item(40, "Thorty")
      ,DragAndDrop.Item(50, "Fipty")
    )) render dom.document.getElementById("target3")
  }

  // ===================================================================================================================

  object IssueConfig {

    type CustomIssueTypeId = Long
    case class CustomIssueType(key: String, desc: Option[String])

    type P = CustomIssueType
    type Px = (CustomIssueTypeId, P)
    val PreSpec = SpecBuilder[P](
                    SpecAttr[P](_.key)(KeyValidator)(TextInputEditor),
                    SpecAttr[P](_.desc)(DescValidator)(TextareaEditor)
                  ).buildO(CustomIssueType.apply)
                  .rowId[CustomIssueTypeId]
    val Spec = PreSpec.ctxAwareValidators(Some(PreSpec.uniquenessCheck(_.key)), None)
                 .saveFn(fakeSave)

    def fakeSave(p: Option[Px], g: CustomIssueType) = IO[Px] {
      console.log(s"SAVING $p ⇒ $g")
      val newId = p.fold[CustomIssueTypeId](666L)(_._1)
      (newId, g)
    }

    def fakeDelete(id: CustomIssueTypeId) = IO {
      console.log(s"DELETING $id")
    }

    object NewRow {
      val create = Spec.createUnsaved(("",""))
      val row = Spec.unsavedRow((T, vv) => {
        val (key, desc) = vv
        val delButton = button(onclick ~~> T.modStateIO(Spec.removeUnsaved))("Cancel")
        tr(keyAttr := "new")(td(key), td(desc), td(delButton))
      })
    }

    object SavedRow {
      private val delete = Spec.deleteSavedFn(fakeDelete)
      val row = Spec.savedRow((T, id, vv) => {
        val (key, desc) = vv
        val delButton = button(onclick ~~> T.runStateIO(delete(id)))("Delete")
        tr(keyAttr := id)(td(key), td(desc), td(delButton))
      })
    }

    val IssueTypeTable = ReactComponentB[List[(CustomIssueTypeId, CustomIssueType)]]("IssueTypeTable")
      .getInitialState(p => Spec.initialState(p))
      .render(T => {
        val S = T.state
        //console.log(s"State = $S")

        val newRow = NewRow.row.render(T)(())
        val savedRows = Spec.renderSaved(T, SavedRow.row)(_.sortBy(_._2._1.key))

        // TODO handle empty table
        div(
          button(onclick ~~> T.runStateIO(NewRow.create))("Create"),
          table(
            thead(tr(th("Name"), th("Description"), th("Ctrls"))),
            tbody(newRow, savedRows)
          )
        )
      }).create
    }

  // ===================================================================================================================

  object DragAndDrop {

    case class Item(id: Int, name: String)

    val RowComp = DND.Child.dndItemComponent[Item](
      (i, hnd) => hnd :: raw(s"${i.id} | ${i.name}") :: Nil)

    case class ParentState(items: List[Item], dnd: DND.Parent.PState[Item], i: Int)

    def itemCmp(a: Item, b: Item) = a.id==b.id

    val Component = ReactComponentB[List[Item]]("DragAndDrop")
      .getInitialState(p => ParentState(p, DND.Parent.initialState, 0))
      .render(T => {
console.log(s"DND.State = ${T.state}")
        val itemsState = T.focusState(_.items)((a, b) => a.copy(items = b))
        val dndState = T.focusState(_.dnd)((a, b) => a.copy(dnd = b))

        def move(from: Item, to: Item) =
          IO{ console.log(s"...Before = ${T.state}") } >>
          itemsState.modStateIO(DND.move(from, to, itemCmp)) >>
          IO{ console.log(s"....After = ${T.state}") }

        def renderItem(i: Item) =
          li(key := i.id)(RowComp((i, DND.Parent.cProps(dndState, i, itemCmp, move ))))

        div(
          h1("Drag and Drop"),
          ol(T.state.items.map(renderItem).toJsArray)

        )
      }).create
  }

  // ===================================================================================================================

  object ReqTypes {

    type CustomReqTypeId = Int

    case class CustomReqType(
        id: CustomReqTypeId,
        mnemonic: String,
        name: String,
        oldMnemonics: Set[String],
        implicationReq: Boolean,
        alive: Boolean)

    // TODO T.state is consistent, doesn't show next iteration's state
    // TODO prevent old mnemonic reuse
    // TODO Add an uneditable UC type in there

    type P = CustomReqType
    val PreSpec = SpecBuilder[P](
        SpecAttr[P](_.mnemonic)(MnemonicValidator)(TextInputEditor),
        SpecAttr[P](_.implicationReq)(NopValidator)(CheckboxEditor)
      ).rowId[CustomReqTypeId]
    val Spec = PreSpec.ctxAwareValidators(Some(PreSpec.uniquenessCheck(_.mnemonic)), None)
      .saveFn(fakeSave)
    type Px = PreSpec.Px

    def fakeSave(op: Option[Px], g: (String, Boolean)) = IO[Px] {
      val r = op match {
        case None => CustomReqType(666, g._1, "No mame yet", Set.empty, g._2, true)
        case Some((_, p)) => p.copy(mnemonic = g._1, implicationReq = g._2)
      }
      console.log(s"SAVING $op =[$g]=> $r")
      (r.id, r)
    }

    def fakeDelete(id: CustomReqTypeId) = IO {
      console.log(s"DELETING $id")
    }

    private val Create = Spec.createUnsaved(("",false))

    private val NewRow = {
      Spec.unsavedRow((T, vv) => {
        val (mnemonic, impReq) = vv
        val delButton = button(onclick ~~> T.modStateIO(Spec.removeUnsaved))("Cancel")
        tr(keyAttr := "new")(td(mnemonic), td(impReq), td(delButton))
      })
    }

    private val SavedRow = {
      val delete = Spec.deleteSavedFn(fakeDelete)
      Spec.savedRow((T, id, p, vv) => {
        val (mnemonic, impReq) = vv
        val delButton = button(onclick ~~> T.runStateIO(delete(id)))("Delete")
        tr(keyAttr := id)(td(mnemonic), td(p.name), td(impReq), td(delButton))
      })
    }

    val ReqTypeTableComp = ReactComponentB[List[CustomReqType]]("ReqTypeTable")
      .getInitialState(p => Spec.initialState(p, _.id))
      .render(T => {

        val newRow = NewRow.render(T)(())
        val savedRows = Spec.renderSaved(T, SavedRow)(_.sortBy(_._2._1.mnemonic))

        div(
          button(onclick ~~> T.runStateIO(Create))("Create"),
          table(
            thead(tr(th("Mnemonic"), th("Name"), th("Implication Required"), th("Ctrls"))),
            tbody(
              newRow, savedRows
            )
          )
        )

      }).create
  }
}
