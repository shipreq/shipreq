import org.scalajs.dom
import org.scalajs.dom.console
import scala.scalajs.js
import scalaz.syntax.bind._
import scalaz.Equal
import scalaz.effect.IO
import scalaz.std.string.stringInstance
import scalaz.std.anyVal.booleanInstance
import monocle.SimpleLens
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._
import japgolly.scalajs.react.ScalazReact._
import utily.EditorStuff._
import utily.FormStuff._
import utily.SpecN._
import utily.Lib._
import domainy.Data._
import domainy.FakeDao

object Phase2 extends js.JSApp {
  override def main(): Unit = {

    {
      import Phase2.IssueConfig._
      IssueTypeTable(List(
        CustomIssueType(CustomIssueTypeId(1), "TODO", None),
        CustomIssueType(CustomIssueTypeId(2), "TBD", Some("To Be Decided."))
      )) render dom.document.getElementById("target")
    }

    {
      import Phase2.ReqTypes._
      ReqTypeTableCompOuter(ReqTypeTableProps(List(
        CustomReqType(CustomReqTypeId(1), "CO", Set.empty, "Constraint", false, true),
        CustomReqType(CustomReqTypeId(2), "MF", Set.empty, "Major Feature", false, true),
        CustomReqType(CustomReqTypeId(3), "FR", Set.empty, "Functional Requirement", false, true),
        CustomReqType(CustomReqTypeId(4), "BR", Set.empty, "Business Rule", false, true),
        CustomReqType(CustomReqTypeId(5), "DD", Set("DA","DDF"), "Data Definition", false, false),
        CustomReqType(CustomReqTypeId(6), "SI", Set.empty, "Solution Idea", true, false)
      ), false)) render dom.document.getElementById("target2")
    }

    ReactExamples.DragAndDrop.demo  render dom.document.getElementById("target3")
  }

  // ===================================================================================================================

  object IssueConfig {

    type P = CustomIssueType
    val PreSpec = SpecBuilder[P](
                    SpecAttr[P](_.key)(KeyValidator)(TextInputEditor),
                    SpecAttr[P](_.desc)(DescValidator)(TextareaEditor)
                  ).mapO(CustomIssueTypeV.fromTuple)
                  .rowId[CustomIssueTypeId]
    val Spec = PreSpec.ctxAwareValidators(Some(PreSpec.uniquenessCheck(_.key)), None)
                 .saveFn2(fakeSave, _.id)

    def fakeSave(prev: Option[P], newValues: CustomIssueTypeV) = IO[P] {
      prev match {
        case None =>
          FakeDao.customIssueType.create(newValues)
        case Some(old) if old.value == newValues =>
          old
        case Some(p) =>
          FakeDao.customIssueType.update(newValues withId p.id)
      }
    }

    def fakeDelete(id: CustomIssueTypeId) = IO { FakeDao.customIssueType.deleteHard(id) }

    object NewRow {
      val create = Spec.createUnsaved(("",""))
      val row = Spec.unsavedRow((T, vv) => {
        val (key, desc) = vv
        val delButton = button(onclick ~~> T.runState(Spec.removeUnsavedS))("Cancel")
        tr(keyAttr := "new")(td(key), td(desc), td(delButton))
      })
    }

    object SavedRow {
      private val delete = Spec.deleteSavedS(fakeDelete)
      val row = Spec.savedRow((T, id, vv) => {
        val (key, desc) = vv
        val delButton = button(onclick ~~> T.runState(delete(id)))("Delete")
        tr(keyAttr := id.value)(td(key), td(desc), td(delButton))
      })
    }

    val IssueTypeTable = ReactComponentB[List[CustomIssueType]]("IssueTypeTable")
      .getInitialState(p => Spec.initialState(p, _.id))
      .render(T => {
        val S = T.state
        //console.log(s"State = $S")

        val newRow = NewRow.row.render(T)(())
        val savedRows = Spec.renderSaved(T, SavedRow.row)(_.sortBy(_._2.key))

        // TODO handle empty table
        div(
          button(onclick ~~> T.runState(NewRow.create))("Create"),
          table(
            thead(tr(th("Name"), th("Description"), th("Ctrls"))),
            tbody(newRow, savedRows)
          )
        )
      }).create
    }

  // ===================================================================================================================

  object ReqTypes {

    // TODO render old mnemonics

    type P = CustomReqType
    val PreSpec = SpecBuilder[P](
        SpecAttr[P](_.mnemonic)(MnemonicValidator)(TextInputEditor),
        SpecAttr[P](_.name)(ReqNameValidator)(TextInputEditor),
        SpecAttr[P](_.implicationRequired)(NopValidator)(CheckboxEditor)
      ).mapO(CustomReqTypeNV.fromTuple)
      .rowId[CustomReqTypeId]

    // TODO UC hardcoding here
    val mnemonicUniqueness = uniqueness[PreSpec.S, PreSpec.RowId, ReqTypeMnemonic, String](
      (s,ow) => UC #:: s._1.toStream.filterNot(x => ow.fold(false)(_ == x._1)).flatMap{x=>
        val p = x._2._1
        p.mnemonic #:: p.oldMnemonics.toStream
      },
      (a,i) => a==i,
      "Mnemonic has already been used."
    )

    val Spec = PreSpec.ctxAwareValidators(Some(mnemonicUniqueness), Some(PreSpec.uniquenessCheck(_.name)), None)
      .saveFn2(fakeSave, _.id)

    val Deletion = new DeletionThingy(Spec)(
      SimpleLens[P](_.alive)((a,b) => a.copy(alive = b)),
      id => a => IO(a match {
        case HardDelete => FakeDao.customReqType.deleteHard(id)
        case SoftDelete => FakeDao.customReqType.deleteSoft(id)
        case Restore    => FakeDao.customReqType.restore(id)
      }))

    def fakeSave(op: Option[P], newValues: CustomReqTypeNV) = IO[P] {
      op match {
        case None =>
          FakeDao.customReqType.create(newValues)
        case Some(old) if old.value == newValues =>
          old
        case Some(p) =>
          FakeDao.customReqType.update(p.id, newValues)
      }
    }

    private val Create = Spec.createUnsaved(("","",false))

    private def row(mnemonic: Modifier, name: Modifier, impReq: Modifier, delButton: Modifier) =
      Seq(td(mnemonic), td(name), td(impReq), td(delButton))

    private def UC: ReqTypeMnemonic = "UC"
    private val ucRow =
      tr(key := UC, row(raw(UC), raw("Use Case"), checkbox(true)(disabled := true), Nop))

    private val NewRow =
      Spec.unsavedRow((T, vv) => {
        val (mnemonic, name, impReq) = vv
        val delButton = button(onclick ~~> T.runState(Spec.removeUnsavedS))("Cancel")
        tr(keyAttr := "new")(row(mnemonic, name, impReq, delButton))
      })

    private val SavedRow =
      Spec.savedRow((T, id, p, vv) => {
        val (mnemonic, name, impReq) = vv
        tr(keyAttr := id.value)(row(mnemonic, name, impReq, Deletion.buttons(T, id, HardDelete, SoftDelete)))
      })

    def deletedRow(T: ComponentStateFocus[PreSpec.S], p: P) =
      tr(cls := "del", key := p.id.value, row(
        raw(p.mnemonic),
        raw(p.name),
        checkbox(p.implicationRequired)(disabled := true),
        Deletion.button(T, p.id, Restore)))

    case class ReqTypeTableProps(items: List[CustomReqType], showDeleted: Boolean)

    val ReqTypeTableComp = ReactComponentB[ReqTypeTableProps]("ReqTypeTable")
      .getInitialState(p => Spec.initialState(p.items, _.id))
      .render(T => {

        val newRow = NewRow.render(T)(())

        type RS = Stream[(ReqTypeMnemonic, Tag)]
        def savedRows: RS = {
          val rr = SavedRow.render(T)
          // TODO UC hardcoding here
          (UC -> ucRow) #:: Deletion.getSavedP(T, true).map(p => (p.mnemonic, rr(p.id)))
        }
        def deletedRows: RS =
          if (T.props.showDeleted)
            Deletion.getSavedP(T, false).map(p => (p.mnemonic, deletedRow(T, p)))
          else Stream.empty

        val savedAndDeleted = (savedRows #::: deletedRows).sortBy(_._1).map(_._2).toJsArray

        div(
          button(onclick ~~> T.runState(Create))("Create"),
          table(
            thead(tr(th("Mnemonic"), th("Name"), th("Implication Required"), th("Ctrls"))),
            tbody(newRow, savedAndDeleted)
          )
        )

      }).create

    val ReqTypeTableCompOuter = ReactComponentB[ReqTypeTableProps]("ReqTypeTableOuter")
      .getInitialState(p => p.showDeleted)
      .renderS((t,p,s) => {

        div(
          label(
            checkbox(s)(onchange --> t.modState(b => !b)),
            raw(if (s) "Showing deleted" else "Not showing deleted")
          ),
          ReqTypeTableComp(p.copy(showDeleted = s))
        )

      }).create
  }
}
