import org.scalajs.dom
import org.scalajs.dom.console
import scala.scalajs.js
import scalaz.syntax.bind._
import scalaz.effect.IO
import monocle.function.Field2._
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
                 .saveFn(fakeSave)
    type Px = PreSpec.Px

    def fakeSave(prev: Option[Px], newValues: CustomIssueTypeV) = IO[Px] {
      val n = prev match {
        case None         => FakeDao.customIssueType.create(newValues)
        case Some((id,_)) => FakeDao.customIssueType.update(newValues withId id)
      }
      (n.id, n)
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

    // TODO prevent reuse over old mnemonics & UC
    // TODO Add an uneditable UC type in there

    type P = CustomReqType
    val PreSpec = SpecBuilder[P](
        SpecAttr[P](_.mnemonic)(MnemonicValidator)(TextInputEditor),
        SpecAttr[P](_.implicationRequired)(NopValidator)(CheckboxEditor)
      ).mapO(CustomReqTypeNV.fromTuple)
      .rowId[CustomReqTypeId]

    //def uniqueness[S, W, A, I](extract: (S,W) => Stream[A], cmp: (A, I) => Boolean, errorMsg: ErrorMsg = "Already in use. Duplicate.") =
    // TODO UC hardcoding here
    val mnemonicUniqueness = uniqueness[PreSpec.S, PreSpec.RowId, ReqTypeMnemonic, String](
      (s,ow) => UC #:: s._1.toStream.filterNot(x => ow.fold(false)(_ == x._1)).flatMap{x=>
        val p = x._2._1
        p.mnemonic #:: p.oldMnemonics.toStream
      },
      (a,i) => a==i,
      "Mnemonic has already been used."
    )

    val Spec = PreSpec.ctxAwareValidators(Some(mnemonicUniqueness), None)
      .saveFn(fakeSave)
    type Px = PreSpec.Px

    def fakeSave(op: Option[Px], g: CustomReqTypeNV) = IO[Px] {
      val r = op match {
        case None          => FakeDao.customReqType.create(g)
        case Some((id, p)) => FakeDao.customReqType.update(id, g)
      }
      (r.id, r)
    }

    private val Create = Spec.createUnsaved(("",false))

    private def row(mnemonic: Modifier, name: Modifier, impReq: Modifier, delButton: Modifier) =
      Seq(td(mnemonic), td(name), td(impReq), td(delButton))

    private def UC: ReqTypeMnemonic = "UC"
    private val ucRow =
      tr(key := UC, row(raw(UC), raw("Use Case"), checkbox(true)(disabled := "disabled"), Nop))

    private val NewRow = {
      Spec.unsavedRow((T, vv) => {
        val (mnemonic, impReq) = vv
        val delButton = button(onclick ~~> T.runState(Spec.removeUnsavedS))("Cancel")
        tr(keyAttr := "new")(row(mnemonic, "NO NAME!", impReq, delButton))
      })
    }

    private val softDeleteL = _2[Px, P] composeLens SimpleLens2[P](_.alive)((a,b) => a.copy(alive = b))
    private val SavedRow = {
      val hardDelS = Spec.deleteSavedS(id => IO(FakeDao.customReqType.deleteHard(id)))
      val softDelS = Spec.modAndSaveS(px => IO {
        FakeDao.customReqType.deleteSoft(px._1)
        softDeleteL.set(px, false)
      })

      Spec.savedRow((T, id, p, vv) => {
        val (mnemonic, impReq) = vv
        val hardDel = button(onclick ~~> T.runState(hardDelS(id)))("Delete Forever")
        val softDel = button(onclick ~~> T.runState(softDelS(id)))("Delete")
        tr(keyAttr := id.value)(row(mnemonic, p.name, impReq, Seq(hardDel,softDel)))
      })
    }

    case class ReqTypeTableProps(items: List[CustomReqType], showDeleted: Boolean)

    val ReqTypeTableComp = ReactComponentB[ReqTypeTableProps]("ReqTypeTable")
      .getInitialState(p => Spec.initialState(p.items, _.id))
      .render(T => {

        val newRow = NewRow.render(T)(())
        val savedRows = {
          val rr = SavedRow.render(T)
          // TODO UC hardcoding here
          val rows = (UC -> ucRow) #:: Spec.getSaved(T).filter(_._2.alive).map(x => (x._2.mnemonic, rr(x._1)))
          rows.sortBy(_._1).map(_._2).toJsArray
        }
        def deletedRows: Modifier = {
          val restoreS = Spec.modAndSaveS(px => IO {
            FakeDao.customReqType.restore(px._1)
            softDeleteL.set(px, true)
          })
          def r(p: P) =
            tr(cls := "del", key := p.id.value, row(
              raw(p.mnemonic),
              raw(p.name),
              checkbox(p.implicationRequired)(disabled := "disabled"),
              button(onclick ~~> T.runState(restoreS(p.id)))("Restore")))
          Spec.getSaved(T).map(_._2).filterNot(_.alive).sortBy(_.mnemonic).map(r).toJsArray
        }

        div(
          button(onclick ~~> T.runState(Create))("Create"),
          table(
            thead(tr(th("Mnemonic"), th("Name"), th("Implication Required"), th("Ctrls"))),
            tbody(
              newRow, savedRows, T.props.showDeleted && deletedRows
            )
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
