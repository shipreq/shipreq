package shipreq.webapp.client

import monocle.{Iso, SimpleLens}
import org.scalajs.dom.console

import scalaz.effect.IO
import scalaz.std.anyVal.booleanInstance
import scalaz.std.string.stringInstance
import shipreq.webapp.shared.data._
import shipreq.webapp.client.ui.table._
import shipreq.webapp.client.ui.{Editors => E, Util}
import Validators.{reqType => V}
import ReqType.Mnemonic

object CfgReqType {

  type P = CustReqType
  type D = CustReqType.Id

  val prespec = TableSpecBuilder[P](
    FieldSpec[P](_.mnemonic.value)(V.mnemonic)(E.TextInputEditor),
    FieldSpec[P](_.name)(V.name)(E.TextInputEditor),
    FieldSpec[P].noValidation(_.imp, ImplicationRequired)(E.CheckboxEditor))
    .dataId[D]

  //  val mnemonicUniqueness = Validator.uniqueness[PreSpec.S, PreSpec.R, ReqTypeMnemonic, String](
  //    (s,ow) => UC #:: s._1.toStream.filterNot(x => ow.fold(false)(_ == x._1)).flatMap{x=>
  //      val p = x._2._1
  //      p.mnemonic #:: p.oldMnemonics.toStream
  //    },
  //    (a,i) => a==i,
  //    "Mnemonic has already been used."
  //  )

  val spec = prespec
    .tableConstraints(None, Some(prespec.uniquenessCheck(_.name).fieldName("Name")), None)
    .saveFn2(fakeSave, _.id)

  // AJAX is async
  def fakeSave(op: Option[P], newValues: prespec.U) = IO[P] {
    val (a,b,c) = newValues
    op match {
      case None =>
        console.log(s"FAKE-SAVE₁: New row $newValues")
        println(s"FAKE-SAVE₂: New row $newValues")
        CustReqType(CustReqType.Id(666L), a, Set.empty, b, c, Alive)
      //        case Some(old) if old.value == newValues =>
      //          old
      case Some(p) =>
        console.log(s"FAKE-SAVE₁: Update [$p] → $newValues")
        println(s"FAKE-SAVE₂: Update [$p] → $newValues")
        p
    }
  }

  def alive1: SimpleLens[P, Alive] = SimpleLens[P](_.alive)((a,b) => a.copy(alive = b))
  def alive2: SimpleLens[P, Boolean] = alive1 composeLens liftIso(Alive).reverse

  import scalaz.Isomorphism.<=>
  def liftIso[A, B](i: A <=> B) = Iso(i.to, i.from)

  val deletion = new DeletionManager(spec)(
    alive2,
    id => a => IO(a match {
//      case HardDelete => FakeDao.customReqType.deleteHard(id)
//      case SoftDelete => FakeDao.customReqType.deleteSoft(id)
//      case Restore    => FakeDao.customReqType.restore(id)
      case x => println(s"FAKE DELETE: $x on $id")
    }))

  object Action {
    val newRow =
      spec.createUnsaved(("","",false))
  }

  object Render {
    import japgolly.scalajs.react._
    import japgolly.scalajs.react.vdom.ReactVDom._
    import japgolly.scalajs.react.vdom.ReactVDom.all._
    import japgolly.scalajs.react.ScalazReact._

    private def row(mnemonic: Modifier, name: Modifier, impReq: Modifier, delButton: Modifier) =
      Seq(td(mnemonic), td(name), td(impReq), td(delButton))

    val newRow =
      spec.unsavedRow((T, vv) => {
        val (mnemonic, name, impReq) = vv
        val delButton = button(onclick ~~> T.runState(spec.removeUnsavedS))("Cancel")
        tr(keyAttr := "new")(row(mnemonic, name, impReq, delButton))
      })

    val savedRow =
      spec.savedRow((T, id, p, vv) => {
        val (mnemonic, name, impReq) = vv
        tr(keyAttr := id.value)(row(mnemonic, name, impReq, deletion.buttons(T, id, HardDelete, SoftDelete)))
      })

    def deletedRow(T: ComponentStateFocus[prespec.S], p: P) =
      tr(cls := "del", key := p.id.value, row(
        raw(p.mnemonic),
        raw(p.name),
        Util.checkbox(ImplicationRequired from p.imp)(disabled := true),
        deletion.button(T, p.id, Restore)))
  }

//  object Comp {
    import japgolly.scalajs.react._
    import japgolly.scalajs.react.vdom.ReactVDom._
    import japgolly.scalajs.react.vdom.ReactVDom.all._
    import japgolly.scalajs.react.ScalazReact._

    case class ReqTypeTableProps(items: List[P], showDeleted: Boolean)

    val ReqTypeTableComp = ReactComponentB[ReqTypeTableProps]("CfgReqTypesⁱ")
      .getInitialState(p => spec.initialState(p.items, _.id))
      .render(T => {

        val newRow = Render.newRow.render(T)(())

        type RS = Stream[(Mnemonic, Tag)]
        def savedRows: RS = {
          val rr = Render.savedRow.render(T)
          // TODO UC hardcoding here
          // (UC -> ucRow) #::
          deletion.getSavedP(T, true).map(p => (p.mnemonic, rr(p.id)))
        }
        def deletedRows: RS =
          if (T.props.showDeleted)
            deletion.getSavedP(T, false).map(p => (p.mnemonic, Render.deletedRow(T, p)))
          else Stream.empty

        val savedAndDeleted = (savedRows #::: deletedRows).sortBy(_._1.value).map(_._2).toJsArray

        div(
          button(onclick ~~> T.runState(Action.newRow))("New"),
          table(
            thead(tr(th("Mnemonic"), th("Name"), th("Implication Required"), th("Ctrls"))),
            tbody(newRow, savedAndDeleted)
          )
        )
    }).create

    val ReqTypeTableCompOuter = ReactComponentB[ReqTypeTableProps]("CfgReqTypes")
      .getInitialState(p => p.showDeleted)
      .renderS((t,p,s) => {
        div(
          label(
            Util.checkbox(s)(onchange --> t.modState(b => !b)), // TODO --> instead of ~~>
            raw(if (s) "Showing deleted" else "Not showing deleted")
          ),
          ReqTypeTableComp(p.copy(showDeleted = s))
        )
      }).create

//  }

  /*
  // ===================================================================================================================

    // TODO UC hardcoding here
    val mnemonicUniqueness = Validator.uniqueness[PreSpec.S, PreSpec.R, ReqTypeMnemonic, String](
      (s,ow) => UC #:: s._1.toStream.filterNot(x => ow.fold(false)(_ == x._1)).flatMap{x=>
        val p = x._2._1
        p.mnemonic #:: p.oldMnemonics.toStream
      },
      (a,i) => a==i,
      "Mnemonic has already been used."
    )

    private def UC: ReqTypeMnemonic = "UC"
    private val ucRow =
      tr(key := UC, row(raw(UC), raw("Use Case"), checkbox(true)(disabled := true), Nop))



  // ===================================================================================================================
  // Done

    type P = CustomReqType
    val PreSpec = TableSpecBuilder[P](
        FieldSpec[P](_.mnemonic           )(MnemonicValidator)(TextInputEditor),
        FieldSpec[P](_.name               )(ReqNameValidator )(TextInputEditor),
        FieldSpec[P](_.implicationRequired)(Validator.nop    )(CheckboxEditor)
      ).mapU(CustomReqTypeNV.fromTuple)
      .dataId[CustomReqTypeId]

     val Spec = PreSpec.tableConstraints(Some(mnemonicUniqueness), Some(PreSpec.uniquenessCheck(_.name)), None)
      .saveFn2(fakeSave, _.id)

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

val Deletion = new DeletionManager(Spec)(
      SimpleLens[P](_.alive)((a,b) => a.copy(alive = b)),
      id => a => IO(a match {
        case HardDelete => FakeDao.customReqType.deleteHard(id)
        case SoftDelete => FakeDao.customReqType.deleteSoft(id)
        case Restore    => FakeDao.customReqType.restore(id)
      }))

    private val Create = Spec.createUnsaved(("","",false))

    private def row(mnemonic: Modifier, name: Modifier, impReq: Modifier, delButton: Modifier) =
      Seq(td(mnemonic), td(name), td(impReq), td(delButton))

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

   */
}