package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<*._, ScalazReact._
import japgolly.scalajs.react.experiment.OnUnmount
import scala.language.reflectiveCalls
import scalaz.std.string.stringInstance
import scalaz.std.tuple._

import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.data.delta.Partition
import shipreq.webapp.base.data.Validators.{reqType => V}
import shipreq.webapp.base.protocol.Routines.CustomReqTypeCrud
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.lib.CrudIO
import shipreq.webapp.client.lib.ui._
import UI.checkbox

object CfgReqTypes {

  val fields = FieldSet3[CustomReqType](_.mnemonic.value, _.name, _.imp)(("", "", ImplicationNotRequired))

  val storesAndState = TypicalStoresAndState(fields).keyedBy[CustomReqType.Id]
  import storesAndState._

  case class Props(remote: CustomReqTypeCrud.Remote, clientData: ClientData, showDeleted: Boolean) {
    def component = Component(this)
  }

  val Component =
    ReactComponentB[Props]("Cfg: Req Types")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(
        RemoteDeltaListener(CustomReqType, CustomReqTypeCrud)
          .install(savedRowStoreS, Partition.CustomReqTypes, _.clientData))
      .build

  private def initialState(p: Props): S =
    State(newRowStore.initState,
      savedRowStore.initStateS(p.clientData.project.customReqTypes.data, _.id),
      p.showDeleted)

  // ===================================================================================================================
  final class Backend(c: BackendScope[Props, S]) extends OnUnmount {

    val crudIO = CrudIO(CustomReqType, CustomReqTypeCrud)(c.props.remote, c.props.clientData)

    val deletion = Persistence.asyncDeletionS(savedRowStoreS)(_.alive, crudIO._deleteIO, c runState _)

    val rowE = {
      val mnemonicE = Editors.textInputEditor.applyValidator(V.mnemonicS)
      val nameE     = Editors.textInputEditor.applyValidator(V.nameS)
      val impE      = Editors.checkboxEditor.imap(ImplicationRequired).strengthL[V.S]

      var e = Editor.merge3S(fields, mnemonicE, nameE, impE).tupleI.zoomU[S]

      e = e.applyRowUpdateAndRevert(savedRowStoreS, newRowStoreS)(_._1._2)

      val needSave = SaveNeed.cmpToExtract((p: CustomReqType) => (p.mnemonic, p.name, p.imp))
      val savef = Persistence.asyncSaveT(V.all, storesAndState)(needSave, crudIO, c runState _)
      e.applyOnEditFinishedK(savef)(_._1._2)
    }

    val table = {
      def rowRenderer =
        new CfgTable.RowRenderer[CustomReqType, rowE.View, (Modifier, Set[ReqType.Mnemonic], Modifier, Modifier)] {

          override def newRow = {
            case (mnemonic, name, impReq) => (mnemonic, Set.empty, name, impReq)
          }

          override def savedRow = {
            case ((mnemonic, name, impReq), p) => (mnemonic, p.oldMnemonics, name, impReq)
          }

          override def deletedRow = p =>
            (p.mnemonic.value, p.oldMnemonics, p.name, checkbox(ImplicationRequired from p.imp)(*.disabled := true))

          override def render = {
            case (mnemonic, oldMnemonics, name, impReq) =>
              val mn: Modifier =
                if (oldMnemonics.isEmpty)
                  mnemonic
                else
                  Seq(mnemonic, <.div(*.cls := "oldMnemonics", oldMnemonics.toStream.map(_.value).sorted.mkString(", ")))
              Seq(mn, name, impReq)
          }
        }

      val t = CfgTable.typical(storesAndState)(rowE)(_.mnemonic, rowRenderer, deletion, c)

      val headerRow = CfgTable.header(List("Mnemonic", "Name", "Implication Required"))

      val staticRows: t.RowStream = {
        def rr(r: ReqType.Static): ReactElement = {
          val imp = checkbox(ImplicationRequired from r.imp)(*.disabled := true)
          val norm: t.RowContent = (r.mnemonic.value, r.oldMnemonics, r.name, imp)
          t.row("static", RowStatus.Sync, norm, EmptyTag)(*.keyAttr := r.mnemonic.value)
        }
        ReqType.static.map(r => r.mnemonic -> rr(r)).toStream
      }

      () => t.table(headerRow, staticRows)
    }

    def render: ReactElement =
      <.div(
        ShowDeletedToggler(storesAndState)(c),
        table())
  }
}