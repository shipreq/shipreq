package shipreq.webapp.client.app.ui.cfg

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra.OnUnmount
import scala.language.reflectiveCalls
import scalaz.std.string.stringInstance
import scalaz.std.tuple._

import shipreq.base.util.UnivEq
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.delta.Partition
import shipreq.webapp.base.data.Validators.{reqType => V}
import shipreq.webapp.base.protocol.Routines.CustomReqTypeCrud
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.lib.{FilterDead, CrudIO}
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.On

object CfgReqTypes {

  case class Props(cp: ClientProtocol, remote: CustomReqTypeCrud.Remote, clientData: ClientData, filterDead: FilterDead) {
    def component = Component(this)
  }

  val fields = FieldSet3[CustomReqType](_.mnemonic.value, _.name, _.imp)(("", "", ImplicationRequired.Not))
  val storesAndState = TypicalStoresAndState(fields).keyedBy[CustomReqTypeId]
  import storesAndState._

  val Component =
    ReactComponentB[Props]("Cfg: Req Types")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(DeltaListener(_.clientData, DeltaListener.store(savedRowStoreS).handler(Partition.CustomReqTypes)))
      .build

  private def initialState(p: Props): S =
    State(newRowStore.initState,
      savedRowStore.initStateIM(p.clientData.project.customReqTypes.data),
      p.filterDead)

  // ===================================================================================================================
  final class Backend($: BackendScope[Props, S]) extends OnUnmount {
    val crudIO = CrudIO(CustomReqType, CustomReqTypeCrud)($.props.cp, $.props.remote, $.props.clientData)
    val supp = TypicalSupp(storesAndState, crudIO)($)

    val onWhenImplicationRequired = On <=> ImplicationRequired

    val rowE = {
      val mnemonicE = Editors.textInputEditor.applyValidator(V.mnemonicS)
      val nameE     = Editors.textInputEditor.applyValidator(V.nameS)
      val impE      = Editors.checkboxEditor.imap(onWhenImplicationRequired).strengthL[V.S]
      val e = Editor.merge3S(fields, mnemonicE, nameE, impE).tupleI.zoomU[S]
      supp.addEditorFeatures(e)(V.all, _._1._2, p => (p.mnemonic, p.name, p.imp))
    }

    def checkbox(i: ImplicationRequired) =
      UI checkbox (onWhenImplicationRequired from i)

    val table = {
      def rowRenderer =
        new CfgTable.RowRenderer[CustomReqType, rowE.View, (TagMod, Set[ReqType.Mnemonic], TagMod, TagMod)] {
          override def newRow = {
            case (mnemonic, name, impReq) => (mnemonic, UnivEq.emptySet, name, impReq)
          }
          override def savedRow = {
            case ((mnemonic, name, impReq), p) => (mnemonic, p.oldMnemonics, name, impReq)
          }
          override def deletedRow = p =>
            (p.mnemonic.value, p.oldMnemonics, p.name, checkbox(p.imp)(^.disabled := true))

          override def render = {
            case (mnemonic, oldMnemonics, name, impReq) =>
              val mn: TagMod =
                if (oldMnemonics.isEmpty)
                  mnemonic
                else
                  Seq(mnemonic, <.div(CSS.deadInline, oldMnemonics.toStream.map(_.value).sorted.mkString(", ")))
              Seq(mn, name, impReq)
          }
        }

      val t = CfgTable.typical(storesAndState)(rowE)(_.mnemonic, rowRenderer, supp.deletion, _.alive, $)

      val headerRow = CfgTable.header(List("Mnemonic", "Name", "Implication Required"))

      val staticRows: t.RowStream = {
        def rr(r: StaticReqType): ReactElement = {
          val imp = checkbox(r.imp)(^.disabled := true)
          val norm: t.RowContent = (r.mnemonic.value, r.oldMnemonics, r.name, imp)
          t.row("static", RowStatus.Sync, norm, EmptyTag)(^.key := r.mnemonic.value)
        }
        StaticReqType.values.toStream.map(r => r.mnemonic -> rr(r))
      }

      () => t.table(headerRow, staticRows)
    }

    val outer =
      CfgTable.outer(storesAndState)($)

    def render: ReactElement =
      outer(table())
  }
}