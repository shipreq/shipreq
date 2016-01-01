package shipreq.webapp.client.app.cfg.reqtypes

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import scala.language.reflectiveCalls
import scalacss.ScalaCssReact._
import scalaz.std.string.stringInstance
import scalaz.std.tuple._

import shipreq.base.util.UnivEq
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.data.Validators.{reqType => V}
import shipreq.webapp.base.filter.FilterSpec
import shipreq.webapp.base.protocol.CustomReqTypeCrud
import shipreq.webapp.client.app.ProjectSpaMain
import shipreq.webapp.client.app.Style
import shipreq.webapp.client.app.cfg.shared._
import shipreq.webapp.client.app.state.{ClientData, ChangeListener}
import shipreq.webapp.client.data.{FilterDead, On}
import shipreq.webapp.client.lib.DataReusability._
import shipreq.webapp.client.lib.CrudActionIO
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.widgets.Widgets

object CfgReqTypes {

  case class Props(cp        : ClientProtocol,
                   remote    : CustomReqTypeCrud.Instance,
                   clientData: ClientData,
                   filterDead: FilterDead,
                   routerCtl : ProjectSpaMain.RouterCtl) {
    def component = Component(this)
  }
  implicit val reusability = Reusability.caseClass[Props]

  val fields = FieldSet3[CustomReqType](_.mnemonic.value, _.name, _.imp)(("", "", ImplicationRequired.Not))
  val storesAndState = TypicalStoresAndState(fields).keyedBy[CustomReqTypeId]
  import storesAndState._
  val changeListener = ChangeListener.store(savedRowStoreS)(_.customReqTypes, _.config.customReqTypes.get)

  val Component =
    ReactComponentB[Props]("Cfg: Req Types")
      .initialState_P(initialState)
      .renderBackend[Backend]
      .configure(changeListener.install(_.clientData))
      .build

  private def initialState(p: Props): S =
    State(newRowStore.initState,
      savedRowStore.initStateIM(p.clientData.project.config.customReqTypes),
      p.filterDead)

  // ===================================================================================================================
  final class Backend($: BackendScope[Props, S]) extends OnUnmount {
    val project    = Px.bs($).propsM(_.clientData.project)
    val filterDead = Px.bs($).stateM(_.filterDead)
    val routerCtl  = Px.bs($).propsM(_.routerCtl)

    val crudIO = Px.bs($).propsA.map(p => CrudActionIO(CustomReqType, CustomReqTypeCrud)(p.cp, p.remote, p.clientData))
    val supp = TypicalSupp(storesAndState)(crudIO.value(), $)

    val onWhenImplicationRequired = On <=> ImplicationRequired

    val rowE = {
      val mnemonicE = Editors.textInputEditor.applyValidator(V.mnemonicS)
      val nameE     = Editors.textInputEditor.applyValidator(V.nameS)
      val impE      = Editors.checkboxEditor.imap(onWhenImplicationRequired).strengthL[V.S]
      val e = Editor.merge3S(fields, mnemonicE, nameE, impE).tupleI.zoomU[S]
      supp.addEditorFeatures(e)(V.all, _._1._2, p => (p.mnemonic, p.name, p.imp))
    }

    def checkbox(i: ImplicationRequired) =
      Widgets.checkbox(onWhenImplicationRequired from i)

    val usageFn = Usage((_: ReqType).reqTypeId)(
      _.reqTypeCount,
      FilterSpec ReqType _.mnemonic,
      project, filterDead, routerCtl)

    val table = {
      def rowRenderer =
        new CfgTable.RowRenderer[CustomReqType, rowE.View, (TagMod, Set[ReqType.Mnemonic], TagMod, TagMod, Option[Usage.View])] {
          override def newRow = {
            case (mnemonic, name, impReq) => (mnemonic, UnivEq.emptySet, name, impReq, None)
          }
          override def savedRow = {
            case ((mnemonic, name, impReq), p) => (mnemonic, p.oldMnemonics, name, impReq, Some(usageFn(p)))
          }
          override def deletedRow = p =>
            (p.mnemonic.value, p.oldMnemonics, p.name, checkbox(p.imp)(^.disabled := true), Some(usageFn(p)))

          override def render = {
            case (mnemonic, oldMnemonics, name, impReq, usage) =>
              val mn: TagMod =
                if (oldMnemonics.isEmpty)
                  mnemonic
                else
                  Seq(mnemonic, <.div(Style.cfg.deadMnemonic, oldMnemonics.toStream.map(_.value).sorted.mkString(", ")))
              Seq(mn, name, impReq, usage)
          }
        }

      val t = CfgTable.typical(storesAndState)(rowE)(_.mnemonic, rowRenderer, () => supp.deletion.value(), _.live, $)

      val headerRow = CfgTable.header(List(
        FieldNames.mnemonic,
        FieldNames.name,
        FieldNames.implicationRequired,
        FieldNames.usage))

      val staticRows: t.RowStream = {
        def rr(r: StaticReqType): ReactElement = {
          val imp = checkbox(r.imp)(^.disabled := true)
          val usage = Some(usageFn(r))
          val norm: t.RowContent = (r.mnemonic.value, r.oldMnemonics, r.name, imp, usage)
          t.row("static", RowStatus.Sync, norm, EmptyTag)(^.key := r.mnemonic.value)
        }
        StaticReqType.values.toStream.map(r => r.mnemonic -> rr(r))
      }

      () => t.table(headerRow, staticRows)
    }

    val outer =
      CfgTable.outer(storesAndState)($)

    def render: ReactElement = {
      Px.refresh(project, filterDead, routerCtl)
      outer(table())
    }
  }
}