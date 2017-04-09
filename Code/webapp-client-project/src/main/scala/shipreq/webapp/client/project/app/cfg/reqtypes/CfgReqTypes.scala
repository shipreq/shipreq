package shipreq.webapp.client.project.app.cfg.reqtypes

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._, vdom.html_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import scala.language.reflectiveCalls
import scalacss.ScalaCssReact._
import scalaz.std.string.stringInstance
import scalaz.std.tuple._
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.data.Validators2.{reqType => V}
import shipreq.webapp.base.filter.PotentialFilter
import shipreq.webapp.base.protocol.CustomReqTypeCrud
import shipreq.webapp.client.base.data.On
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.cfg.shared._
import shipreq.webapp.client.project.app.state.{ChangeListener, ClientData}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.Widgets

object CfgReqTypes {

  case class Props(cp        : ClientProtocol,
                   remote    : CustomReqTypeCrud.Instance,
                   clientData: ClientData,
                   filterDead: StateSnapshot[FilterDead],
                   usageShow : Usage.Show) {
    def component = Component(this)
  }
  implicit val reusability = Reusability.caseClass[Props]

  val fields = FieldSet3[CustomReqType](_.mnemonic.value, _.name, _.imp)(("", "", ImplicationRequired.Not))
  val storesAndState = TypicalStoresAndState(fields).keyedBy[CustomReqTypeId]
  import storesAndState._
  val changeListener = ChangeListener.store(savedRowStoreS)(_.customReqTypes, _.config.reqTypes.custom.get)

  val Component =
    ScalaComponent.builder[Props]("Cfg: Req Types")
      .initialState_P(initialState)
      .renderBackend[Backend]
      .configure(changeListener.install(_.clientData))
      .build

  private def initialState(p: Props): S =
    State(
      newRowStore.initState,
      savedRowStore.initStateIM(p.clientData.project().config.reqTypes.custom))

  // ===================================================================================================================
  final class Backend($: BackendScope[Props, S]) extends OnUnmount {
    val project    = Px.props($).map(_.clientData.project()).withReuse.manualRefresh
    val filterDead = Px.props($).map(_.filterDead.value).withReuse.manualRefresh
    val usageShow  = Px.props($).map(_.usageShow).withReuse.manualRefresh

    val crudIO = Px.props($).withReuse.autoRefresh.map(p => CrudActionIO(CustomReqType, CustomReqTypeCrud)(p.cp, p.remote, p.clientData))
    val supp = TypicalSupp(storesAndState)(crudIO.value(), $)

    val onWhenImplicationRequired = On <=> ImplicationRequired

    def validatorState(k: Option[CustomReqTypeId]): V.State =
      validatorState(k, $.state.runNow())

    def validatorState(k: Option[CustomReqTypeId], s: => S): V.State =
      V.State(k, () => savedRowStoreS.getAllP(s))

    val rowE = {
      val mnemonicE = Editors.textInputEditor.applyStatefulValidator(V.mnemonic.unnamedFn)
      val nameE     = Editors.textInputEditor.applyStatefulValidator(V.name.unnamedFn)
      val impE      = Editors.checkboxEditor.imap(onWhenImplicationRequired).strengthL[V.State]
      val e = Editor.merge3S(fields, mnemonicE, nameE, impE).tupleI.zoomU[S]
      import supp.sas
      val saveFn = supp.crudIO.map(c =>
        Persistence.asyncSaveS(
          V.all,
          sas.savedRowStoreS)(
          sas.newRowStoreS,
          validatorState(None, _),
          k => validatorState(Some(k), _),
          supp.saveNeed(p => (p.mnemonic, p.name, p.imp)),
          c.createIO,
          c.updateIO,
          supp.realise)
      ).extract

      supp.addEditorFeatures2(e)(saveFn, _._1.subject)
    }

    def checkbox(i: ImplicationRequired) =
      Widgets.checkbox(onWhenImplicationRequired from i)

    val usageFn = Usage((_: ReqType).reqTypeId)(
      _.reqTypeCount,
      PotentialFilter ReqType _.mnemonic,
      project, filterDead, usageShow)

    val cfgTable = {
      val rowRenderer =
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
                  TagMod(mnemonic, <.div(Style.cfg.deadMnemonic, MutableArray(oldMnemonics.iterator.map(_.value)).sort.mkString(", ")))
              Seq(mn, name, impReq, usage.whenDefined)
          }
        }

      type I = storesAndState.Input
      def rowA(k: Option[CustomReqTypeId], i: I): rowE.InputA = (validatorState(k), i)
      def newRowA(i: I) = rowA(None, i)
      def savedRowA(id: CustomReqTypeId) = rowA(Some(id), storesAndState.savedRowStoreS.getI(id)($.state.runNow()))

      new CfgTable[storesAndState.S, CustomReqTypeId, storesAndState.Persisted, I, rowE._A, rowE._B, rowE._C, rowE._V, ReqType.Mnemonic, rowRenderer._R](
        rowE,
        storesAndState.savedRowStoreS,
        storesAndState.newRowStoreS,
        _.mnemonic,
        rowRenderer,
        newRowA,
        savedRowA,
        () => supp.deletion.value(),
        _.live,
        $.props.map(_.filterDead.value),
        $)
    }

    val table = {
      val headerRow = CfgTable.header(List(
        FieldNames.mnemonic,
        FieldNames.name,
        FieldNames.implicationRequired,
        FieldNames.usage))

      val staticRows: cfgTable.RowStream = {
        def rr(r: StaticReqType): VdomElement = {
          val imp = checkbox(r.imp)(^.disabled := true)
          val usage = Some(usageFn(r))
          val norm: cfgTable.RowContent = (r.mnemonic.value, r.oldMnemonics, r.name, imp, usage)
          cfgTable.row("static", RowStatus.Sync, norm, EmptyVdom)(^.key := r.mnemonic.value)
        }
        StaticReqType.values.toStream.map(r => r.mnemonic -> rr(r))
      }

      () => cfgTable.table(headerRow, staticRows)
    }

    val outer =
      cfgTable.wrapWithFilterDeadCheckbox(fd => $.props.flatMap(_.filterDead setState fd))

    def render: VdomElement = {
      Px.refresh(project, filterDead, usageShow)
      outer(table())
    }
  }
}