package shipreq.webapp.client.project.app.cfg.reqtypes

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra._
import scala.language.reflectiveCalls
import scalacss.ScalaCssReact._
import scalaz.std.string.stringInstance
import scalaz.std.tuple._
import shipreq.base.util.univeq._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.DataValidators.{reqType => V}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.protocol.{ServerSideProcInvoker, UpdateConfigCmd}
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.cfg.shared._
import shipreq.webapp.client.project.app.state.{ChangeListener, Global}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.Widgets
import DataImplicits._

object CfgReqTypes {

  case class Props(remote    : ServerSideProcInvoker[UpdateConfigCmd.ToModifyCustomReqTypes, ErrorMsg, VerifiedEvent.Seq],
                   global    : Global,
                   filterDead: StateSnapshot[FilterDead],
                   usageShow : Usage.Show) {
    def component = Component(this)
  }
  implicit val reusability = Reusability.derive[Props]

  val fields = FieldSet3[CustomReqType](_.mnemonic.value, _.name, _.imp)(("", "", ImplicationRequired.Not))
  val storesAndState = TypicalStoresAndState(fields).keyedBy[CustomReqTypeId]
  import storesAndState._
  val changeListener = ChangeListener.store(savedRowStoreS)(_.customReqTypes.all, _.config.reqTypes.custom.get)

  val Component =
    ScalaComponent.builder[Props]("Cfg: Req Types")
      .initialStateFromProps(initialState)
      .renderBackend[Backend]
      .configure(changeListener.install(_.global))
      .build

  private def initialState(p: Props): S =
    State(
      newRowStore.initState,
      savedRowStore.initStateIM(p.global.unsafeProject().config.reqTypes.custom))

  // ===================================================================================================================
  final class Backend($: BackendScope[Props, S]) extends OnUnmount {
    private val project    = Px.props($).map(_.global.unsafeProject()).withReuse.manualRefresh
    private val filterDead = Px.props($).map(_.filterDead.value).withReuse.manualRefresh
    private val usageShow  = Px.props($).map(_.usageShow).withReuse.manualRefresh

    private val crudIO = Px.props($).withReuse.autoRefresh.map(p => CrudActionIO(p.remote)(
      create  = UpdateConfigCmd.CustomReqTypeCreate,
      update  = UpdateConfigCmd.CustomReqTypeUpdate,
      delete  = UpdateConfigCmd.CustomReqTypeDelete,
      restore = UpdateConfigCmd.CustomReqTypeRestore,
    ).contramapValues(UpdateConfigCmd.CustomReqTypeValues.tupled))

    private val supp = TypicalSupp(storesAndState)(crudIO.value(), $)

    private val onWhenImplicationRequired = On <=> ImplicationRequired

    private def validatorState(k: Option[CustomReqTypeId]): V.State =
      validatorState(k, $.state.runNow())

    private def validatorState(k: Option[CustomReqTypeId], s: => S): V.State =
      V.State(k, () => savedRowStoreS.getAllP(s))

    private val rowE = {
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

    e.applyRowUpdateAndRevert(savedRowStoreS, newRowStoreS)(_._1.subject)
      .applyOnEditFinishedK(saveFn)(_._1.subject)
    }

    private def checkbox(i: ImplicationRequired) =
      Widgets.checkbox(onWhenImplicationRequired from i)

    private val usageFn = Usage((_: ReqType).reqTypeId)(
      _.reqTypeCount,
      Filter.Valid.reqType,
      project, filterDead, usageShow)

    private val cfgTable = {
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

    private val table = {
      val headerRow = CfgTable.header(List[TagMod](
        FieldNames.mnemonic,
        FieldNames.name,
        TagMod("Implication", <.br, "Required"),
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

      () => cfgTable.justTheTable(headerRow, staticRows)
    }

    def render: VdomElement = {
      Px.refresh(project, filterDead, usageShow)
      cfgTable.wrapWithFilterDeadCheckbox2(
        SetStateFn((o, cb) => $.props.flatMap(_.filterDead.setStateOption(o, cb))),
        cfgTable.newButton,
        table())(
        BaseStyles.containerLarge, Style.cfg.reqTypes)
    }
  }
}