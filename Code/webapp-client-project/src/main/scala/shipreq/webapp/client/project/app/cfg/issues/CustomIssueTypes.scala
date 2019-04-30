package shipreq.webapp.client.project.app.cfg.issues

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import vdom.html_<^._
import ScalazReact._
import japgolly.scalajs.react.extra._
import scala.language.reflectiveCalls
import scalacss.ScalaCssReact._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.DataValidators.{customIssueType => V, hashRefKey => VH}
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.util.TextMod
import shipreq.webapp.base.protocol.ProjectSpaProtocols.WsReqRes.CustomIssueTypeCrud
import shipreq.webapp.base.ui.AutosizeTextarea
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.cfg.shared._
import shipreq.webapp.client.project.app.state.{ChangeListener, ClientData}
import shipreq.webapp.client.project.lib.DataReusability._
import DataImplicits._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol.ServerSideProcInvoker

private[issues] object CustomIssueTypes {

  final case class Props(remote    : ServerSideProcInvoker[CustomIssueTypeCrud.RequestType, ErrorMsg, VerifiedEvent.Seq],
                         clientData: ClientData,
                         filterDead: StateSnapshot[FilterDead],
                         usageShow : Usage.Show) {
    @inline def component = Component(this)
  }
  implicit val reusability = Reusability.derive[Props]

  val fields = FieldSet2[CustomIssueType](_.key.value, _.desc getOrElse "")(("", ""))
  val storesAndState = TypicalStoresAndState(fields).keyedBy[CustomIssueTypeId]
  import storesAndState._
  val changeListener = ChangeListener.store(savedRowStoreS)(_.customIssueTypes, _.config.customIssueTypes.get)

  val Component =
    ScalaComponent.builder[Props]("Cfg: User-Defined Issue Types")
      .initialStateFromProps(initialState)
      .renderBackend[Backend]
      .configure(changeListener.install(_.clientData))
      .configure(AutosizeTextarea.applyToChildren("textarea"))
      .build

  private def initialState(p: Props): S =
    State(
      newRowStore.initState,
      savedRowStore.initStateIM(p.clientData.project().config.customIssueTypes))

  private def validatorState(k: Option[CustomIssueTypeId], cd: CallbackTo[ClientData]): S => V.State = {
    val tagData: Px[List[(Option[TagId], HashRefKey)]] =
      Px.callback(cd.map(_.project().config.tags)).withReuse.autoRefresh
        .map(_.valuesIterator.map(t => t.tag.keyO.map(k => (t.tag.id.some, k))).filterDefined.toList)

    val tags: VH.SubState[TagId] =
      VH.SubState(None, () => tagData.value())

    s => {
      def customIssues = VH.SubState[CustomIssueTypeId](
        k, () => savedRowStoreS.getAllP(s).map(i => (i.id.some, i.key)))

      V.State(tags, customIssues)
    }
  }

  final class Backend($: BackendScope[Props, S]) extends OnUnmount {
    val project    = Px.props($).map(_.clientData.project()).withReuse.manualRefresh
    val filterDead = Px.props($).map(_.filterDead.value).withReuse.manualRefresh
    val usageShow  = Px.props($).map(_.usageShow).withReuse.manualRefresh

    val crudIO =
      Px.props($).withReuse.autoRefresh.map(p => CrudActionIO(p.remote))

    val supp = TypicalSupp(storesAndState)(crudIO.value(), $)

    def valState(k: Option[CustomIssueTypeId]) = validatorState(k, $.props.map(_.clientData))

    val rowE = {
      val keyE  = Editors.textInputEditor.applyStatefulValidator(V.key.unnamedFn)
      val descE = Editors.textareaEditor.applyStatefulValidator(V.desc.unnamedFn)
      val e = Editor.merge2S(fields, keyE, descE).tupleI.zoomU[S]

      val saveFn = crudIO.map(c =>
        Persistence.asyncSaveS(V.all, savedRowStoreS)(
          newRowStoreS,
          valState(None),
          k => valState(k.some),
          supp.saveNeed(p => (p.key, p.desc)),
          c.createIO, c.updateIO,
          $ runState _)
        ).extract

      supp.addEditorFeatures2(e)(saveFn, _._1.customIssues.subject)
    }

    val usageFn = Usage((_: CustomIssueType).id)(
      _.atomScan.issueCounts,
      Filter.Valid.issue,
      project, filterDead, usageShow)

    val cfgTable = {
      def rowRenderer =
        new CfgTable.RowRenderer[CustomIssueType, rowE.View, (TagMod, TagMod, Option[Usage.View])] {
          override def newRow = {
            case (key, desc) => (key, desc, None)
          }
          override def savedRow = {
            case ((key, desc), i) => (key, desc, Some(usageFn(i)))
          }
          override def deletedRow = i =>
            (i.key.value, TextMod.nonBlank from i.desc, Some(usageFn(i)))

          override def render = {
            case (key, desc, usage) =>
              Seq(key, desc, usage.whenDefined)
        }
      }

      // Few $.state.runNow() in CustomIssueTypes - safe because in lambdas
      def s = $.state.runNow()
      CfgTable(rowE, savedRowStoreS, newRowStoreS).build(
          _.key, rowRenderer,
          i => (valState(None)(s), i),
          k => (valState(k.some)(s), savedRowStoreS.getI(k)(s)),
          () => supp.deletion.value(),
          _.live,
          $.props.map(_.filterDead.value),
          $)
    }

    val table = {
      val headerRow = CfgTable.header(List(FieldNames.hashRefKey, FieldNames.desc, FieldNames.usage))
      () => cfgTable.justTheTable(headerRow, Stream.empty)
    }

    def render: VdomElement = {
      Px.refresh(project, filterDead, usageShow)
      cfgTable.wrapWithFilterDeadCheckbox2(
        SetStateFn((o, cb) => $.props.flatMap(_.filterDead.setStateOption(o, cb))),
        cfgTable.newButton,
        table())
    }
  }
}
