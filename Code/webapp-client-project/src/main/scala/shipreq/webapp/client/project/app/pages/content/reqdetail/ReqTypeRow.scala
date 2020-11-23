package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.client.project.app.Style.{reqdetail => *}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.util.DataReusability._
import shipreq.webapp.client.project.widgets.{ImplyNewReqButton, ProjectWidgets, ViewReq}
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.library.NewEvents
import shipreq.webapp.member.project.protocol.websocket.CreateContentCmd

private[reqdetail] object ReqTypeRow {
  import Row.{ReqType => row}
  import EditorFeature.FieldKey.{ReqType => field}

  final case class Props(subject         : ReqId,
                         reqType         : ReqType,
                         live            : Live,
                         filterDead      : FilterDead,
                         editor          : EditorFeature.ReadWrite.For[field.type],
                         view            : Reusable[ViewReq[VdomTag]],
                         projectWidgets  : ProjectWidgets.AnyCtx,
                         reqTypes        : ReqTypes,
                         newReqState     : StateSnapshot[ImplyNewReqButton.State],
                         newReqAsync     : AsyncFeature.ReadWrite.D0[ErrorMsg],
                         sspCreateContent: ServerSideProcInvoker[CreateContentCmd, ErrorMsg, NewEvents],
                         reqDetailRC     : RouterCtl[ExternalPubid]) {
    @inline def render: VdomElement = Component.withKey(row.key)(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  final class Backend($: BackendScope[Props, Unit]) {

    private val onCreate = {
      val click: ImplyNewReqButton.Method => ImplyNewReqButton.Click => Callback =
        method => click =>
          CallbackOption.traverseOption(click.value.reqTypeIdOption) { reqTypeId =>
            $.props.flatMap { p =>

              val cmd: CreateContentCmd =
                method match {
                  case ImplyNewReqButton.Method.New   => CreateContentCmd.empty(reqTypeId)
                  case ImplyNewReqButton.Method.Imply => CreateContentCmd.imply(reqTypeId, Set1(p.subject))
                }

              def onSuccess(newEvents: NewEvents, reqId: ReqId): Callback = {
                import newEvents.project
                val pubid = project.content.reqs.need(reqId).pubid.external(project)
                if (click.targetsNewTab_?) {
                  val url = p.reqDetailRC.urlFor(pubid).value
                  CallbackTo.windowOpen(url, focus = false).void
                } else
                  p.reqDetailRC.set(pubid)
              }

              p.newReqAsync.write.onFailureShowAndForget(
                p.sspCreateContent(cmd).rightFlatTapSync(newEvents =>
                  Callback.traverseOption(newEvents.summary.newReqIds.headOption)(onSuccess(newEvents, _))
                )
              )
            }
          }
      Some(Reusable.byRef(click))
    }

    private val onReqTypeSelect = {
      val select: ImplyNewReqButton.DropdownValue => Callback =
        rowKey => $.props.flatMap(_.newReqState.setState(Some(rowKey)))
      Some(Reusable.byRef(select))
    }

    private def renderRowData(cell: Shared.DataCell, p: Props): VdomNode = {
      val args = EditorFeature.EditorArgs.ForReqTypeEditor(p.reqTypes)
      val editor = p.editor.themedRenderOr(args)(p.view.editable(field).getOrElse(EmptyVdom))

      def defaultSelected =
        Some(CreateFeature.RowKey.req(p.reqType.reqTypeId))

      def newButton =
        ImplyNewReqButton.Props(
          state      = p.newReqState.value.orElse(defaultSelected),
          reqTypes   = p.reqTypes,
          allowRCG   = Deny,
          pw         = p.projectWidgets,
          selectItem = onReqTypeSelect,
          create     = onCreate,
          inProgress = p.newReqAsync.isInProgress,
          basic      = true,
        ).render

      p.live match {
        case Live =>
          cell.nonDirectlyEditableNavParent(
            <.div(*.reqTypeRow,
              <.div(*.reqTypeRowL, editor),
              <.div(*.reqTypeRowR, newButton)))

        case Dead =>
          cell.nonDirectlyEditableNavParent(editor)
      }
    }

    def render(p: Props): VdomNode =
      Shared.renderRow(
        row        = row,
        name       = SpecialBuiltInField.ReqType.name,
        headerLive = Live,
        dataLive   = p.live,
      )(renderRowData(_, p))
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
