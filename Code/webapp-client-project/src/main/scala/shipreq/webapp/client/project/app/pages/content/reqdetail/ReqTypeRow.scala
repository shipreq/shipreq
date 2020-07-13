package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.CreateContentCmd
import shipreq.webapp.base.ui.semantic.{Button, Colour => SColour, Icon}
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.client.project.app.Style.{reqdetail => *}
import shipreq.webapp.client.project.app.state.NewEvents
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets._

private[reqdetail] object ReqTypeRow {
  import Row.{ReqType => row}
  import EditorFeature.FieldKey.{ReqType => field}

  final case class Props(reqType         : ReqType,
                         live            : Live,
                         filterDead      : FilterDead,
                         editor          : EditorFeature.ReadWrite.For[field.type],
                         view            : Reusable[ViewReq[VdomTag]],
                         newReqAsync     : AsyncFeature.ReadWrite.D0[ErrorMsg],
                         sspCreateContent: ServerSideProcInvoker[CreateContentCmd, ErrorMsg, NewEvents],
                         reqDetailRC     : RouterCtl[ExternalPubid]) {
    @inline def render: VdomElement = Component.withKey(row.key)(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private def render(p: Props): VdomNode =
    Shared.renderRow(
      row        = row,
      name       = SpecialBuiltInField.ReqType.name,
      headerLive = Live,
      dataLive   = p.live,
    )(renderRowData(_, p))

  private def renderRowData(cell: Shared.DataCell, p: Props): VdomNode = {

    val editor =
      p.editor.themedRenderOr(())(p.view.editable(field).getOrElse(EmptyVdom))

    def newButton = {
      val async = p.newReqAsync

      val newReq: Callback =
        Callback.byName {
          val cmd = CreateContentCmd.empty(p.reqType.reqTypeId)
          async.write.onFailureShowAndForget(
            p.sspCreateContent(cmd).rightFlatTapSync(newEvents =>
              Callback.traverseOption(newEvents.summary.newReqIds.headOption) { reqId =>
                import newEvents.project
                val pubid = project.content.reqs.need(reqId).pubid.external(project)
                p.reqDetailRC.set(pubid)
              }
            )
          )
        }

      val asyncInProgress =
        async.isInProgress

      Button(
        tipe   = Button.Type.BasicIconAndText(Icon.Plus, "New " + p.reqType.mnemonic.value),
        state  = Button.State.loadingWhen(asyncInProgress),
        colour = SColour.Green,
      ).tag(^.onClick --> newReq.unless_(asyncInProgress))
    }

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

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
