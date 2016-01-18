package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import shipreq.webapp.base.UiText
import scalacss.ScalaCssReact._
import scalaz.{\/, -\/, \/-}
import shipreq.base.util.NonEmptyVector
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
// import shipreq.webapp.client.app.Style.{reqtable => *}
import shipreq.webapp.client.feature.AsyncActionFeature
import shipreq.webapp.client.lib._
import shipreq.webapp.client.widgets.DragToReorder
import AsyncActionFeature.Table.RowState
import AsyncActionFeature.{Locked, renderLocked}
import DataReusability._
import DomUtil._

object ReqDetail {

  case class Props(rtm: ReqType.Mnemonic,
                   pos: ReqTypePos,
                   project: Project) {
    def component = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): ReactElement =
      p.project.findReq(p.rtm, p.pos) match {
        case \/-(req)                                 => renderDetail(p, req)
        case -\/(PubidQueryError.InvalidReqType)      => renderNotFound(s"${UiText.FieldNames.reqType} ${p.rtm.value} not found.")
        case -\/(PubidQueryError.InvalidPos(rt, len)) => renderNotFound(s"${p.rtm.value}-${p.pos.value} not found.")
      }

    def renderNotFound(failureReason: String): ReactElement =
      <.div(
        <.h2("ERROR"),
        <.h5(failureReason))

    def renderDetail(p: Props, req: Req): ReactElement = {
      <.div(
        <.h2("Req found: (TODO)"),
        <.code(<.pre(req.toString)))
    }
  }

  val Component = ReactComponentB[Props]("ReqDetail")
    .renderBackend[Backend]
    .build
}
