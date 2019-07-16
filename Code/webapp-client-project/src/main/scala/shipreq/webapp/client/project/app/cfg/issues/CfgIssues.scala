package shipreq.webapp.client.project.app.cfg.issues

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import scalacss.ScalaCssReact._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.FilterDead
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol.ProjectSpaProtocols.WsReqRes._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.cfg.shared.Usage
import shipreq.webapp.client.project.app.state.Global

object CfgIssues {

  final case class Props(a         : ServerSideProcInvoker[UpdateConfigCmd.ToModifyCustomIssueTypes, ErrorMsg, VerifiedEvent.Seq],
                         b         : ServerSideProcInvoker[ReqTypeImplicationMod.RequestType, ErrorMsg, VerifiedEvent.Seq],
                         c         : ServerSideProcInvoker[FieldMandatorinessMod.RequestType, ErrorMsg, VerifiedEvent.Seq],
                         g         : Global,
                         filterDead: StateSnapshot[FilterDead],
                         usageShow : Usage.Show) {
    @inline def component = Component(this)
  }

  val Component =
    ScalaComponent.builder[Props]("Cfg: Issues")
      .render_P { p =>
        import p._

        <.div(BaseStyles.containerLarge, Style.cfg.issues,

          <.h4("User-Defined Issue Types"),
          CustomIssueTypes.Props(a, g, filterDead, usageShow).component,

          <.h4("Other Causes of Issues", ^.marginTop := "3em"),
          <.div(^.cls := "other", ^.display.flex, ^.justifyContent.spaceAround,
            <.div(ReqTypeImplication.Props(b, g).component),
            <.div(MandatoryFields.Props(c, g).component)))
      }
      .build
}
