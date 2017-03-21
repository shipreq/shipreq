package shipreq.webapp.client.project.app.cfg.issues

import japgolly.scalajs.react._, vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import shipreq.webapp.base.data.FilterDead
import shipreq.webapp.base.protocol._
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.project.app.cfg.shared.Usage
import shipreq.webapp.client.project.app.state.ClientData

object CfgIssues {

  final case class Props(cp        : ClientProtocol,
                         a         : CustomIssueTypeCrud.Instance,
                         b         : ReqTypeImplicationMod.Instance,
                         c         : FieldMandatorinessMod.Instance,
                         cd        : ClientData,
                         filterDead: StateSnapshot[FilterDead],
                         usageShow : Usage.Show) {
    @inline def component = Component(this)
  }

  val Component =
    ScalaComponent.builder[Props]("Cfg: Issues")
      .render_P { p =>
        import p._
        <.section(
          <.h4("User-Defined Issue Types"),
          CustomIssueTypes.Props(cp, a, cd, filterDead, usageShow).component,
          <.h4("Other Causes of Issues"),
          <.table(<.tbody(
            <.td(ReqTypeImplication.Props(cp, b, cd).component),
            <.td(MandatoryFields.Props(cp, c, cd).component))))
      }
      .build
}
