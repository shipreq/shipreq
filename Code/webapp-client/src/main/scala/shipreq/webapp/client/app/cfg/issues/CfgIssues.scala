package shipreq.webapp.client.app.cfg.issues

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import shipreq.webapp.base.protocol._
import shipreq.webapp.client.app.ProjectSpaMain
import shipreq.webapp.client.app.state.ClientData
import shipreq.webapp.client.data.FilterDead
import shipreq.webapp.client.protocol.ClientProtocol

object CfgIssues {

  case class Props(cp        : ClientProtocol,
                   a         : CustomIssueTypeCrud.Instance,
                   b         : ReqTypeImplicationMod.Instance,
                   c         : FieldMandatorinessMod.Instance,
                   cd        : ClientData,
                   filterDead: FilterDead,
                   routerCtl : ProjectSpaMain.RouterCtl) {
    @inline def component = Component(this)
  }

  val Component =
    ReactComponentB[Props]("Cfg: Issues")
      .render_P { p =>
        import p._
        <.section(
          <.h4("User-Defined Issue Types"),
          CustomIssueTypes.Props(cp, a, cd, filterDead, routerCtl).component,
          <.h4("Other Causes of Issues"),
          <.table(<.tbody(
            <.td(ReqTypeImplication.Props(cp, b, cd).component),
            <.td(MandatoryFields.Props(cp, c, cd).component))))
      }
      .build
}
