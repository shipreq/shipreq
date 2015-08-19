package shipreq.webapp.client.app

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra.router._
import org.scalajs.dom
import scalacss.Defaults._
import scalacss.ScalaCssReact._

import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.base.protocol.ProjectSPA
import shipreq.webapp.client.app.state.ClientData
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.lib.HideDead

object ProjectSpaMain {

  def main(r: ProjectSPA): Callback = {
    val cp = ClientProtocol.Default
    ClientData.init(cp, r.projectInit, clientData => Callback {
      ui.Style.addToDocument()
      val baseUrl = determineBaseUrl(dom.window.location.href)
      val router = Router(baseUrl, routerConfig(r, cp, clientData))
      router() render dom.document.getElementById("tgt")
    })
  }

  // ===================================================================================================================
  // Routes

  sealed trait Page
  case object Index       extends Page
  case object CfgFields   extends Page
  case object CfgIssues   extends Page
  case object CfgReqTypes extends Page
  case object CfgTags     extends Page
  case object ReqTable    extends Page

  implicit def pageEq: UnivEq[Page] = UnivEq.derive

  val pages = NonEmptyVector[Page](
    Index, ReqTable, CfgFields, CfgIssues, CfgReqTypes, CfgTags)

  def routerConfig(r: ProjectSPA, cp: ClientProtocol, cd: ClientData) =
    RouterConfigDsl[Page].buildConfig { dsl =>

      def reqTable =
        ui.reqtable.ReqTable.Props(cd, cp, r.createContent, r.updateContent, HideDead).component

      def cfgIssues =
        ui.cfg.issues.CfgIssues.Props(cp, r.issueTypeCrud, r.reqTypeImpMod, r.fieldMandMod, cd, HideDead).component

      def cfgReqTypes =
        ui.cfg.CfgReqTypes.Props(cp, r.reqTypeCrud, cd, HideDead).component

      def cfgTags =
        ui.cfg.tags.CfgTags.Props(cp, r.tagCrud, cd, HideDead).component

      def cfgFields =
        ui.cfg.fields.CfgFields.Props(cp, r.fieldCrud, cd, HideDead).component

      import dsl._

      ( staticRoute(root,            Index      ) ~> renderR(IndexComponent(_))
      | staticRoute("/table",        ReqTable   ) ~> render(reqTable)
      | staticRoute("/cfg/fields",   CfgFields  ) ~> render(cfgFields)
      | staticRoute("/cfg/issues",   CfgIssues  ) ~> render(cfgIssues)
      | staticRoute("/cfg/reqtypes", CfgReqTypes) ~> render(cfgReqTypes)
      | staticRoute("/cfg/tags",     CfgTags    ) ~> render(cfgTags)
      | trimSlashes
      ).notFound(redirectToPage(Index)(Redirect.Replace))
        .renderWith(layout)
        .verify(Index, pages.whole: _*)
    }

  def determineBaseUrl(url: String) = {
    val pat = "^([^/#?]+//[^/#?]+/[^/#?]+/[^/#?]+)(?:[/#?].*|$)".r.pattern
    val m = pat.matcher(url)
    if (m.matches) BaseUrl(m group 1) else BaseUrl(url).endWith_/
  }

  // ===================================================================================================================
  // UI

  val IndexComponent = ReactComponentB[RouterCtl[Page]]("Index")
    .render(ctl =>
      <.ul(
        pages.whole.map(p =>
          <.li(ctl.link(p)(p.toString))))
    ).build

  def layout(ctl: RouterCtl[Page], res: Resolution[Page]): ReactElement =
    res.page match {
      case Index => res.render()
      case _ =>
        <.div(
          <.div(
            ^.textAlign.right,
            ^.paddingRight := "0.6ex",
            ^.marginTop := "-14px",
            ctl.link(Index)("← Back")),
          res.render())
    }
}
