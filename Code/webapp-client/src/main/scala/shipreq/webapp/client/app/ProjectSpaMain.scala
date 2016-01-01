package shipreq.webapp.client.app

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra.router.{RouterCtl => RouterCtl_, _}
import org.scalajs.dom
import scalacss.Defaults._
import scalacss.ScalaCssReact._

import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.base.filter.FilterSpec
import shipreq.webapp.base.protocol.ProjectSPA
import shipreq.webapp.client.app.state.ClientData
import shipreq.webapp.client.data.{FilterDead, HideDead}
import shipreq.webapp.client.protocol.ClientProtocol

object ProjectSpaMain {

  def main(r: ProjectSPA): Callback = {
    val cp = ClientProtocol.Default
    ClientData.init(cp, r.projectInit, clientData => Callback {
      Style.addToDocument()
      val baseUrl = determineBaseUrl(dom.window.location.href)
      val router = Router(baseUrl, routerConfig(r, cp, clientData))
      router() render dom.document.getElementById("tgt")
    })
  }

  /**
   * This is used so that "Usage" columns in config screens (within this SPA) can have links that initialise the
   * ReqTable to a given state.
   *
   * It is cleared after a single use.
   *
   * Being a global variable, this is a shithouse solution and will be replaced eventually.
   */
  case class ReqTableNextState(fd: FilterDead, fs: Option[FilterSpec]) {
    def set: Callback =
      Callback(_reqTableNextState = Some(this))
  }

  private var _reqTableNextState: Option[ReqTableNextState] = None

  private def reqTableNextState(): ReqTableNextState = {
    val s = _reqTableNextState getOrElse ReqTableNextState(HideDead, None)
    _reqTableNextState = None
    s
  }

  // ===================================================================================================================
  // Routes

  type RouterCtl = RouterCtl_[Page]

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

      def reqTable = {
        val s = reqTableNextState()
        reqtable.ReqTable.Props(cd, cp, r.createContent, r.updateContent, s.fd, s.fs).component
      }

      def cfgIssues(ctl: RouterCtl) =
        cfg.issues.CfgIssues.Props(cp, r.issueTypeCrud, r.reqTypeImpMod, r.fieldMandMod, cd, HideDead, ctl).component

      def cfgReqTypes(ctl: RouterCtl) =
        cfg.reqtypes.CfgReqTypes.Props(cp, r.reqTypeCrud, cd, HideDead, ctl).component

      def cfgTags =
        cfg.tags.CfgTags.Props(cp, r.tagCrud, cd, HideDead).component

      def cfgFields =
        cfg.fields.CfgFields.Props(cp, r.fieldCrud, cd, HideDead).component

      import dsl._

      ( staticRoute(root,            Index      ) ~> renderR(IndexComponent(_))
      | staticRoute("/table",        ReqTable   ) ~> render(reqTable)
      | staticRoute("/cfg/fields",   CfgFields  ) ~> render(cfgFields)
      | staticRoute("/cfg/issues",   CfgIssues  ) ~> renderR(cfgIssues)
      | staticRoute("/cfg/reqtypes", CfgReqTypes) ~> renderR(cfgReqTypes)
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

  val IndexComponent = ReactComponentB[RouterCtl]("Index")
    .render_P(ctl =>
      <.ul(
        pages.whole.map(p =>
          <.li(ctl.link(p)(p.toString))))
    ).build

  def layout(ctl: RouterCtl, res: Resolution[Page]): ReactElement =
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
