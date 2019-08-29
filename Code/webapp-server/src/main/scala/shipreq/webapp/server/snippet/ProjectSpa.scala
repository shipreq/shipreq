package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import shipreq.base.util.FxModule._
import shipreq.webapp.base.AssetManifest
import shipreq.webapp.base.protocol.ProjectSpaEntryPoint
import shipreq.webapp.server.app.{Global, LiftDispatcher}
import shipreq.webapp.server.lib.SingleOpStatelessSnippet
import shipreq.webapp.server.protocol._
import shipreq.webapp.ssr.Html
import shipreq.webapp.ssr.SsrSharedData.ProjectSpaLoaderData

object ProjectSpa extends SingleOpStatelessSnippet {

  private def ResourceBundle =
    LoadJs.Bundle(
      LoadJs.Resource(AssetManifest.semanticJs),
      LoadJs.Resource(AssetManifest.reactJs),
      LoadJs.Resource(AssetManifest.reactDomJs),
      LoadJs.Resource(AssetManifest.reactDomServerJs),
      LoadJs.Resource(AssetManifest.memberLibBundleJs),
      LoadJs.Resource(AssetManifest.webappClientProjectJs),
      LoadJs.Resource(AssetManifest.katexCss),
      LoadJs.Resource(AssetManifest.katexJs))

  val EntryPoint = ClientSideProcInvoker(ProjectSpaEntryPoint.proc, ResourceBundle)

  private[this] val ssrFallback = Html(
    """<div style="margin-top:33vh;text-align:center;font-size:150%;color:#333;">loading ...</div>""")

  override def render = {
    val projectId = LiftDispatcher.ProjectIdVar.is
    assert(projectId != null, "Project SPA snippet invoked without a ProjectId")

    val user = currentUser_!()

    val logic = Global.logic.projectSpa

    val init: ProjectSpaEntryPoint.InitData =
      logic.initPage(projectId, user.username).unsafeRun()

    val loaderData =
      ProjectSpaLoaderData(user.username, init.projectName)

    val loaderHtml =
      Global.ssr.projectSpaLoader(loaderData).unsafeRun()
        .getOrElse(ssrFallback)
        .xml

    "*" #> (loaderHtml :+ EntryPoint.invokeOnLoadHtml(init))
  }
}