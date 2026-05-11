package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import shipreq.base.util.FxModule._
import shipreq.webapp.member.protocol.entrypoint.ProjectSpaEntryPoint
import shipreq.webapp.server.config.Global
import shipreq.webapp.server.http.LiftDispatcher
import shipreq.webapp.server.protocol.entrypoint.{ClientSideProcInvoker, LoadJs}
import shipreq.webapp.ssr.Html
import shipreq.webapp.ssr.SsrSharedData.ProjectSpaLoaderData

object ProjectSpa extends SingleOpStatelessSnippet {

  private def ResourceBundle = {
    val sjsm = Global.config.server.scalaJsManifest
    LoadJs.Bundle(
      LoadJs.Resource(assetManifest.semanticJs),
      LoadJs.Resource(assetManifest.reactJs),
      LoadJs.Resource(assetManifest.reactDomJs),
      LoadJs.Resource(assetManifest.reactDomServerJs),
      LoadJs.Resource(assetManifest.memberLibBundleJs),
      LoadJs.Resource(sjsm.project),
      LoadJs.Resource(assetManifest.katexCss),
      LoadJs.Resource(assetManifest.katexJs),
      LoadJs.Resource(assetManifest.prismJsCss),
      LoadJs.Resource(assetManifest.prismJsCore),
      LoadJs.Resource(assetManifest.prismJsAutoloader),
      LoadJs.Resource(assetManifest.prismJsLineNumbers),
      LoadJs.Resource(assetManifest.prismJsLineNumbersCss),
      LoadJs.Resource(assetManifest.prismJsMatchBraces),
      LoadJs.Resource(assetManifest.prismJsMatchBracesCss),
    )
  }

  val EntryPoint = ClientSideProcInvoker(ProjectSpaEntryPoint.proc, ResourceBundle)

  private[this] val ssrFallback = Html(
    """<div style="margin-top:33vh;text-align:center;font-size:150%;color:#333;">loading ...</div>""")

  override def render = {
    val projectId = LiftDispatcher.ProjectIdVar.is
    assert(projectId != null, "Project SPA snippet invoked without a ProjectId")

    val user = currentUser_!()

    val logic = Global.logic.projectSpa

    val init: ProjectSpaEntryPoint.InitData =
      logic.initPage(projectId, user.id, user.username, assetManifest)
        .unsafeRun()
        .getOrElse(throw SnippetError.MemberDataNotFound)

    val loaderData =
      ProjectSpaLoaderData(user.username, init.projectName, assetManifest)

    val loaderHtml =
      Global.ssr.projectSpaLoader(loaderData).unsafeRun()
        .getOrElse(ssrFallback)
        .xml

    "*" #> (loaderHtml :+ EntryPoint.invokeOnLoadHtml(init))
  }
}