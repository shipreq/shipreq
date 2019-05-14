package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import shipreq.base.util.FxModule._
import shipreq.webapp.base.AssetManifest
import shipreq.webapp.base.protocol.ProjectSpaProtocols
import shipreq.webapp.gen.transform.ProjectSpaLoader
import shipreq.webapp.server.app.{Global, LiftDispatcher}
import shipreq.webapp.server.lib.SingleOpStatelessSnippet
import shipreq.webapp.server.protocol._

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

  val EntryPoint = ClientSideProcInvoker(ProjectSpaProtocols.EntryPoint, ResourceBundle)

  override def render = {
    val projectId = LiftDispatcher.ProjectIdVar.is
    assert(projectId != null, "Project SPA snippet invoked without a ProjectId")

    val user = currentUser_!()

    val logic = Global.logic.projectSpa

    val init: ProjectSpaProtocols.InitPageData =
      logic.initPage(projectId, user.username).unsafeRun()

    "*" #> (
      ProjectSpaLoader.xml(user.username, init.projectName) :+
        EntryPoint.invokeOnLoadHtml(init))
    // ClientFn.ProjectSpa.htmlToLoadJsAndRun(Assets.ProjectSpa)(initData(user.username, p)))
  }
}