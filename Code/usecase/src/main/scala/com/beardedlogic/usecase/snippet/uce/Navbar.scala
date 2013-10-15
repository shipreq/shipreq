package com.beardedlogic.usecase.snippet.uce

import net.liftweb.http.DispatchSnippet
import net.liftweb.util.Helpers._
import com.beardedlogic.usecase.app.AppSiteMap
import com.beardedlogic.usecase.app.RequestVars.{UseCases, SoleProject}
import com.beardedlogic.usecase.db.{UseCaseSummary, Project}
import com.beardedlogic.usecase.lib.SnippetHelpers
import com.beardedlogic.usecase.lib.Types.UseCaseIdentId
import AppSiteMap.Implicits._

class Navbar(ucId: UseCaseIdentId) extends DispatchSnippet with SnippetHelpers {
  override def dispatch = { case _ => render }

  @inline def isActive(uc: UseCaseSummary): Boolean = uc.id == ucId

  def render = {
    val ucs = UseCases.get
    val p = SoleProject.get
    val currentUc = ucs.find(isActive) getOrElse redirectHome

    (".ucs .dropdown-menu li" #> ucs.filterNot(isActive).map(renderUseCaseLink)) andThen
    ".project" #> renderProjectLink(p) &
    ".active-uc" #> renderActiveUseCase(currentUc)
  }

  def renderProjectLink(p: Project) = (
    "* *" #> p.name &
    "* [href]" #> AppSiteMap.Project.relativeUrl(p.id)
  )

  def renderActiveUseCase(uc: UseCaseSummary) = (
    ".num" #> uc.number.toString &
    ".cur-uc-title *" #> uc.title
  )

  def renderUseCaseLink(uc: UseCaseSummary) = (
    ".num" #> uc.number.toString &
    (if (isActive(uc))
      ".title *" #> uc.title &
      "li [disabled]" #> "disabled" &
      "a [disabled]" #> "disabled"
    else
      ".title" #> uc.title &
      "a [href]" #> AppSiteMap.UseCaseEditor.relativeUrl(uc.id))
  )
}
