package com.beardedlogic.usecase
package app

import net.liftweb.common.{Logger, Box}
import net.liftweb.http.RequestVar
import db.{UseCaseSummary, Project}
import DI.DaoProvider
import lib.Types._
import lib.SnippetHelpers._
import feature.Navbar

object RequestVars extends Logger {

  object Navbar extends RequestVar[Navbar](fail("Navbar"))

  object SoleProject extends RequestVar[Project](discoverProject openOr fail("SoleProject"))

  object UseCases extends RequestVar[List[UseCaseSummary]](loadUseCases)

  object SoleUseCaseId extends RequestVar[UseCaseIdentId](urlProvidedUseCaseId openOr fail("SoleUseCaseId"))

  // -------------------------------------------------------------------------------------------------------------------

  private def fail(name: String): Nothing = {
    warn("No value available to RequestVar: " + name)
    redirectHome
  }

  private def urlProvidedProjectId: Box[ProjectId] =
    AppSiteMap.Project.currentValue or
    AppSiteMap.ReadOwnUcs.currentValue

  private def urlProvidedUseCaseId: Box[UseCaseIdentId] =
    AppSiteMap.UseCaseEditor.currentValue

  private def discoverProject: Box[Project] = {
    def discoverProjectByProjectId: Box[Project] = for {
      id <- urlProvidedProjectId
      p <- DaoProvider.withSession(_.findProject(id))
    } yield p

    def discoverProjectByUseCaseId: Box[Project] = for {
      id <- urlProvidedUseCaseId
      p <- DaoProvider.withSession(_.findProjectByUc(id))
    } yield p

    discoverProjectByProjectId.or(discoverProjectByUseCaseId)
  }

  def getProjectId: ProjectId =
    urlProvidedProjectId getOrElse SoleProject.get.id

  private def loadUseCases: List[UseCaseSummary] =
    DaoProvider.withSession(_.summariseUseCases(getProjectId))
}
