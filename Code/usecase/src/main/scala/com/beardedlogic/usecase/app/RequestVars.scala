package com.beardedlogic.usecase
package app

import net.liftweb.common.{Box, Full}
import net.liftweb.http.RequestVar
import db.{UseCaseSummary, Project}
import lib.DI.DaoProvider
import lib.Types._
import lib.SnippetHelpers._

object RequestVars {

  object SoleProject extends RequestVar[Project](discoverProject openOr redirectHome)

  object UseCases extends RequestVar[List[UseCaseSummary]](loadUseCases)

  // -------------------------------------------------------------------------------------------------------------------

  private def urlProvidedProjectId: Box[ProjectId] = AppSiteMap.Project.currentValue
  private def urlProvidedUseCaseId: Box[UseCaseIdentId] = AppSiteMap.UseCaseEditor.currentValue

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
