package com.beardedlogic.usecase
package app

import net.liftweb.common.{Box, Full}
import net.liftweb.http.RequestVar
import db.Project
import lib.DI.DaoProvider
import lib.Types._
import lib.SnippetHelpers._

object RequestVars {

  object SoleProject extends RequestVar[Project](discoverProject openOr redirectHome)

  private def discoverProjectId: Box[ProjectId] = AppSiteMap.Project.currentValue
  private def discoverUseCaseId: Box[UseCaseIdentId] = AppSiteMap.UseCaseEditor.currentValue

  private def discoverProject: Box[Project] = {
    def discoverProjectByProjectId: Box[Project] = for {
      id <- discoverProjectId
      p <- DaoProvider.withSession(_.findProject(id))
    } yield p

    def discoverProjectByUseCaseId: Box[Project] = for {
      id <- discoverUseCaseId
      p <- DaoProvider.withSession(_.findProjectByUc(id))
    } yield p

    discoverProjectByProjectId.or(discoverProjectByUseCaseId)
  }
}
