package com.beardedlogic.usecase
package app

import scalaz.{Name, Need}
import net.liftweb.common.Logger
import net.liftweb.http.RequestVar
import db.{DaoS, UseCaseSummary, Project}
import DI.DaoProvider
import lib.Types._
import lib.SnippetHelpers._
import feature.Navbar

object RequestVars extends Logger {

  // -------------------------------------------------------------------------------------------------------------------
  // Manually set

  object Navbar extends RequestVar[Navbar](fail("Navbar"))

  object ProjectId extends RequestVar[Name[ProjectId]](fail("ProjectId")) {
    def deriveFromProject(): Unit = ProjectId.set(Need(Project.get.value.id))
  }

  object Project extends RequestVar[Name[Project]](fail("SoleProject")) {
    def deriveFromProjectId(): Unit = Project.set(requireDbData("Project")(_.findProject(ProjectId.get.value)))
    def deriveFromUseCaseId(): Unit = Project.set(requireDbData("Project")(_.findProjectByUc(UseCaseId.get.value)))
  }

  object UseCaseId extends RequestVar[Name[UseCaseIdentId]](fail("SoleUseCaseId"))

  // -------------------------------------------------------------------------------------------------------------------
  // Derived

  object UseCases extends RequestVar[List[UseCaseSummary]](
    DaoProvider.withSession(_.summariseUseCases(ProjectId.get.value))
  )

  // -------------------------------------------------------------------------------------------------------------------
  // Helpers

  private def fail(name: String): Nothing = {
    warn("No value available to RequestVar: " + name)
    redirectHome
  }

  private def notFound(name: String): Nothing = {
    warn(s"$name not found.")
    redirectHome
  }

  private def requireDbData[T](name: String)(f: DaoS => Option[T]): Need[T] =
    Need(DaoProvider.withSession(f) getOrElse notFound(name))
}
