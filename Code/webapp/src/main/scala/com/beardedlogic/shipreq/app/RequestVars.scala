package shipreq.webapp
package app

import scalaz.{Name, Need}
import net.liftweb.common.Logger
import net.liftweb.http.RequestVar
import db.{Share, DaoS, UseCaseSummary, Project}
import lib.Types._
import lib.SnippetHelpers._
import feature.Navbar

object RequestVars extends Logger with DI {

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

  object ShareId extends RequestVar[Name[ShareId]](fail("ShareId")) {
    def deriveFromShare(): Unit = ShareId.set(Need(Share.get.value.id))
  }

  object Share extends RequestVar[Name[Share]](fail("SoleShare")) {
    def deriveFromShareId(): Unit = Share.set(requireDbData("Share")(_.findShare(ShareId.get.value)))
  }

  object UseCaseId extends RequestVar[Name[UseCaseIdentId]](fail("SoleUseCaseId"))

  // -------------------------------------------------------------------------------------------------------------------
  // Derived

  object UseCases extends RequestVar[List[UseCaseSummary]](
    daoProvider.withSession(_.summariseUseCases(ProjectId.get.value))
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

  private def requireDbData[T](name: String)(f: => DaoS => Option[T]): Need[T] =
    Need(daoProvider.withSession(f) getOrElse notFound(name))

  def deriveShareAndProjectFromShareUrlToken(token: Name[ShareUrlToken]): Unit = {
    val both = requireDbData("Share & Project")(_ findShareAndProject token.value)
    Share set Need(both.value._1)
    Project set Need(both.value._2)
    ProjectId.deriveFromProject()
    ShareId.deriveFromShare()
  }
}
