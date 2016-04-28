package shipreq.webapp.server.app

import scalaz.{Name, Need}
import net.liftweb.common.Logger
import net.liftweb.http.RequestVar
import shipreq.webapp.server.data._
import shipreq.webapp.server.db.DaoS
import shipreq.webapp.server.lib.SnippetHelpers._
import shipreq.webapp.server.feature.Navbar

object RequestVars extends Logger with DI {

  // -------------------------------------------------------------------------------------------------------------------
  // Manually set

  object Navbar extends RequestVar[Navbar](fail("Navbar"))

  object ProjectId extends RequestVar[Name[ProjectId]](fail("ProjectId")) {
    def deriveFromProject(): Unit = ProjectId.set(Need(Project.get.value.id))
  }

  object Project extends RequestVar[Name[Project]](fail("SoleProject")) {
    def deriveFromProjectId(): Unit = Project.set(requireDbData("Project")(_.findProject(ProjectId.get.value)))
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Derived

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
}
