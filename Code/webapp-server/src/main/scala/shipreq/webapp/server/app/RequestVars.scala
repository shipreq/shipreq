package shipreq.webapp.server.app

import scalaz.{Name, Need}
import net.liftweb.common.Logger
import net.liftweb.http.RequestVar
import shipreq.taskman.api.UserId
import shipreq.webapp.server.data
import shipreq.webapp.server.db.DaoS
import shipreq.webapp.server.lib.SnippetHelpers._

object RequestVars extends Logger with DI {

  // -------------------------------------------------------------------------------------------------------------------
  // Manually set

  object ProjectId extends RequestVar[Name[data.ProjectId]](fail("ProjectId"))

  object ProjectOwner extends RequestVar[Name[UserId]](fail("ProjectOwner")) {
    def loadFromProjectId(): Unit =
      set(requireDbData("ProjectOwner")(_.findProjectOwner(ProjectId.get.value)))
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
