package shipreq.webapp.server.app

import doobie.imports.ConnectionIO
import net.liftweb.common.Logger
import net.liftweb.http.RequestVar
import scalaz.{Name, Need}
import shipreq.taskman.api.UserId
import shipreq.webapp.server.db.DbLogic
import shipreq.webapp.server.logic.{ProjectId => DataProjectId}
import shipreq.webapp.server.lib.SnippetHelpers.redirectHome

object RequestVars extends Logger with DI {

  // -------------------------------------------------------------------------------------------------------------------
  // Manually set

  object ProjectId extends RequestVar[Name[DataProjectId]](fail("ProjectId"))

  object ProjectOwner extends RequestVar[Name[UserId]](fail("ProjectOwner")) {
    def loadFromProjectId(): Unit =
      set(requireDbData("ProjectOwner")(DbLogic.project.findOwner(ProjectId.get.value)))
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Derived

  // -------------------------------------------------------------------------------------------------------------------
  // Helpers

  private def fail(name: String): Nothing = {
    warn("No value available to RequestVar: " + name)
    redirectHome()
  }

  private def notFound(name: String): Nothing = {
    warn(s"$name not found.")
    redirectHome()
  }

  private def requireDbData[T](name: String)(query: => ConnectionIO[Option[T]]): Need[T] =
    Need(db().io.trans(query).unsafePerformIO() getOrElse notFound(name))
}
