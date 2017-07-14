package shipreq.webapp.base.event

import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{DataValidators => V, _}
import ApplyEventLib._, SE.SE
import DataImplicits._

trait ApplyOtherEvent {
  this: ApplyEvent =>

  object OtherEvents {
    val validateProjectName = validateA(V.projectName)

    def applyProjectNameSet(e: ProjectNameSet): SE[Unit] =
      validateProjectName(e.name) >>= (name =>
        SE.mod(Project.name.set(name)))
  }
}