package shipreq.webapp.base.event

import scala.reflect.ClassTag
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data.{Validators => V, _}
import shipreq.webapp.base.util.GenericData
import ApplyEventLib._, SE.SE
import DataImplicits._

trait ApplyOtherEvent {
  this: ApplyEvent =>

  object OtherEvents {
    val validateProjectName = validateA(V.projectName, FieldNames.name)

    def applyProjectNameSet(e: ProjectNameSet): SE[Unit] =
      validateProjectName(e.name) >>= (name =>
        SE.mod(Project.name.set(name)))
  }
}