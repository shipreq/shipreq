package shipreq.taskman.api.impl

import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}
import scalaz.{\/-, \/, -\/}
import shipreq.base.util.{BiMap, Error, ErrorOr}
import shipreq.taskman.api.Types._
import shipreq.taskman.api.{TaskDef, TaskTypes}
import TaskDef._

private[api] object Serialisation {

  type Ser = Json[TaskDef]
  type DeSer = ErrorOr[TaskDef]

  private def fieldRenamer[A: Manifest](m: BiMap[String, String]): FieldSerializer[A] =
    FieldSerializer({
      case p@(name, x) =>
        Some(m.ab.get(name).map(newName => (newName, x)).getOrElse(p))
    },{
      case f@JField(name, x) =>
        m.ba.get(name).map(newName => JField(newName, x)).getOrElse(f)
    })

  implicit val formats: Formats = (
    Serialization.formats(NoTypeHints)
      + fieldRenamer[RegistrationRequested] (BiMap("email"->"e", "url"->"u"))
      + fieldRenamer[RegistrationCompleted] (BiMap("userId"->"u"))
      + fieldRenamer[PasswordResetRequested](BiMap("email"->"e", "url"->"u"))
      + fieldRenamer[LandingPageHit]        (BiMap("email"->"e", "name"->"n", "msg"->"m", "newsletter"->"w"))
    )

  def serialise(t: TaskDef): Ser = write(t).tag

  def deserialise(taskTypeId: Int, s: Ser): DeSer = {
    TaskTypes.lookupType(taskTypeId) match {
      case Some(tt) =>
        val defClass = TaskTypes.lookupTaskDef(tt)
        ErrorOr.annotate(s"Failed to parse JSON: $s") {
          ErrorOr.catchException {
            val t: TaskDef = read(s)(implicitly[Formats], Manifest.classType(defClass))
            \/-(t)
          }
        }
      case None =>
        Error(s"Unknown task type: $taskTypeId")
    }
  }
}