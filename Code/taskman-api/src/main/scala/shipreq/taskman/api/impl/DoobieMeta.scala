package shipreq.taskman.api.impl

import doobie.imports._
import io.circe.Json
import japgolly.microlibs.adt_macros.AdtMacros
import shipreq.base.db.DoobieHelpers._
import shipreq.base.db.DoobieMeta._
import shipreq.base.util.JsonUtil
import shipreq.taskman.api._

object DoobieMeta {

  private val msgTypes = AdtMacros.adtIso[TaskType, Short] {
    case TaskType.DummyTask                =>   1
    case TaskType.SendDiagEmail            =>   2
    case TaskType.RegistrationRequested    => 100
    case TaskType.RegistrationCompleted    => 101
    case TaskType.ReRegistrationAttempted  => 102
    case TaskType.PasswordResetRequested   => 103
    case TaskType.UserUpdated              => 104
    case TaskType.LandingPageHit           => 200
    case TaskType.UserFeedbackReceived     => 201
    case TaskType.SyncToMailingList        => 300
    case TaskType.ReportServerError        => 500
    case TaskType.ReportClientError        => 501
  }

  implicit val doobieMetaTaskId: Meta[TaskId] =
    meta1(TaskId.apply)(_.value)

  implicit val doobieMetaTaskType: Meta[TaskType] =
    Meta[Short].xmap(msgTypes._2, msgTypes._1)

  implicit val doobieMetaPriority: Meta[Priority] =
    meta1(Priority.apply)(_.value)

  implicit val doobieMetaTaskStatus: Meta[TaskStatus] =
    Meta[String].readOnly {
      case "unassigned"    => TaskStatus.Unassigned
      case "node_assigned" => TaskStatus.NodeAssigned
      case "working"       => TaskStatus.Working
      case "complete"      => TaskStatus.Complete
      case "aborted"       => TaskStatus.Aborted
    }

  implicit val doobieCompositeTask: Composite[Task] = {
    type T = (TaskType, Json)

    val f: T => Task =
      t => TaskJson.dataDecoder(t._1).decodeJson(t._2) match {
             case Right(msg) => msg
             case Left(e)    => throw new UnsupportedOperationException(JsonUtil.decodingFailureMsg(e))
           }

    val g: Task => T =
      m => (m.taskType, TaskJson.encodeData(m))

    Composite[T].xmap(f, g)
  }

}
