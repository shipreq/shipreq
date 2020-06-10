package shipreq.taskman.api.impl

import cats.implicits._
import doobie._
import doobie.postgres.circe.jsonb.implicits._
import io.circe.Json
import japgolly.microlibs.adt_macros.AdtMacros
import shipreq.base.util.JsonUtil
import shipreq.taskman.api._

object TaskmanDoobieCodecs {

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
    Meta[Long].timap(TaskId.apply)(_.value)

  implicit val doobieMetaTaskType: Meta[TaskType] =
    Meta[Short].timap(msgTypes._2)(msgTypes._1)

  implicit val doobieMetaPriority: Meta[Priority] =
    Meta[Short].timap(Priority.apply)(_.value)

  implicit val doobieGetTaskStatus: Get[TaskStatus] =
    Get[String].temap {
      case "unassigned"    => Right(TaskStatus.Unassigned)
      case "node_assigned" => Right(TaskStatus.NodeAssigned)
      case "working"       => Right(TaskStatus.Working)
      case "complete"      => Right(TaskStatus.Complete)
      case "aborted"       => Right(TaskStatus.Aborted)
      case x               => Left(s"Unknown TaskStatus: $x")
    }

  private type TaskColumns = (TaskType, Json)

  implicit val doobieReadTask: Read[Task] =
    Read[TaskColumns].map { t =>
      TaskJson.dataDecoder(t._1).decodeJson(t._2) match {
        case Right(msg) => msg
        case Left(e)    => throw new UnsupportedOperationException(JsonUtil.decodingFailureMsg(e))
      }
    }

  implicit val doobieWriteTask: Write[Task] =
    Write[TaskColumns].contramap(t => (t.taskType, TaskJson.encodeData(t)))
}
