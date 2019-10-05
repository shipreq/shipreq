package shipreq.taskman.api.impl

import doobie.imports._
import io.circe.Json
import japgolly.microlibs.adt_macros.AdtMacros
import shipreq.base.db.DoobieHelpers._
import shipreq.base.db.DoobieMeta._
import shipreq.base.util.JsonUtil
import shipreq.taskman.api._

object DoobieMeta {

  private val msgTypes = AdtMacros.adtIso[MsgType, Short] {
    case MsgType.DummyMsg                 =>   1
    case MsgType.SendDiagEmail            =>   2
    case MsgType.RegistrationRequested    => 100
    case MsgType.RegistrationCompleted    => 101
    case MsgType.ReRegistrationAttempted  => 102
    case MsgType.PasswordResetRequested   => 103
    case MsgType.UserUpdated              => 104
    case MsgType.LandingPageHit           => 200
    case MsgType.SyncToMailingList        => 300
    case MsgType.WebappErrorOccurred      => 500
  }

  implicit val doobieMetaMsgId: Meta[MsgId] =
    meta1(MsgId.apply)(_.value)

  implicit val doobieMetaMsgType: Meta[MsgType] =
    Meta[Short].xmap(msgTypes._2, msgTypes._1)

  implicit val doobieMetaPriority: Meta[Priority] =
    meta1(Priority.apply)(_.value)

  implicit val doobieMetaMsgStatus: Meta[MsgStatus] =
    Meta[String].readOnly {
      case "unassigned"    => MsgStatus.Unassigned
      case "node_assigned" => MsgStatus.NodeAssigned
      case "working"       => MsgStatus.Working
      case "complete"      => MsgStatus.Complete
      case "aborted"       => MsgStatus.Aborted
    }

  implicit val doobieCompositeMsg: Composite[Msg] = {
    type T = (MsgType, Json)

    val f: T => Msg =
      t => MsgJson.dataDecoder(t._1).decodeJson(t._2) match {
             case Right(msg) => msg
             case Left(e)    => throw new UnsupportedOperationException(JsonUtil.decodingFailureMsg(e))
           }

    val g: Msg => T =
      m => (m.msgType, MsgJson.encodeData(m))

    Composite[T].xmap(f, g)
  }

}
