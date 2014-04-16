package shipreq.taskman.api.impl

import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}
import shipreq.base.util.{BiMap, ErrorOr}
import shipreq.taskman.api.Types._
import shipreq.taskman.api.{Msg, MsgType}
import Msg._

private[taskman] object Serialisation {

  type Ser = Json[Msg]
  type DeSer = ErrorOr[Msg]

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
      + fieldRenamer[RegistrationRequested]  (BiMap("email"->"e", "verifyEmailUrl"->"u"))
      + fieldRenamer[RegistrationCompleted]  (BiMap("userId"->"u"))
      + fieldRenamer[ReRegistrationAttempted](BiMap("email"->"e", "loginUrl"->"l"))
      + fieldRenamer[PasswordResetRequested] (BiMap("email"->"e", "resetPasswordUrl"->"u"))
      + fieldRenamer[LandingPageHit]         (BiMap("email"->"e", "name"->"n", "msg"->"m", "newsletter"->"w"))
    )

  def serialise(m: Msg): Ser = write(m).tag

  def deserialise(msgTypeId: Short, s: Ser): DeSer =
    MsgType.lookup(msgTypeId) match {
      case Some(t) => deserialise(t, s)
      case None    => ErrorOr.error(s"Unknown message type: $msgTypeId")
    }

  def deserialise(t: MsgType, s: Ser): DeSer =
    ErrorOr.annotate(s"Failed to parse JSON: $s") {
      ErrorOr.catchException {
        val m: Msg = read(s)(implicitly[Formats], Manifest.classType(t.msgClass))
        ErrorOr(m)
      }
    }
}