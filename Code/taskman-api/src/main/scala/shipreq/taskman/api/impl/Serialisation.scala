package shipreq.taskman.api.impl

import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}
import shipreq.base.util.{BiMap, ErrorOr}
import shipreq.base.util.TaggedTypes._
import shipreq.taskman.api._
import Msg._

private[taskman] object Serialisation {

  type Ser = JsonStr[Msg]
  type DeSer = ErrorOr[Msg]

  private def fieldRenamer[A: Manifest](m: BiMap[String, String]): FieldSerializer[A] =
    FieldSerializer({
      case p@(name, x) =>
        Some(m.forward.get(name).fold(p)(newName => (newName, x)))
    },{
      case f@JField(name, x) =>
        m.backward.get(name).fold(f)(newName => JField(newName, x))
    })

  object CodecEmailAddr extends CustomSerializer[EmailAddr](_ => (
    { case JString(a) => EmailAddr(a) },
    { case a: EmailAddr => JString(a.value) }))

  object CodecUserId extends CustomSerializer[UserId](_ => (
    { case JLong(a) => UserId(a)
      case JInt(a) => UserId(a.toLong)
    },
    { case a: UserId => JLong(a.value) }))

  implicit val formats: Formats = (
    Serialization.formats(NoTypeHints)
      + fieldRenamer[RegistrationRequested]  (BiMap(Map("email"->"e", "verifyEmailUrl"->"u")))
      + fieldRenamer[RegistrationCompleted]  (BiMap(Map("userId"->"u")))
      + fieldRenamer[UserUpdated]            (BiMap(Map("userId"->"u")))
      + fieldRenamer[ReRegistrationAttempted](BiMap(Map("email"->"e", "loginUrl"->"l")))
      + fieldRenamer[PasswordResetRequested] (BiMap(Map("email"->"e", "resetPasswordUrl"->"u")))
      + fieldRenamer[LandingPageHit]         (BiMap(Map("email"->"e", "name"->"n", "msg"->"m", "newsletter"->"w")))
      + CodecEmailAddr
      + CodecUserId
    )

  def serialise(m: Msg): Ser = JsonStr(write(m))

  def deserialise(msgTypeId: Short, s: Ser): DeSer =
    MsgType.lookup(msgTypeId) match {
      case Some(t) => deserialise(t, s)
      case None    => ErrorOr.error(s"Unknown message type: $msgTypeId")
    }

  def deserialise(t: MsgType, s: Ser): DeSer =
    ErrorOr.annotate(s"Failed to parse JSON: $s") {
      ErrorOr.catchException {
        val m: Msg = read(s.value)(implicitly[Formats], Manifest.classType(t.msgClass))
        ErrorOr(m)
      }
    }
}