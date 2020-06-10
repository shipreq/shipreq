package shipreq.webapp.server.db

import doobie._
import doobie.postgres.implicits._
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.univeq.UnivEq
import shipreq.base.db.DoobieHelpers._
import shipreq.webapp.base.data._
import shipreq.webapp.base.user._
import shipreq.webapp.server.logic._

object WebappDoobieCodecs {

  implicit val doobieMetaResponseType: Meta[ResponseType] =
    pgEnumString[ResponseType]("response_type", _ => ???, _.dbValue)

  implicit val doobieMetaEmailAddr: Meta[EmailAddr] =
    Meta[String].timap(EmailAddr.apply)(_.value)

  implicit val doobieMetaIP: Meta[IP] =
    Meta[String].timap(IP.apply)(_.value)

  implicit val doobieMetaPasswordHash: Meta[PasswordHash] =
    Meta[String].timap(PasswordHash.apply)(_.value)

  implicit val doobieMetaPersonName: Meta[PersonName] =
    Meta[String].timap(PersonName.apply)(_.value)

  implicit val doobieMetaProjectId: Meta[ProjectId] =
    Meta[Long].timap(ProjectId.apply)(_.value)

  implicit val doobieMetaSalt: Meta[Salt] =
    Meta[String].timap(Salt.apply)(_.base64)

  implicit val doobieMetaVerificationToken: Meta[VerificationToken] =
    Meta[String].timap(VerificationToken.apply)(_.value)

  implicit val doobieMetaUserId: Meta[UserId] =
    Meta[Long].timap(UserId.apply)(_.value)

  implicit val doobieMetaUsername: Meta[Username] =
    Meta[String].timap(Username.apply)(_.value)

  implicit val doobieReadPasswordAndSalt: Read[PasswordAndSalt] =
    Read.apply2(PasswordAndSalt.apply)

  implicit val doobieWritePasswordAndSalt: Write[PasswordAndSalt] =
    Write.apply2(a => (a.passwordHash, a.salt))

  implicit val doobieReadUser: Read[User] =
    Read.apply2(User.apply)

}

/** @since DB migration v4.4 */
sealed abstract class ResponseType(final val dbValue: String, final val idx: Int)
object ResponseType {
  case object `1xx` extends ResponseType("1xx", 0)
  case object `2xx` extends ResponseType("2xx", 1)
  case object `3xx` extends ResponseType("3xx", 2)
  case object `4xx` extends ResponseType("4xx", 3)
  case object `5xx` extends ResponseType("5xx", 4)
  case object Other extends ResponseType("other", 5)

  def apply(code: Int): ResponseType =
    if (code >= 200) {
      if (code < 300)
        `2xx`
      else if (code < 400)
        `3xx`
      else if (code < 500)
        `4xx`
      else if (code < 600)
        `5xx`
      else
        Other
    } else {
      if (code >= 100)
        `1xx`
      else
        Other
    }

  implicit def univEq: UnivEq[ResponseType] = UnivEq.derive

  val values = AdtMacros.adtValues[ResponseType]

  assert(values.whole.indices.toSet == values.whole.map(_.idx).toSet)
}
