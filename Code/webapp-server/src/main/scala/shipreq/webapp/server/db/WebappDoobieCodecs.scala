package shipreq.webapp.server.db

import doobie._
import doobie.postgres.circe.jsonb.implicits._
import doobie.postgres.implicits._
import shipreq.base.db.BaseDoobieCodecs._
import shipreq.base.db.DoobieHelpers._
import shipreq.base.util.BinaryData
import shipreq.webapp.base.data._
import shipreq.webapp.server.logic.data._
import shipreq.webapp.server.logic.util.Obfuscators

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

  implicit val doobieReadProjectId: Read[ProjectId] =
    Read.fromGet(doobieMetaProjectId.get)

  implicit val doobieWriteProjectId: Write[ProjectId] =
    Write.fromPut(doobieMetaProjectId.put)

  implicit val doobieMetaSalt: Meta[Salt] =
    Meta[String].timap(Salt.apply)(_.base64)

  implicit val doobieMetaVerificationToken: Meta[VerificationToken] =
    Meta[String].timap(VerificationToken.apply)(_.value)

  implicit val doobieMetaUserId: Meta[UserId] =
    Meta[Long].timap(UserId.apply)(_.value)

  implicit val doobieReadUserId: Read[UserId] =
    Read.fromGet(doobieMetaUserId.get)

  implicit val doobieWriteUserId: Write[UserId] =
    Write.fromPut(doobieMetaUserId.put)

  implicit val doobieMetaUsername: Meta[Username] =
    Meta[String].timap(Username.apply)(_.value)

  implicit val doobieReadPasswordAndSalt: Read[PasswordAndSalt] =
    Read.apply2(PasswordAndSalt.apply)

  implicit val doobieWritePasswordAndSalt: Write[PasswordAndSalt] =
    Write.apply2(a => (a.passwordHash, a.salt))

  implicit val doobieReadUser: Read[User] =
    Read.apply2(User.apply)

  implicit val doobieReadGlobalEventSerialisationRowData: Read[GlobalEventSerialisation.RowData] =
    Read.apply3(GlobalEventSerialisation.RowData.apply)

  implicit val doobieWriteGlobalEventSerialisationRowData: Write[GlobalEventSerialisation.RowData] =
    Write.apply3(a => (a.data, a.ip, a.userId))

  implicit val doobieReadGlobalEventSerialisationRow: Read[GlobalEventSerialisation.Row] =
    Read.apply2(GlobalEventSerialisation.Row)

  implicit val doobieWriteGlobalEventSerialisationRow: Write[GlobalEventSerialisation.Row] =
    Write.apply2(a => (a.`type`, a.data))

  implicit val doobieMetaProjectEncryptionKey: Meta[ProjectEncryptionKey] =
    Meta[BinaryData].timap(ProjectEncryptionKey.apply)(_.value)

  implicit val doobieMetaUserEncryptionKey: Meta[UserEncryptionKey] =
    Meta[BinaryData].timap(UserEncryptionKey.apply)(_.value)

  implicit val doobieMetaProjectPerm: Meta[ProjectPerm] =
    pgEnumString[ProjectPerm]("project_perm", {
      case "admin"        => ProjectPerm.Admin
      case "collaborator" => ProjectPerm.Collaborator
    }, {
      case ProjectPerm.Admin        => "admin"
      case ProjectPerm.Collaborator => "collaborator"
    })

  implicit val doobieWriteArrayUsername: Write[Set[Username]] =
    Write[List[String]].contramap(_.iterator.map(_.value).toList)

  implicit val doobieWriteArrayUserId: Write[Set[UserId]] =
    Write[List[Long]].contramap(_.iterator.map(_.value).toList)

  implicit val doobieReadUserIdPublic: Read[UserId.Public] =
    Read[Long].map(id => Obfuscators.userId.obfuscate(UserId(id)))

}
