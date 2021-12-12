package shipreq.webapp.server.db

import doobie._
import doobie.postgres.circe.jsonb.implicits._
import doobie.postgres.implicits._
import shipreq.base.db.BaseDoobieCodecs._
import shipreq.base.db.DoobieHelpers._
import shipreq.base.util.BinaryData
import shipreq.webapp.base.data._
import shipreq.webapp.member.social._
import shipreq.webapp.server.logic.data._

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

  implicit val doobieMetaUserGroupId: Meta[UserGroup.Id] =
    Meta[Long].timap(UserGroup.Id.apply)(_.value)

  implicit val doobieMetaUserGroupName: Meta[UserGroup.Name] =
    Meta[String].timap(UserGroup.Name.apply)(_.value)

  implicit val doobieMetaUserGroupHandle: Meta[UserGroup.Handle] =
    Meta[String].timap(UserGroup.Handle.apply)(_.value)

  implicit val doobieMetaUserGroupPerm: Meta[UserGroup.Perm] =
    pgEnumString[UserGroup.Perm]("usr_group_perm", {
      case "admin"  => UserGroup.Perm.Admin
      case "member" => UserGroup.Perm.Member
    }, {
      case UserGroup.Perm.Admin  => "admin"
      case UserGroup.Perm.Member => "member"
    })

  private def doobieReadUserGroupRel[A: Read, B: Read]: Read[UserGroup.Rel[A, B]] =
    Read.apply3(UserGroup.Rel.apply[A, B])

  private def doobieWriteUserGroupRel[A: Write, B: Write]: Write[UserGroup.Rel[A, B]] =
    Write.apply3(a => (a.from, a.to, a.perm))

  implicit val doobieReadUserGroupUserRelUserId: Read[UserGroup.Rel[UserGroup.Id, UserId]] = doobieReadUserGroupRel
  implicit val doobieWriteUserGroupUserRelUserId: Write[UserGroup.Rel[UserGroup.Id, UserId]] = doobieWriteUserGroupRel

  implicit val doobieReadUserGroupUserRelUserGroupId: Read[UserGroup.Rel[UserGroup.Id, UserGroup.Id]] = doobieReadUserGroupRel
  implicit val doobieWriteUserGroupUserRelUserGroupId: Write[UserGroup.Rel[UserGroup.Id, UserGroup.Id]] = doobieWriteUserGroupRel

  implicit val doobieWriteArrayUserGroupId: Write[List[UserGroup.Id]] =
    Write[List[Long]].contramap(_.map(_.value))

  implicit val doobieWriteArrayUsersId: Write[List[UserId]] =
    Write[List[Long]].contramap(_.map(_.value))

  implicit val doobieWriteArrayUsername: Write[Set[Username]] =
    Write[List[String]].contramap(_.iterator.map(_.value).toList)

}
