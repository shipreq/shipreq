package shipreq.webapp.server.db

import doobie.imports._
import shipreq.base.db.DoobieHelpers._
import shipreq.webapp.base.data._
import shipreq.webapp.base.user._
import shipreq.webapp.server.logic._

object DbMeta {

  implicit val doobieMetaEmailAddr: Meta[EmailAddr] =
    meta1(EmailAddr.apply)(_.value)

  implicit val doobieMetaIP: Meta[IP] =
    meta1(IP.apply)(_.value)

  implicit val doobieMetaPasswordHash: Meta[PasswordHash] =
    meta1(PasswordHash.apply)(_.value)

  implicit val doobieMetaPersonName: Meta[PersonName] =
    meta1(PersonName.apply)(_.value)

  implicit val doobieMetaProjectId: Meta[ProjectId] =
    meta1(ProjectId.apply)(_.value)

  implicit val doobieMetaSalt: Meta[Salt] =
    meta1(Salt.apply)(_.base64)

  implicit val doobieMetaVerificationToken: Meta[VerificationToken] =
    meta1(VerificationToken.apply)(_.value)

  implicit val doobieMetaUserId: Meta[UserId] =
    meta1(UserId.apply)(_.value)

  implicit val doobieMetaUsername: Meta[Username] =
    meta1(Username.apply)(_.value)

  implicit val doobieCompositePasswordAndSalt: Composite[PasswordAndSalt] =
    Composite.generic

  implicit val doobieCompositeUser: Composite[User] = {
    Composite[(UserId, Username)].readOnly(r => User(r._1, r._2))
  }

}