package shipreq.webapp.server.db

import doobie.imports._
import java.time.Instant
import shipreq.base.db.SqlHelpers._
import shipreq.webapp.base.data._
import shipreq.webapp.base.user._
import shipreq.webapp.server.data._
import shipreq.webapp.server.logic._
import shipreq.webapp.server.security._

object SqlHelpers {

  implicit val doobieMetaEmailAddr     = doobieMetaCaseClass[EmailAddr]
  implicit val doobieMetaHashedStr     = doobieMetaCaseClass[PasswordHash]
  implicit val doobieMetaProjectId     = doobieMetaCaseClass[ProjectId]
  implicit val doobieMetaUserId        = doobieMetaCaseClass[UserId]
  implicit val doobieMetaUsername      = doobieMetaCaseClass[Username]

  implicit val doobieCompositePasswordAndSalt =
    Composite[(PasswordHash, String)].xmap[PasswordAndSalt](
      p => PasswordAndSalt(p._1, Salt.fromBase64(p._2)),
      v => (v.hashedPassword, v.salt.toBase64))

  implicit val doobieCompositeResetPasswordInfo: Composite[ResetPasswordInfo] =
    Composite.generic

  implicit val doobieCompositeUserRegistrationInfo: Composite[UserRegistrationInfo] =
    Composite.generic

  implicit val doobieCompositeUserDescriptor: Composite[User] =
    Composite[(UserId, Username, EmailAddr, Option[String])]
      .readOnly(r => User(r._1, r._2, r._3, userRoles(r._4)))

  implicit val doobieCompositeProjectMetaData: Composite[ProjectMetaData] =
    Composite[(ProjectId, String, Int, Int, Instant, Option[Instant])].readOnly {
      case(id, name, evCount, reqCount, createdAt, lastUpdatedAt) =>
        ProjectMetaData(ProjectId.Extern(id), name unNull "", evCount, reqCount, createdAt, lastUpdatedAt)
    }

  def userRoles(r: Option[String]): Set[String] =
    r match {
      case None        => Set.empty
      case Some(roles) => roles.split(',').toSet
    }

  final case class UserDescAndPasswordInDb(id            : UserId,
                                           username      : Option[Username],
                                           email         : EmailAddr,
                                           rolesStr      : Option[String],
                                           hashedPassword: Option[PasswordHash],
                                           saltBase64    : Option[String]) {
    def resolve: Option[(User, PasswordAndSalt)] =
      for {
        u <- username
        a <- hashedPassword
        b <- saltBase64
        roles = rolesStr.fold(Set.empty[String])(_.split(',').toSet)
      } yield (User(id, u, email, roles), PasswordAndSalt(a, Salt.fromBase64(b)))
  }

  implicit val doobieCompositeUserDescAndPasswordInDb: Composite[UserDescAndPasswordInDb] =
    Composite.generic

}