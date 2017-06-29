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
  implicit val doobieMetaHashedStr     = doobieMetaCaseClass[HashedStr]
  implicit val doobieMetaProjectId     = doobieMetaCaseClass[ProjectId]
  implicit val doobieMetaUserId        = doobieMetaCaseClass[UserId]
  implicit val doobieMetaUsername      = doobieMetaCaseClass[Username]

  implicit val doobieCompositePasswordAndSalt =
    Composite[(HashedStr, String)].xmap[PasswordAndSalt](
      (PasswordAndSalt.restore _).tupled,
      v => (v.hashedPassword, v.salt))

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
                                           hashedPassword: Option[HashedStr],
                                           saltBytes     : Option[String]) {
    def resolve: Option[(User, PasswordAndSalt)] =
      for {
        u <- username
        a <- hashedPassword
        b <- saltBytes
        roles = rolesStr.fold(Set.empty[String])(_.split(',').toSet)
      } yield (User(id, u, email, roles), PasswordAndSalt.restore(a, b))
  }

  implicit val doobieCompositeUserDescAndPasswordInDb: Composite[UserDescAndPasswordInDb] =
    Composite.generic

}