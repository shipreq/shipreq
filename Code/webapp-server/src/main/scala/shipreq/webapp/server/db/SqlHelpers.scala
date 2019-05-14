package shipreq.webapp.server.db

import doobie.imports._
import java.time.Instant
import shipreq.base.db.SqlHelpers._
import shipreq.webapp.base.data._
import shipreq.webapp.base.user._
import shipreq.webapp.server.logic._

object SqlHelpers {

  implicit val doobieMetaEmailAddr     = doobieMetaCaseClass[EmailAddr]
  implicit val doobieMetaIP            = doobieMetaCaseClass[IP]
  implicit val doobieMetaPasswordHash  = doobieMetaCaseClass[PasswordHash]
  implicit val doobieMetaPersonName    = doobieMetaCaseClass[PersonName]
  implicit val doobieMetaProjectId     = doobieMetaCaseClass[ProjectId]
  implicit val doobieMetaSalt          = doobieMetaCaseClass[Salt]
  implicit val doobieMetaSecurityToken = doobieMetaCaseClass[SecurityToken]
  implicit val doobieMetaUserId        = doobieMetaCaseClass[UserId]
  implicit val doobieMetaUsername      = doobieMetaCaseClass[Username]

  implicit val doobieCompositePasswordAndSalt: Composite[PasswordAndSalt] =
    Composite.generic

  implicit val doobieCompositeUser: Composite[User] =
    Composite[(UserId, Username, Option[String])]
      .readOnly(r => User(r._1, r._2, userRoles(r._3)))

  def userRoles(r: Option[String]): Set[String] =
    r match {
      case None        => Set.empty
      case Some(roles) => roles.split(',').toSet
    }

  implicit val doobieCompositeProjectMetaData: Composite[ProjectMetaData] =
    Composite[(ProjectId, String, Int, Int, Int, Instant, Option[Instant])].readOnly {
      case(id, name, initEvents, eventMaxOrd, reqCount, createdAt, lastUpdatedAt) =>
        ProjectMetaData(
          Obfuscators.projectId.obfuscate(id),
          name unNull "",
          initEvents,
          eventMaxOrd,
          reqCount,
          createdAt,
          lastUpdatedAt)
    }

}