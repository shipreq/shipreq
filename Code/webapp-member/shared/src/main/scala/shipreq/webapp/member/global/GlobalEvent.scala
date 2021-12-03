package shipreq.webapp.member.global

import shipreq.base.util.SetDiff
import shipreq.webapp.base.data._
import shipreq.webapp.member.social._

sealed trait GlobalEvent

object GlobalEvent {

  final case class UserRegister1(ip    : Option[IP],
                                 userId: UserId) extends GlobalEvent

  final case class UserRegister2(ip    : Option[IP],
                                 userId: UserId) extends GlobalEvent

  final case class UserPasswordResetRequest(ip    : Option[IP],
                                            query : String,
                                            userId: Option[UserId]) extends GlobalEvent

  final case class UserPasswordReset(ip    : Option[IP],
                                     userId: UserId) extends GlobalEvent

  final case class UserGroupCreate(userId: UserId,
                                   id    : UserGroup.Id,
                                   name  : UserGroup.Name,
                                   handle: UserGroup.Handle,
                                   rels  : UserGroup.ARels[Set, UserGroup.Id, UserId]) extends GlobalEvent

  final case class UserGroupUpdate(userId: UserId,
                                   id    : UserGroup.Id,
                                   name  : Option[UserGroup.Name],
                                   handle: Option[UserGroup.Handle],
                                   rels  : UserGroup.ARels[SetDiff, UserGroup.Id, UserId]) extends GlobalEvent

  implicit def univEq: UnivEq[GlobalEvent] = UnivEq.derive
}