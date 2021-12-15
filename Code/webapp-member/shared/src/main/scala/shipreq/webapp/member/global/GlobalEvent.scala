package shipreq.webapp.member.global

import shipreq.webapp.base.data._

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

  final case class ProjectCreate(userId   : UserId,
                                 projectId: ProjectId) extends GlobalEvent

  implicit def univEq: UnivEq[GlobalEvent] = UnivEq.derive
}