package shipreq.webapp.server.db

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.utils.Utils
import shipreq.webapp.member.global.GlobalEvent
import shipreq.webapp.member.global.GlobalEvent._

object GlobalEventTypes {

  final val TypeUserRegister1            = 1001
  final val TypeUserRegister2            = 1002
  final val TypeUserPasswordResetRequest = 1003
  final val TypeUserPasswordReset        = 1004
  final val TypeUserGroupCreate          = 1005
  final val TypeUserGroupUpdate          = 1006

  // ===================================================================================================================

  private def allTypes = AdtMacros.valuesForAdt[GlobalEvent, Short] {
    case _: UserRegister1            => TypeUserRegister1
    case _: UserRegister2            => TypeUserRegister2
    case _: UserPasswordResetRequest => TypeUserPasswordResetRequest
    case _: UserPasswordReset        => TypeUserPasswordReset
    case _: UserGroupCreate          => TypeUserGroupCreate
    case _: UserGroupUpdate          => TypeUserGroupUpdate
  }

  private def dups = Utils.dups(allTypes.iterator).toSet

  assert(dups.isEmpty, dups.mkString("Duplicate event types: ", ",", ""))
}
