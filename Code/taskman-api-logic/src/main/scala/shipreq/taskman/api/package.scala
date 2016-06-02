package shipreq.taskman

import japgolly.univeq.UnivEq
import scalaz.~>
import scalaz.effect.IO

package object api {

  type ApiOpReifier = ApiOp ~> IO

  final case class MsgId(value: Long) extends AnyVal
  final case class UserId(value: Long) // extends AnyVal // TODO Mocking in ResetPasswordTest prevents AnyVal here
  final case class EmailAddr(value: String) extends AnyVal

  implicit def univEqMsgId     : UnivEq[MsgId    ] = UnivEq.derive
  implicit def univEqUserId    : UnivEq[UserId   ] = UnivEq.derive
  implicit def univEqEmailAddr : UnivEq[EmailAddr] = UnivEq.derive
}
