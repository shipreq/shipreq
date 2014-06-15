package shipreq.taskman

import scalaz.~>
import scalaz.effect.IO
import shipreq.base.util.TaggedTypes._

package object api {

  type ApiOpReifier = ApiOp ~> IO

  final case class MsgId(value: Long) extends TaggedLong
  implicit object MsgId extends TaggedTypeCtor[MsgId]

  final case class UserId(value: Long) extends TaggedLong
  implicit object UserId extends TaggedTypeCtor[UserId]

  final case class EmailAddr(value: String) extends TaggedString
  implicit object EmailAddr extends TaggedTypeCtor[EmailAddr]

}
