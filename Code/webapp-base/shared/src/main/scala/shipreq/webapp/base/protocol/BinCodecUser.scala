package shipreq.webapp.base.protocol

import boopickle._
import scalaz.\/
import shipreq.webapp.base.user._
import BinCodecGeneric._
import BoopickleMacros._

object BinCodecUser {

  implicit lazy val pickleEmailAddr        : Pickler[EmailAddr        ] = pickleCaseClass
  implicit lazy val picklePersonName       : Pickler[PersonName       ] = pickleCaseClass
  implicit lazy val picklePlainTextPassword: Pickler[PlainTextPassword] = pickleCaseClass
  implicit lazy val pickleUsername         : Pickler[Username         ] = pickleCaseClass

  implicit lazy val pickleUsernameOrEmailAddr: Pickler[Username \/ EmailAddr] = pickleXor

}
