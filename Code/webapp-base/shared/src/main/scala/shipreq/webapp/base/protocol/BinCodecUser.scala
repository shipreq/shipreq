package shipreq.webapp.base.protocol

import boopickle._
import shipreq.webapp.base.user._
import BinCodecGeneric._
import BoopickleMacros._

object BinCodecUser {

  implicit lazy val pickleEmailAddr : Pickler[EmailAddr ] = pickleCaseClass
  implicit lazy val picklePersonName: Pickler[PersonName] = pickleCaseClass
  implicit lazy val pickleUsername  : Pickler[Username  ] = pickleCaseClass

}
