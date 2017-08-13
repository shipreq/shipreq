package shipreq.webapp.base.protocol

import boopickle._
import scalaz.\&/
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecMemberData._

object TagCrud {

  sealed trait Values

  final case class TagGroupValues(name         : String,
                                  mutexChildren: MutexChildren,
                                  desc         : Option[String]) extends Values

  final case class ApplicableTagValues(name: String,
                                       key : HashRefKey,
                                       desc: Option[String]) extends Values

  implicit def tagGroupValueEquality     : UnivEq[TagGroupValues]      = UnivEq.derive
  implicit def applicableTagValueEquality: UnivEq[ApplicableTagValues] = UnivEq.derive
  implicit def equalValues               : UnivEq[Values]              = UnivEq.derive

  implicit val pickleTagPovRelations    : Pickler[TagInTree.Relations] = pickleCaseClass
  implicit val pickleTagGroupValues     : Pickler[TagGroupValues]      = pickleCaseClass
  implicit val pickleApplicableTagValues: Pickler[ApplicableTagValues] = pickleCaseClass
  implicit val pickleTagValues          : Pickler[Values]              = pickleADT

  val Protocol = CrudProtocol[TagId, Values \&/ TagInTree.Relations]("TagCrud")
}