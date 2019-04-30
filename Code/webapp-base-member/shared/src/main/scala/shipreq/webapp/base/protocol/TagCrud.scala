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

  type Value = Values \&/ TagInTree.Relations
  type Action = CrudAction[TagId, Value]

  implicit val picklerTagPovRelations    : Pickler[TagInTree.Relations] = pickleCaseClass
  implicit val picklerTagGroupValues     : Pickler[TagGroupValues]      = pickleCaseClass
  implicit val picklerApplicableTagValues: Pickler[ApplicableTagValues] = pickleCaseClass
  implicit val picklerTagValues          : Pickler[Values]              = pickleADT
  implicit val picklerAction             : Pickler[Action]              = CrudAction.pickler
}