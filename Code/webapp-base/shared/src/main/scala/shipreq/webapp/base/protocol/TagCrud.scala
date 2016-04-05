package shipreq.webapp.base.protocol

import scalaz.\&/
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import boopickle._, BoopickleMacros._, BinCodecGeneric._, BinCodecData._
import TagInTree.Relations

object TagCrud {

  sealed trait Values

  final case class TagGroupValues(name: String,
                                  mutexChildren: MutexChildren,
                                  desc: Option[String]) extends Values

  final case class ApplicableTagValues(name: String,
                                       key: HashRefKey,
                                       desc: Option[String]) extends Values

  implicit def tagGroupValueEquality     : UnivEq[TagGroupValues]      = UnivEq.derive
  implicit def applicableTagValueEquality: UnivEq[ApplicableTagValues] = UnivEq.derive
  implicit def equalValues               : UnivEq[Values]              = UnivEq.derive

  implicit val pickleTagPovRelations    : Pickler[Relations]           = pickleCaseClass
  implicit val pickleTagGroupValues     : Pickler[TagGroupValues]      = pickleCaseClass
  implicit val pickleApplicableTagValues: Pickler[ApplicableTagValues] = pickleCaseClass
  implicit val pickleTagValues          : Pickler[Values]              = pickleADT

  object Fn extends CrudFn.CAux[TagId, Values \&/ Relations]
}