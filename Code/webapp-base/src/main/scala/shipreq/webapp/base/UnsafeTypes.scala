package shipreq.webapp.base

import scalaz.OneAnd
import shipreq.webapp.base.data.Field.ApplicableReqTypes

trait UnsafeTypesLowPriority {
  // implicit def autoSome[A, B](a: A)(implicit f: A => B): Option[B] = Some(f(a))
  implicit def autoSome[A](a: A): Option[A] = Some(a)
}

/**
 * THIS SHOULD ONLY BE USED FOR TESTING.
 */
object UnsafeTypes extends UnsafeTypesLowPriority {
  import shipreq.webapp.base.data._
  import shipreq.webapp.base.delta._

  implicit def autoMnemonic(s: String) = ReqType.Mnemonic(s)
  implicit def autoHashRefKey(s: String) = HashRefKey(s)

  implicit def autoCustomFieldId    (i: Int) = CustomField.Id(i)
  implicit def autoCustomIssueTypeId(i: Int) = CustomIssueType.Id(i)
  implicit def autoCustomReqTypeId  (i: Int) = CustomReqType.Id(i)
  implicit def autoTagId            (i: Int) = Tag.Id(i)
  implicit def autoRev              (i: Int) = Rev(i)

  implicit def autoCustomFieldIdO    (i: Int): Option[CustomField.Id]     = Some(i)
  implicit def autoCustomIssueTypeIdO(i: Int): Option[CustomIssueType.Id] = Some(i)
  implicit def autoCustomReqTypeIdO  (i: Int): Option[CustomReqType.Id]   = Some(i)
  implicit def autoTagIdO            (i: Int): Option[Tag.Id]             = Some(i)

  implicit def tagTreeTree(t: TagTree) = t.mapValues(_.children)

  def reqTypesSet1(a: ReqType.Id, as: ReqType.Id*): OneAnd[Set, ReqType.Id] = OneAnd(a, as.toSet)
  def onlyReqTypes(a: ReqType.Id, as: ReqType.Id*): ApplicableReqTypes = ISubset.Only(reqTypesSet1(a, as: _*))
  def notReqTypes(a: ReqType.Id, as: ReqType.Id*): ApplicableReqTypes = ISubset.Not(reqTypesSet1(a, as: _*))
  val allReqTypes: ApplicableReqTypes = ISubset.All()
}
