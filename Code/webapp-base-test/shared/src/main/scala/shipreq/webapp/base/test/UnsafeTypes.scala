package shipreq.webapp.base.test

import japgolly.nyaya.util.Multimap
import shipreq.base.util._
import shipreq.webapp.base.data.Field.ApplicableReqTypes
import shipreq.webapp.base.text.Grammar

trait UnsafeTypesLowPriority {
   implicit def autoSome[A, B](a: A)(implicit f: A => B): Option[B] = Some(f(a))
//  implicit def autoSome[A, B](a: A)(implicit f: A => B): Option[B] = Some(a)
}

object UnsafeTypes extends UnsafeTypesLowPriority {
  import shipreq.webapp.base.data._
  import shipreq.webapp.base.delta._

  implicit def autoMnemonic   (s: String) = ReqType.Mnemonic(s)
  implicit def autoHashRefKey (s: String) = HashRefKey(s)
  implicit def autoFieldRefKey(s: String) = FieldRefKey(s)
  implicit def autoReqCodeNode(s: String) = ReqCode.Node(s)

  implicit def autoReqCode(s: String): ReqCode.Value = {
    val v = Grammar.reqCode.nodeSeqFormat(s).map(ReqCode.Node.applyFn).toVector
    NonEmptyVector(v.head, v.tail)
  }

  implicit def autoReqCodeId        (i: Int) = ReqCodeId(i)
  implicit def autoReqTypePos       (i: Int) = ReqTypePos(i)
  implicit def autoGenericReqId     (i: Int) = GenericReqId(i)
  implicit def autoCustomFieldImpId (i: Int) = CustomField.Implication.Id(i)
  implicit def autoCustomFieldTagId (i: Int) = CustomField.Tag.Id(i)
  implicit def autoCustomFieldTxtId (i: Int) = CustomField.Text.Id(i)
  implicit def autoCustomIssueTypeId(i: Int) = CustomIssueTypeId(i)
  implicit def autoCustomReqTypeId  (i: Int) = CustomReqTypeId(i)
  implicit def autoTagGroupId       (i: Int) = TagGroupId(i)
  implicit def autoApplicableTagId  (i: Int) = ApplicableTagId(i)
  implicit def autoRev              (i: Int) = Rev(i)

  implicit def autoReqCodeIdO        (i: Int): Option[ReqCodeId]                  = Some(i)
  implicit def autoReqTypePosO       (i: Int): Option[ReqTypePos]                 = Some(i)
  implicit def autoGenericReqIdO     (i: Int): Option[GenericReqId]               = Some(i)
  implicit def autoCustomFieldImpIdO (i: Int): Option[CustomField.Implication.Id] = Some(i)
  implicit def autoCustomFieldTagIdO (i: Int): Option[CustomField.Tag.Id]         = Some(i)
  implicit def autoCustomFieldTxtIdO (i: Int): Option[CustomField.Text.Id]        = Some(i)
  implicit def autoCustomIssueTypeIdO(i: Int): Option[CustomIssueTypeId]          = Some(i)
  implicit def autoCustomReqTypeIdO  (i: Int): Option[CustomReqTypeId]            = Some(i)
  implicit def autoTagGroupIdO       (i: Int): Option[TagGroupId]                 = Some(i)
  implicit def autoApplicableTagIdO  (i: Int): Option[ApplicableTagId]            = Some(i)

  implicit def tagTreeTree(t: TagTree) = t.mapValues(_.children)

  implicit def autoTrieData(ad: ReqCode.ActiveData): ReqCode.Data =
    ReqCode.Data(ad, Set.empty, Multimap.empty)

  def onlyReqTypes(a: ReqTypeId, as: ReqTypeId*): ApplicableReqTypes = ISubset.Only(NonEmptySet(a, as: _*))
  def notReqTypes(a: ReqTypeId, as: ReqTypeId*): ApplicableReqTypes = ISubset.Not(NonEmptySet(a, as: _*))
  val allReqTypes: ApplicableReqTypes = ISubset.All()

  implicit class UnsafeIntExt(val a: Int) extends AnyVal {
    def AT = ApplicableTagId(a)
    def TG = TagGroupId(a)
    def CFText = CustomField.Text.Id(a)
    def CFTag  = CustomField.Tag.Id(a)
    def CFImp  = CustomField.Implication.Id(a)
  }

  implicit class UnsafeMustExt[A](val m: Must[A]) extends AnyVal {
    def get = m.fold(sys.error, identity)
  }
  implicit def autoMustGet[A](m: Must[A]): A = m.get

  implicit def autoNevWhole[A](as: NonEmptyVector[A]): Vector[A] = as.whole
  implicit def autoNesWhole[A](as: NonEmptySet[A]): Set[A] = as.whole

  def min2set[A: UnivEq](a: A, b: A, t: A*): Min2Set[A] =
    Min2Set(NonEmptySet(a, t.toSet + b)).fold(nes => sys.error(s"Not make a Min2Set from $nes"), a => a)

  implicit def boolToMutexChildren(b: Boolean) = MutexChildren <~ b
  implicit def boolToMandatory(b: Boolean) = Mandatory <~ b
}
