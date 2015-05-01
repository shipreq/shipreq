package shipreq.webapp.base.test

import japgolly.nyaya.util.Multimap
import scalaz.OneAnd
import shipreq.base.util.{NonEmptyVector, Must}
import shipreq.webapp.base.data.Field.ApplicableReqTypes
import shipreq.webapp.base.text.Grammar

trait UnsafeTypesLowPriority {
  // implicit def autoSome[A, B](a: A)(implicit f: A => B): Option[B] = Some(f(a))
  implicit def autoSome[A](a: A): Option[A] = Some(a)
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

  implicit def autoReqCodeId        (i: Int) = ReqCode.Id(i)
  implicit def autoReqTypePos       (i: Int) = ReqTypePos(i)
  implicit def autoGenericReqId     (i: Int) = GenericReq.Id(i)
  implicit def autoCustomFieldImpId (i: Int) = CustomField.Implication.Id(i)
  implicit def autoCustomFieldTagId (i: Int) = CustomField.Tag.Id(i)
  implicit def autoCustomFieldTxtId (i: Int) = CustomField.Text.Id(i)
  implicit def autoCustomIssueTypeId(i: Int) = CustomIssueType.Id(i)
  implicit def autoCustomReqTypeId  (i: Int) = CustomReqType.Id(i)
  implicit def autoTagGroupId       (i: Int) = TagGroup.Id(i)
  implicit def autoApplicableTagId  (i: Int) = ApplicableTag.Id(i)
  implicit def autoRev              (i: Int) = Rev(i)

  implicit def autoReqTypePosO       (i: Int): Option[ReqTypePos]                 = Some(i)
  implicit def autoGenericReqIdO     (i: Int): Option[GenericReq.Id]              = Some(i)
  implicit def autoCustomFieldImpIdO (i: Int): Option[CustomField.Implication.Id] = Some(i)
  implicit def autoCustomFieldTagIdO (i: Int): Option[CustomField.Tag.Id]         = Some(i)
  implicit def autoCustomFieldTxtIdO (i: Int): Option[CustomField.Text.Id]        = Some(i)
  implicit def autoCustomIssueTypeIdO(i: Int): Option[CustomIssueType.Id]         = Some(i)
  implicit def autoCustomReqTypeIdO  (i: Int): Option[CustomReqType.Id]           = Some(i)
  implicit def autoTagGroupIdO       (i: Int): Option[TagGroup.Id]                = Some(i)
  implicit def autoApplicableTagIdO  (i: Int): Option[ApplicableTag.Id]           = Some(i)

  implicit def tagTreeTree(t: TagTree) = t.mapValues(_.children)

  implicit def autoTrieData(ad: ReqCode.ActiveData): ReqCode.Data =
    ReqCode.Data(ad, Set.empty, Multimap.empty)

  def reqTypesSet1(a: ReqType.Id, as: ReqType.Id*): OneAnd[Set, ReqType.Id] = OneAnd(a, as.toSet)
  def onlyReqTypes(a: ReqType.Id, as: ReqType.Id*): ApplicableReqTypes = ISubset.Only(reqTypesSet1(a, as: _*))
  def notReqTypes(a: ReqType.Id, as: ReqType.Id*): ApplicableReqTypes = ISubset.Not(reqTypesSet1(a, as: _*))
  val allReqTypes: ApplicableReqTypes = ISubset.All()

  implicit class UnsafeIntExt(val a: Int) extends AnyVal {
    def AT = ApplicableTag.Id(a)
    def TG = TagGroup.Id(a)
  }

  implicit class UnsafeMustExt[A](val m: Must[A]) extends AnyVal {
    def get = m.fold(sys.error, identity)
  }
  implicit def autoMustGet[A](m: Must[A]): A = m.get
}
