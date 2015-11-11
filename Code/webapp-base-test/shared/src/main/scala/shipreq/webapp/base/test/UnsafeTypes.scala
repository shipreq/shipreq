package shipreq.webapp.base.test

import nyaya.util.Multimap
import scala.collection.generic.CanBuildFrom
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar, Text}
import Field.ApplicableReqTypes
import ScalaExt._

trait UnsafeTypesLowPriority {
//   implicit def autoSome[A, B](a: A)(implicit f: A => B): Option[B] = Some(f(a))
  implicit def autoSome[A](a: A): Option[A] = Some(a)
}

trait UnsafeTypesMedPriority extends UnsafeTypesLowPriority {

  implicit def autoMnemonic   (s: String) = ReqType.Mnemonic(s)
  implicit def autoHashRefKey (s: String) = HashRefKey(s)
  implicit def autoFieldRefKey(s: String) = FieldRefKey(s)
  implicit def autoReqCodeNode(s: String) = ReqCode.Node(s)

  implicit def autoReqCode(s: String): ReqCode.Value = {
    val v = Grammar.reqCode.nodeSeqFormat.split(s).map(ReqCode.Node.applyFn).toVector
    NonEmptyVector(v.head, v.tail)
  }

  implicit def autoReqCodeSet[C <% ReqCode.Value](c: C): ReqCode.CodeSet =
    ReqCode.CodeSet.empty.put(c, ())

  implicit def autoReqCodeSetFromSet[C <% ReqCode.Value](cs: Set[C]): ReqCode.CodeSet =
    cs.foldLeft(ReqCode.CodeSet.empty)(_.put(_, ()))

  implicit def autoReqCodeId        (i: Int) = ReqCodeId(i)
  implicit def autoReqTypePos       (i: Int) = ReqTypePos(i)
  implicit def autoGenericReqId     (i: Int) = GenericReqId(i)
//  implicit def autoUseCaseId        (i: Int) = UseCaseId(i)
  implicit def autoUseCaseStepId    (i: Int) = UseCaseStepId(i)
  implicit def autoCustomFieldImpId (i: Int) = CustomField.Implication.Id(i)
  implicit def autoCustomFieldTagId (i: Int) = CustomField.Tag.Id(i)
  implicit def autoCustomFieldTxtId (i: Int) = CustomField.Text.Id(i)
  implicit def autoCustomIssueTypeId(i: Int) = CustomIssueTypeId(i)
  implicit def autoCustomReqTypeId  (i: Int) = CustomReqTypeId(i)
  implicit def autoTagGroupId       (i: Int) = TagGroupId(i)
  implicit def autoApplicableTagId  (i: Int) = ApplicableTagId(i)
  implicit def autoDeletionReasonId (i: Int) = DeletionReasonId(i)

  implicit def autoReqCodeIdO        (i: Int): Option[ReqCodeId]                  = Some(i)
  implicit def autoReqTypePosO       (i: Int): Option[ReqTypePos]                 = Some(i)
  implicit def autoGenericReqIdO     (i: Int): Option[GenericReqId]               = Some(i)
//  implicit def autoUseCaseIdO        (i: Int): Option[UseCaseId]                  = Some(i)
  implicit def autoUseCaseStepIdO    (i: Int): Option[UseCaseStepId]              = Some(i)
  implicit def autoCustomFieldImpIdO (i: Int): Option[CustomField.Implication.Id] = Some(i)
  implicit def autoCustomFieldTagIdO (i: Int): Option[CustomField.Tag.Id]         = Some(i)
  implicit def autoCustomFieldTxtIdO (i: Int): Option[CustomField.Text.Id]        = Some(i)
  implicit def autoCustomIssueTypeIdO(i: Int): Option[CustomIssueTypeId]          = Some(i)
  implicit def autoCustomReqTypeIdO  (i: Int): Option[CustomReqTypeId]            = Some(i)
  implicit def autoTagGroupIdO       (i: Int): Option[TagGroupId]                 = Some(i)
  implicit def autoApplicableTagIdO  (i: Int): Option[ApplicableTagId]            = Some(i)
  implicit def autoDeletionReasonIdO (i: Int): Option[DeletionReasonId]           = Some(i)

  implicit def autoUseCaseStepIdPair(p: (Int, Int)): (UseCaseStepId, UseCaseStepId) = p.mapEach(UseCaseStepId)

  implicit def autoReqCodeIdS(i: Int): Set[ReqCodeId] = Set(i)

  implicit def tagTreeTree(t: TagTree) = t.mapValues(_.children)

  def onlyReqTypes(a: ReqTypeId, as: ReqTypeId*): ApplicableReqTypes = ISubset.Only(NonEmptySet(a, as: _*))
  def notReqTypes(a: ReqTypeId, as: ReqTypeId*): ApplicableReqTypes = ISubset.Not(NonEmptySet(a, as: _*))
  val allReqTypes: ApplicableReqTypes = ISubset.All()

  implicit def autoNevWhole[A](as: NonEmptyVector[A]): Vector[A] = as.whole
  implicit def autoNesWhole[A](as: NonEmptySet[A]): Set[A] = as.whole

  //  implicit def autoNes[A: UnivEq](a: A): NonEmptySet[A] = NonEmptySet one a
  implicit def autoNes[A, B: UnivEq](a: A)(implicit ev: A => B): NonEmptySet[B] =
    NonEmptySet one a

//  implicit def autoSet[A, B: UnivEq](a: A)(implicit ev: A => B): Set[B] =
//    Set(a)

  def min2set[A: UnivEq](a: A, b: A, t: A*): Min2Set[A] =
    Min2Set(NonEmptySet(a, t.toSet + b)).fold(nes => sys.error(s"Not make a Min2Set from $nes"), a => a)

  implicit def boolToMutexChildren(b: Boolean) = MutexChildren <~ b
  implicit def boolToMandatory(b: Boolean) = Mandatory <~ b
  implicit def boolToImplicationRequired(b: Boolean) = ImplicationRequired <~ b

  def ∅[A](implicit cbf: CanBuildFrom[Nothing, Nothing, A]): A = cbf().result()

  def nesd[A: UnivEq](remove: A*)(add: A*): NonEmpty[SetDiff[A]] = {
    val sd = SetDiff(removed = remove.toSet, add.toSet)
    NonEmpty(sd) getOrElse sys.error(s"nesd()() called with no data.")
  }

  private def __checkLiteral(s: String): String =
    if (s.isEmpty)
      sys.error("Invalid literal: empty string.")
    else if (s.trim != s)
      sys.error(s"Invalid literal: '$s' isn't trimmed.")
    else
      s

  implicit def autoText_CustomTextFieldA(s: String): Text.CustomTextField.Atom = Text.CustomTextField Literal __checkLiteral(s)
  implicit def autoText_CustomTextFieldN(s: String): Text.CustomTextField.NonEmptyText = NonEmptyVector one s
  implicit def autoText_CustomTextFieldO(s: String): Text.CustomTextField.OptionalText = Vector1(s)

  implicit def autoText_DeletionReasonA(s: String): Text.DeletionReason.Atom = Text.DeletionReason Literal __checkLiteral(s)
  implicit def autoText_DeletionReasonN(s: String): Text.DeletionReason.NonEmptyText = NonEmptyVector one s
  implicit def autoText_DeletionReasonO(s: String): Text.DeletionReason.OptionalText = Vector1(s)

  implicit def autoText_GenericReqTitleA(s: String): Text.GenericReqTitle.Atom = Text.GenericReqTitle Literal __checkLiteral(s)
  implicit def autoText_GenericReqTitleN(s: String): Text.GenericReqTitle.NonEmptyText = NonEmptyVector one s
  implicit def autoText_GenericReqTitleO(s: String): Text.GenericReqTitle.OptionalText = Vector1(s)

  implicit def autoText_InlineIssueDescA(s: String): Text.InlineIssueDesc.Atom = Text.InlineIssueDesc Literal __checkLiteral(s)
  implicit def autoText_InlineIssueDescN(s: String): Text.InlineIssueDesc.NonEmptyText = NonEmptyVector one s
  implicit def autoText_InlineIssueDescO(s: String): Text.InlineIssueDesc.OptionalText = Vector1(s)

  implicit def autoText_ReqCodeGroupTitleA(s: String): Text.ReqCodeGroupTitle.Atom = Text.ReqCodeGroupTitle Literal __checkLiteral(s)
  implicit def autoText_ReqCodeGroupTitleN(s: String): Text.ReqCodeGroupTitle.NonEmptyText = NonEmptyVector one s
  implicit def autoText_ReqCodeGroupTitleO(s: String): Text.ReqCodeGroupTitle.OptionalText = Vector1(s)

  implicit def autoText_UseCaseTitleA(s: String): Text.UseCaseTitle.Atom = Text.UseCaseTitle Literal __checkLiteral(s)
  implicit def autoText_UseCaseTitleN(s: String): Text.UseCaseTitle.NonEmptyText = NonEmptyVector one s
  implicit def autoText_UseCaseTitleO(s: String): Text.UseCaseTitle.OptionalText = Vector1(s)

  implicit def autoText_UseCaseStepA(s: String): Text.UseCaseStep.Atom = Text.UseCaseStep Literal __checkLiteral(s)
  implicit def autoText_UseCaseStepN(s: String): Text.UseCaseStep.NonEmptyText = NonEmptyVector one s
  implicit def autoText_UseCaseStepO(s: String): Text.UseCaseStep.OptionalText = Vector1(s)

//  /** "0.0" ⇒ NonEmptyVector(0,0) */
//  implicit def vectorTreeLocFromString(s: String): VectorTree.Location =
//    NonEmptyVector force
//      s.split('.').iterator
//        .map(s => IndexLabel.NumericFrom0.parse(s).get)
//        .toVector

  implicit def autoReqCodeIdAndValue(t: (Int, String)) = ReqCode.IdAndValue(t._1, t._2)

  implicit def setLikePatchAdd1(s: Set[(Int, String)]): Multimap[ReqCode.Value, Set, ReqCodeId] =
    setLikePatchAdd(s map autoReqCodeIdAndValue)

  implicit def setLikePatchAdd(s: Set[ReqCode.IdAndValue]): Multimap[ReqCode.Value, Set, ReqCodeId] =
    Multimap(s.toList.map(iv => iv.value -> Set(iv.id)).toMap)
}

object UnsafeTypes extends UnsafeTypesMedPriority {
  implicit class UnsafeIntExt(val a: Int) extends AnyVal {
    def AT = ApplicableTagId(a)
    def TG = TagGroupId(a)
    def CFText = CustomField.Text.Id(a)
    def CFTag  = CustomField.Tag.Id(a)
    def CFImp  = CustomField.Implication.Id(a)
    def GR = GenericReqId(a)
    def UC = UseCaseId(a)
  }
}
