package shipreq.webapp.base.test

import japgolly.microlibs.nonempty.NonEmpty
import java.time._
import nyaya.util.Multimap
import scala.reflect.ClassTag
import shipreq.base.util.ScalaExt._
import shipreq.base.util.VectorTree.{Location, ParentLocation, PartialLocation}
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.text.{Grammar, Text}
import shipreq.webapp.base.util._

final case class MakeEmpty[+A](empty: A) extends AnyVal

object MakeEmpty {

  implicit def emptyVTPL: MakeEmpty[VectorTree.ParentLocation] =
    MakeEmpty(VectorTree.ParentLocation.Empty)

  implicit def emptyArraySeq[A: ClassTag]: MakeEmpty[ArraySeq[A]] =
    MakeEmpty(ArraySeq.empty)

  implicit def emptyOpt   [A]: MakeEmpty[Option[A]] = MakeEmpty(None)
  implicit def emptyVec   [A]: MakeEmpty[Vector[A]] = MakeEmpty(Vector.empty)
  implicit def emptyList  [A]: MakeEmpty[List  [A]] = MakeEmpty(Nil)
  implicit def emptySet   [A]: MakeEmpty[Set   [A]] = MakeEmpty(Set.empty)
  implicit def emptyMap[A, B]: MakeEmpty[Map[A, B]] = MakeEmpty(Map.empty)

  implicit def emptyApplicableTagGD  : MakeEmpty[ApplicableTagGD  .Values] = MakeEmpty(ApplicableTagGD  .emptyValues)
  implicit def emptyCodeGroupGD      : MakeEmpty[CodeGroupGD      .Values] = MakeEmpty(CodeGroupGD      .emptyValues)
  implicit def emptyCustomImpFieldGD : MakeEmpty[CustomImpFieldGD .Values] = MakeEmpty(CustomImpFieldGD .emptyValues)
  implicit def emptyCustomIssueTypeGD: MakeEmpty[CustomIssueTypeGD.Values] = MakeEmpty(CustomIssueTypeGD.emptyValues)
  implicit def emptyCustomReqTypeGD  : MakeEmpty[CustomReqTypeGD  .Values] = MakeEmpty(CustomReqTypeGD  .emptyValues)
  implicit def emptyCustomTagFieldGD : MakeEmpty[CustomTagFieldGD .Values] = MakeEmpty(CustomTagFieldGD .emptyValues)
  implicit def emptyCustomTextFieldGD: MakeEmpty[CustomTextFieldGD.Values] = MakeEmpty(CustomTextFieldGD.emptyValues)
  implicit def emptyGenericReqGD     : MakeEmpty[GenericReqGD     .Values] = MakeEmpty(GenericReqGD     .emptyValues)
  implicit def emptySavedViewGD      : MakeEmpty[SavedViewGD      .Values] = MakeEmpty(SavedViewGD      .emptyValues)
  implicit def emptyTagGroupGD       : MakeEmpty[TagGroupGD       .Values] = MakeEmpty(TagGroupGD       .emptyValues)
  implicit def emptyUseCaseGD        : MakeEmpty[UseCaseGD        .Values] = MakeEmpty(UseCaseGD        .emptyValues)
  implicit def emptyUseCaseStepGD    : MakeEmpty[UseCaseStepGD    .Values] = MakeEmpty(UseCaseStepGD    .emptyValues)
}

trait UnsafeTypesLowPriority {
//   implicit def autoOption[A, B](a: A)(implicit f: A => B): Option[B] = Option(f(a))
  implicit def autoOption[A](a: A): Option[A] = Option(a)
}

trait UnsafeTypesMedPriority extends UnsafeTypesLowPriority {

  implicit def autoTagsFromTree(t: TagTree) = Tags(t)

  implicit def autoMnemonic   (s: String) = ReqType.Mnemonic(s)
  implicit def autoHashRefKey (s: String) = HashRefKey(s)
  implicit def autoReqCodeNode(s: String) = ReqCode.Node(s)
  implicit def autoColour     (s: String) = Colour(s).get

  implicit def autoReqCode(s: String): ReqCode.Value = {
    val v = Grammar.reqCode.nodeSeqFormat.split(s).map(ReqCode.Node.applyFn).toVector
    NonEmptyVector(v.head, v.tail)
  }

  implicit def autoReqCodeSet[C](c: C)(implicit f: C => ReqCode.Value): ReqCode.CodeSet =
    ReqCode.CodeSet.empty.put(c, ())

  implicit def autoReqCodeSetFromSet[C](cs: Set[C])(implicit f: C => ReqCode.Value): ReqCode.CodeSet =
    cs.foldLeft(ReqCode.CodeSet.empty)(_.put(_, ()))

  implicit def autoEventOrd(i: Int) = EventOrd(i)

  implicit def autoEventOrdLatest(i: Int) = EventOrd.Latest(i)
  implicit def autoSomeEventOrdLatest(i: Int) = if (i == 0) None else Some(EventOrd.Latest(i))

  implicit def autoReqCodeGroupId   (i: Int) = ReqCodeGroupId(i)
  implicit def autoApReqCodeId      (i: Int) = ApReqCodeId(i)
//  implicit def autoReqCodeId        (i: Int) = ReqCodeId(i)
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
  implicit def autoManualIssueId    (i: Int) = ManualIssueId(i)

  implicit def autoReqCodeGroupIdO   (i: Int): Option[ReqCodeGroupId]             = Some(i)
  implicit def autoApReqCodeIdO      (i: Int): Option[ApReqCodeId]                = Some(i)
//  implicit def autoReqCodeIdO        (i: Int): Option[ReqCodeId]                  = Some(i)
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
  implicit def autoApplicableTagIdO  (i: Int): Option[ApplicableTagId]            = i.some
  implicit def autoDeletionReasonIdO (i: Int): Option[DeletionReasonId]           = Some(i)

  implicit def autoObfuscated[A](s: String): Obfuscated[A] = Obfuscated(s)

  implicit def autoUseCaseStepIdPair(p: (Int, Int)): (UseCaseStepId, UseCaseStepId) = p.mapEach(UseCaseStepId)

  implicit def autoReqCodeGroupIdS(i: Int): Set[ReqCodeGroupId] = Set(i)

  implicit def tagTreeTree(t: TagTree) = t.mapValues(_.children)

  def onlyReqTypes(a: ReqTypeId, as: ReqTypeId*): ApplicableReqTypes = ApplicableReqTypes.whitelist((a +: as): _*)
  def notReqTypes(a: ReqTypeId, as: ReqTypeId*): ApplicableReqTypes = ApplicableReqTypes.blacklist((a +: as): _*)
  def allReqTypes = ApplicableReqTypes.empty

  implicit def autoNevWhole[A](as: NonEmptyVector[A]): Vector[A] = as.whole
  implicit def autoNesWhole[A](as: NonEmptySet[A]): Set[A] = as.whole

//  implicit def autoSet[A, B: UnivEq](a: A)(implicit ev: A => B): Set[B] =
//    Set(a)

  implicit def boolToExclusivity(b: Boolean) = Exclusive when b
  implicit def boolToMandatory(b: Boolean) = Mandatory when b

  def ∅[A](implicit e: MakeEmpty[A]): A = e.empty

  def nesd[A: UnivEq](remove: A*)(add: A*): SetDiff.NE[A] = {
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
  implicit def autoText_CustomTextFieldN(s: String): Text.CustomTextField.NonEmptyText = NonEmptyArraySeq one s
  implicit def autoText_CustomTextFieldO(s: String): Text.CustomTextField.OptionalText = ArraySeq(s)

  implicit def autoText_DeletionReasonA(s: String): Text.DeletionReason.Atom = Text.DeletionReason Literal __checkLiteral(s)
  implicit def autoText_DeletionReasonN(s: String): Text.DeletionReason.NonEmptyText = NonEmptyArraySeq one s
  implicit def autoText_DeletionReasonO(s: String): Text.DeletionReason.OptionalText = ArraySeq(s)

  implicit def autoText_ManualIssueA(s: String): Text.ManualIssue.Atom = Text.ManualIssue Literal __checkLiteral(s)
  implicit def autoText_ManualIssueN(s: String): Text.ManualIssue.NonEmptyText = NonEmptyArraySeq one s
  implicit def autoText_ManualIssueO(s: String): Text.ManualIssue.OptionalText = ArraySeq(s)

  implicit def autoText_GenericReqTitleA(s: String): Text.GenericReqTitle.Atom = Text.GenericReqTitle Literal __checkLiteral(s)
  implicit def autoText_GenericReqTitleN(s: String): Text.GenericReqTitle.NonEmptyText = NonEmptyArraySeq one s
  implicit def autoText_GenericReqTitleO(s: String): Text.GenericReqTitle.OptionalText = ArraySeq(s)

  implicit def autoText_InlineIssueDescA(s: String): Text.InlineIssueDesc.Atom = Text.InlineIssueDesc Literal __checkLiteral(s)
  implicit def autoText_InlineIssueDescN(s: String): Text.InlineIssueDesc.NonEmptyText = NonEmptyArraySeq one s
  implicit def autoText_InlineIssueDescO(s: String): Text.InlineIssueDesc.OptionalText = ArraySeq(s)

  implicit def autoText_CodeGroupTitleA(s: String): Text.CodeGroupTitle.Atom = Text.CodeGroupTitle Literal __checkLiteral(s)
  implicit def autoText_CodeGroupTitleN(s: String): Text.CodeGroupTitle.NonEmptyText = NonEmptyArraySeq one s
  implicit def autoText_CodeGroupTitleO(s: String): Text.CodeGroupTitle.OptionalText = ArraySeq(s)

  implicit def autoText_UseCaseTitleA(s: String): Text.UseCaseTitle.Atom = Text.UseCaseTitle Literal __checkLiteral(s)
  implicit def autoText_UseCaseTitleN(s: String): Text.UseCaseTitle.NonEmptyText = NonEmptyArraySeq one s
  implicit def autoText_UseCaseTitleO(s: String): Text.UseCaseTitle.OptionalText = ArraySeq(s)

  implicit def autoText_UseCaseStepA(s: String): Text.UseCaseStep.Atom = Text.UseCaseStep Literal __checkLiteral(s)
  implicit def autoText_UseCaseStepN(s: String): Text.UseCaseStep.NonEmptyText = NonEmptyArraySeq one s
  implicit def autoText_UseCaseStepO(s: String): Text.UseCaseStep.OptionalText = ArraySeq(s)

  private val extpubFmt = "^([a-zA-Z]+)-?(\\d+)$".r

  implicit def autoExtPubid(s: String): ExternalPubid =
    s match {
      case extpubFmt(a, b) => ExternalPubid(a, b.toInt)
      case w => sys error s"This isn't an ExternalPubid: $w"
    }

//  /** "0.0" ⇒ NonEmptyVector(0,0) */
//  implicit def vectorTreeLocFromString(s: String): VectorTree.Location =
//    NonEmptyVector force
//      s.split('.').iterator
//        .map(s => IndexLabel.NumericFrom0.parse(s).get)
//        .toVector

  implicit def autoReqCodeIdAndValue(t: (Int, String)) =
    ApReqCodeId.AndValue(t._1, t._2)

  implicit def setLikePatchAdd1(s: Set[(Int, String)]): Multimap[ReqCode.Value, Set, ApReqCodeId] =
    setLikePatchAdd(s map autoReqCodeIdAndValue)

  implicit def setLikePatchAdd(s: Set[ApReqCodeId.AndValue]): Multimap[ReqCode.Value, Set, ApReqCodeId] =
    Multimap(s.toList.map(iv => iv.value -> Set(iv.id)).toMap)

  implicit def autoVerifiedEventNonEmptySeqFromOne(v: VerifiedEvent): VerifiedEvent.NonEmptySeq =
    VerifiedEvent.NonEmptySeq.one(v)

  implicit def derivativeTagsTagPair[A, B](p: (A, B))(implicit a: A => ApplicableTagId, b: B => ApplicableTagId): DerivativeTags.TagPair =
    DerivativeTags.TagPair(p._1, p._2)

  implicit def derivativeTagsRule[A, B](p: (A, B))(implicit a: A => DerivativeTags.TagPair, b: B => ApplicableTagId): (DerivativeTags.TagPair, ApplicableTagId) =
    (p._1, p._2)
}

object UnsafeTypes extends UnsafeTypesMedPriority {

  implicit class UnsafeIntExt(val a: Int) extends AnyVal {
    def ARC    = ApReqCodeId(a)
    def RCG    = ReqCodeGroupId(a)
    def AT     = ApplicableTagId(a)
    def TG     = TagGroupId(a)
    def CFText = CustomField.Text.Id(a)
    def CFTag  = CustomField.Tag.Id(a)
    def CFImp  = CustomField.Implication.Id(a)
    def GR     = GenericReqId(a)
    def UC     = UseCaseId(a)
  }

  implicit class UnsafeStringExt(private val str: String) extends AnyVal {
    private def ivec: Vector[Int] =
      str.split('.').iterator.map(_.toInt).toVector

    def ploc: ParentLocation =
      ParentLocation fromVector ivec

    def loc: Location =
      NonEmptyVector force ivec

    def xloc: PartialLocation =
      PartialLocation.detect(
        NonEmptyVector force
          str.split('.').iterator.map(s => if (s == "X") -1 else s.toInt).toVector)
  }

  implicit class UnsafeEventExt(private val self: Event) extends AnyVal {
    def verified(ord: EventOrd, at: Instant = Instant.now()): VerifiedEvent =
      VerifiedEvent(ord, self, at)
  }

  object AutoNES {
    //  implicit def autoNes[A: UnivEq](a: A): NonEmptySet[A] = NonEmptySet one a
    implicit def autoNes[A, B: UnivEq](a: A)(implicit ev: A => B): NonEmptySet[B] =
      NonEmptySet one a
  }
}
