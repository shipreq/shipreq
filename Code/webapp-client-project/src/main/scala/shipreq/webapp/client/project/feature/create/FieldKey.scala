package shipreq.webapp.client.project.feature.create

import japgolly.scalajs.react.Reusability
import monocle.{Iso, Prism}
import shipreq.base.util.fp.~~>
import shipreq.base.util.{Direction, SetDiff}
import shipreq.webapp.client.project.feature.editor.{FieldKey => E}
import shipreq.webapp.client.project.util.DataReusability._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.text.Text

/**
 * ADT representing all types of fields supported by the editor.
 * Meant to be used as a key for some given content (e.g. for requirement FR-1).
 */
sealed trait FieldKey {

  /** Arguments required for every .render call */
  type Args

  type CommitValue

  type Value

  @inline final def cast2[F[_], G[_, _]](f: F[G[Nothing, Any]]) = f.asInstanceOf[F[G[Args, Value]]]

  def fold[F[_, _]](f: FieldKey.Fold[F]): F[Args, Value]

  final type AndValue[F[_, _]] = FieldKey.AndValue[this.type, F]
  @inline final def andValue[F[_, _]](a: F[Args, Value]): AndValue[F] =
    FieldKey.AndValue(this)(a)
}

object FieldKey {

  sealed trait ForCodeGroup   extends FieldKey   { def foldCG[F[_, _]](f: FoldForCodeGroup  [F]): F[Args, Value] }
  sealed trait ForGenericReq  extends ForSomeReq { def foldGR[F[_, _]](f: FoldForGenericReq [F]): F[Args, Value] }
  sealed trait ForUseCase     extends ForSomeReq { def foldUC[F[_, _]](f: FoldForUseCase    [F]): F[Args, Value] }
  sealed trait ForManualIssue extends FieldKey   { def foldMI[F[_, _]](f: FoldForManualIssue[F]): F[Args, Value] }

  /** Fields apply to one or more type of reqs */
  sealed trait ForSomeReq extends FieldKey

  /** Fields apply to all types of reqs */
  sealed trait ForAllReqs extends ForGenericReq with ForUseCase

  case object Code extends ForCodeGroup {
    override type Args = EditorArgs.ForReqCodeEditor[CommitValue]
    override type CommitValue = Value
    override type Value = ReqCode.Value
    override def fold[F[_, _]](f: Fold[F]): F[Args, Value] = f.code(this)
    override def foldCG[F[_, _]](f: FoldForCodeGroup[F]): F[Args, Value] = f.code(this)
  }

  case object CodeGroupTitle extends ForCodeGroup {
    override type Args = EditorArgs.ForTextEditor[CommitValue]
    override type CommitValue = Value
    override type Value = Text.CodeGroupTitle.OptionalText
    override def fold[F[_, _]](f: Fold[F]): F[Args, Value] = f.titleCG(this)
    override def foldCG[F[_, _]](f: FoldForCodeGroup[F]): F[Args, Value] = f.title(this)
  }

  case object Codes extends ForAllReqs {
    override type Args = EditorArgs.ForReqCodeEditor[CommitValue]
    override type CommitValue = SetDiff.NE[ReqCode.Value]
    override type Value = Set[ReqCode.Value]
    override def fold[F[_, _]](f: Fold[F]): F[Args, Value] = f.codes(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Value] = f.codes(this)
    override def foldUC[F[_, _]](f: FoldForUseCase   [F]): F[Args, Value] = f.codes(this)
  }

  final case class CustomTextField(field: CustomField.Text.Id) extends ForAllReqs {
    override type Args = EditorArgs.ForTextEditor[CommitValue]
    override type CommitValue = Value
    override type Value = Text.CustomTextField.OptionalText
    override def fold[F[_, _]](f: Fold[F]): F[Args, Value] = f.customTextField(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Value] = f.customTextField(this)
    override def foldUC[F[_, _]](f: FoldForUseCase   [F]): F[Args, Value] = f.customTextField(this)
  }

  case object GenericReqTitle extends ForGenericReq {
    override type Args = EditorArgs.ForTextEditor[CommitValue]
    override type CommitValue = Value
    override type Value = Text.GenericReqTitle.OptionalText
    override def fold[F[_, _]](f: Fold[F]): F[Args, Value] = f.titleGR(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Value] = f.title(this)
  }

  final case class Implications(scope: ImplicationScope) extends ForAllReqs {
    override type Args = EditorArgs.ForImplicationEditor
    override type CommitValue = EditorArgs.ImplicationEditorCommitValue
    override type Value = Set[ReqId]
    override def fold[F[_, _]](f: Fold[F]): F[Args, Value] = f.implications(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Value] = f.implications(this)
    override def foldUC[F[_, _]](f: FoldForUseCase   [F]): F[Args, Value] = f.implications(this)
    def dir: Direction = ImplicationScope.dir(scope)
  }

  case object AllTags extends ForAllReqs {
    override type Args = EditorArgs.ForTagEditor
    override type CommitValue = EditorArgs.TagEditorCommitValue
    override type Value = Set[ApplicableTagId]
    override def fold[F[_, _]](f: Fold[F]): F[Args, Value] = f.allTags(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Value] = f.allTags(this)
    override def foldUC[F[_, _]](f: FoldForUseCase   [F]): F[Args, Value] = f.allTags(this)
  }

  case object OtherTags extends ForAllReqs {
    override type Args = EditorArgs.ForTagEditor
    override type CommitValue = EditorArgs.TagEditorCommitValue
    override type Value = Set[ApplicableTagId]
    override def fold[F[_, _]](f: Fold[F]): F[Args, Value] = f.otherTags(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Value] = f.otherTags(this)
    override def foldUC[F[_, _]](f: FoldForUseCase   [F]): F[Args, Value] = f.otherTags(this)
  }

  final case class CustomFieldTags(field: CustomField.Tag.Id) extends ForAllReqs {
    override type Args = EditorArgs.ForTagEditor
    override type CommitValue = EditorArgs.TagEditorCommitValue
    override type Value = Set[ApplicableTagId]
    override def fold[F[_, _]](f: Fold[F]): F[Args, Value] = f.customFieldTags(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Value] = f.customFieldTags(this)
    override def foldUC[F[_, _]](f: FoldForUseCase   [F]): F[Args, Value] = f.customFieldTags(this)
  }

  case object UseCaseTitle extends ForUseCase {
    override type Args = EditorArgs.ForTextEditor[CommitValue]
    override type CommitValue = Value
    override type Value = Text.UseCaseTitle.OptionalText
    override def fold[F[_, _]](f: Fold[F]): F[Args, Value] = f.titleUC(this)
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Value] = f.title(this)
  }

  case object ManualIssue extends ForManualIssue {
    override type Args = EditorArgs.ForTextEditor[CommitValue]
    override type CommitValue = Value
    override type Value = Text.ManualIssue.NonEmptyText
    override def fold[F[_, _]](f: Fold[F]): F[Args, Value] = f.manualIssue(this)
    override def foldMI[F[_, _]](f: FoldForManualIssue[F]): F[Args, Value] = f.text(this)
  }

  @inline implicit def equalityForSomeReq: UnivEq[ForSomeReq] =
    UnivEq.derive

  @inline implicit def equality: UnivEq[FieldKey] =
    UnivEq.derive

  implicit val reusability: Reusability[FieldKey] =
    Reusability.byUnivEq

  /** This shit is required to workaround Scala failing to be check exhaustivity when pattern-matching on Aux */
  trait FoldBase[-FK <: FieldKey, F[_, _]] {
    def apply(f: FK): F[f.Args, f.Value]
    def map[G[_, _]](t: F ~~> G): FoldBase[FK, G]
  }

  case class Fold[F[_, _]](allTags        : AllTags        .type => F[AllTags        .Args, AllTags        .Value],
                           code           : Code           .type => F[Code           .Args, Code           .Value],
                           codes          : Codes          .type => F[Codes          .Args, Codes          .Value],
                           customFieldTags: CustomFieldTags      => F[CustomFieldTags#Args, CustomFieldTags#Value],
                           customTextField: CustomTextField      => F[CustomTextField#Args, CustomTextField#Value],
                           implications   : Implications         => F[Implications   #Args, Implications   #Value],
                           manualIssue    : ManualIssue    .type => F[ManualIssue    .Args, ManualIssue    .Value],
                           otherTags      : OtherTags      .type => F[OtherTags      .Args, OtherTags      .Value],
                           titleCG        : CodeGroupTitle .type => F[CodeGroupTitle .Args, CodeGroupTitle .Value],
                           titleGR        : GenericReqTitle.type => F[GenericReqTitle.Args, GenericReqTitle.Value],
                           titleUC        : UseCaseTitle   .type => F[UseCaseTitle   .Args, UseCaseTitle   .Value],
                          ) extends FoldBase[FieldKey, F] {
    override def apply(f: FieldKey): F[f.Args, f.Value] = f.fold(this)
    override def map[G[_, _]](t: F ~~> G): Fold[G] =
      Fold(
        allTags         = f => t(allTags        (f)),
        code            = f => t(code           (f)),
        codes           = f => t(codes          (f)),
        customFieldTags = f => t(customFieldTags(f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        manualIssue     = f => t(manualIssue    (f)),
        otherTags       = f => t(otherTags      (f)),
        titleCG         = f => t(titleCG        (f)),
        titleGR         = f => t(titleGR        (f)),
        titleUC         = f => t(titleUC        (f)),
      )
  }

  case class FoldForCodeGroup[F[_, _]](code : Code          .type => F[Code          .Args, Code          .Value],
                                       title: CodeGroupTitle.type => F[CodeGroupTitle.Args, CodeGroupTitle.Value],
                                      ) extends FoldBase[ForCodeGroup, F] {
    override def apply(f: ForCodeGroup): F[f.Args, f.Value] = f.foldCG(this)
    override def map[G[_, _]](t: F ~~> G): FoldForCodeGroup[G] =
      FoldForCodeGroup(
        code  = f => t(code (f)),
        title = f => t(title(f)))
  }

  case class FoldForGenericReq[F[_, _]](codes          : Codes.type           => F[Codes          .Args, Codes          .Value],
                                        customTextField: CustomTextField      => F[CustomTextField#Args, CustomTextField#Value],
                                        implications   : Implications         => F[Implications   #Args, Implications   #Value],
                                        otherTags      : OtherTags.type       => F[OtherTags      .Args, OtherTags      .Value],
                                        allTags        : AllTags.type         => F[AllTags        .Args, AllTags        .Value],
                                        customFieldTags: CustomFieldTags      => F[CustomFieldTags#Args, CustomFieldTags#Value],
                                        title          : GenericReqTitle.type => F[GenericReqTitle.Args, GenericReqTitle.Value],
                                       ) extends FoldBase[ForGenericReq, F] {
    override def apply(f: ForGenericReq): F[f.Args, f.Value] = f.foldGR(this)
    override def map[G[_, _]](t: F ~~> G): FoldForGenericReq[G] =
      FoldForGenericReq(
        codes           = f => t(codes          (f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        otherTags       = f => t(otherTags      (f)),
        allTags         = f => t(allTags        (f)),
        customFieldTags = f => t(customFieldTags(f)),
        title           = f => t(title          (f)))
  }

  case class FoldForUseCase[F[_, _]](codes          : Codes.type        => F[Codes          .Args, Codes          .Value],
                                     customTextField: CustomTextField   => F[CustomTextField#Args, CustomTextField#Value],
                                     implications   : Implications      => F[Implications   #Args, Implications   #Value],
                                     otherTags      : OtherTags.type    => F[OtherTags      .Args, OtherTags      .Value],
                                     allTags        : AllTags.type      => F[AllTags        .Args, AllTags        .Value],
                                     customFieldTags: CustomFieldTags   => F[CustomFieldTags#Args, CustomFieldTags#Value],
                                     title          : UseCaseTitle.type => F[UseCaseTitle   .Args, UseCaseTitle   .Value],
                                    ) extends FoldBase[ForUseCase, F] {
    override def apply(f: ForUseCase): F[f.Args, f.Value] = f.foldUC(this)
    override def map[G[_, _]](t: F ~~> G): FoldForUseCase[G] =
      FoldForUseCase(
        codes           = f => t(codes          (f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        otherTags       = f => t(otherTags      (f)),
        allTags         = f => t(allTags        (f)),
        customFieldTags = f => t(customFieldTags(f)),
        title           = f => t(title          (f)))
  }

  case class FoldForManualIssue[F[_, _]](text: ManualIssue.type => F[ManualIssue.Args, ManualIssue.Value]) extends FoldBase[ForManualIssue, F] {
    override def apply(f: ForManualIssue): F[f.Args, f.Value] = f.foldMI(this)
    override def map[G[_, _]](t: F ~~> G): FoldForManualIssue[G] =
      FoldForManualIssue(
        text = f => t(text(f)))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed trait AndValue[+FK <: FieldKey, F[_, _]] {
    val field: FK
    val value: F[field.Args, field.Value]

    final override def hashCode =
      field.hashCode * 31 + value.##

    final override def equals(obj: Any): Boolean =
      obj match {
        case t: AndValue[_, _] =>
          @inline def valueRefTest = value.isInstanceOf[AnyRef] && (value.asInstanceOf[AnyRef] eq t.value.asInstanceOf[AnyRef])
          (field == t.field) && (valueRefTest || (value == t.value))
        case _ =>
          false
      }

    @nowarn("cat=unused")
    final def unsafeSyncField[FKK >: FK <: FieldKey](f: FKK): f.AndValue[F] =
      this.asInstanceOf[f.AndValue[F]]

    final def withValue[G[_, _]](a: G[field.Args, field.Value]): AndValue[field.type, G] =
      AndValue(field)(a)

    final def map[G[_, _]](f: F[field.Args, field.Value] => G[field.Args, field.Value]): AndValue[field.type, G] =
      withValue(f(value))

    final def trans[G[_, _]](f: F ~~> G): AndValue[field.type, G] =
      map(f.apply)

    final def foldValue[A](fold: FoldBase[FK, λ[(a, v)  => F[a, v] => A]]): A =
      fold(field)(value)
  }

  object AndValue {
    def apply[F[_, _]](f: FieldKey)(a: F[f.Args, f.Value]): AndValue[f.type, F] =
      new AndValue[f.type, F] {
        override val field = f
        override val value = a
      }

    implicit def univEq[FK <: FieldKey, F[_, _]]: UnivEq[AndValue[FK, F]] =
      UnivEq.force // Proven via univEqValueProof below

    protected def univEqValueProof = {
      // Proof that FieldKeys themselves have UnivEq
      UnivEq[FieldKey]

      // Proof that all FieldKey Values have UnivEq
      type F[A, V] = UnivEq[V]
      import Text.Equality._
      Fold[F](
        allTags         = f => UnivEq[f.Value],
        code            = f => UnivEq[f.Value],
        codes           = f => UnivEq[f.Value],
        customFieldTags = f => UnivEq[f.Value],
        customTextField = f => UnivEq[f.Value],
        implications    = f => UnivEq[f.Value],
        manualIssue     = f => UnivEq[f.Value],
        otherTags       = f => UnivEq[f.Value],
        titleCG         = f => UnivEq[f.Value],
        titleGR         = f => UnivEq[f.Value],
        titleUC         = f => UnivEq[f.Value],
      )
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val editorFieldCG = Iso[E.ForCodeGroup, ForCodeGroup]({
    case E.Code           => Code
    case E.CodeGroupTitle => CodeGroupTitle
  })({
    case Code           => E.Code
    case CodeGroupTitle => E.CodeGroupTitle
  })

  val editorFieldGR = Prism[E.ForGenericReq, ForGenericReq]({
    case E.Codes                  => Some(Codes)
    case E.CustomTextField(field) => Some(CustomTextField(field))
    case E.GenericReqTitle        => Some(GenericReqTitle)
    case E.Implications(scope)    => Some(Implications(scope))
    case E.ReqType                => None
    case E.AllTags                => Some(AllTags)
    case E.OtherTags              => Some(OtherTags)
    case E.CustomFieldTags(field) => Some(CustomFieldTags(field))
  })({
    case Codes                  => E.Codes
    case CustomTextField(field) => E.CustomTextField(field)
    case GenericReqTitle        => E.GenericReqTitle
    case AllTags                => E.AllTags
    case OtherTags              => E.OtherTags
    case Implications(scope)    => E.Implications(scope)
    case CustomFieldTags(field) => E.CustomFieldTags(field)
  })

  val editorFieldUC = Iso[E.ForUseCase, ForUseCase]({
    case E.Codes                  => Codes
    case E.AllTags                => AllTags
    case E.OtherTags              => OtherTags
    case E.CustomTextField(field) => CustomTextField(field)
    case E.Implications(scope)    => Implications(scope)
    case E.CustomFieldTags(field) => CustomFieldTags(field)
    case E.UseCaseTitle           => UseCaseTitle
  })({
    case Codes                  => E.Codes
    case AllTags                => E.AllTags
    case OtherTags              => E.OtherTags
    case CustomTextField(field) => E.CustomTextField(field)
    case UseCaseTitle           => E.UseCaseTitle
    case Implications(scope)    => E.Implications(scope)
    case CustomFieldTags(field) => E.CustomFieldTags(field)
  })
}
