package shipreq.webapp.client.project.feature.editor

import japgolly.scalajs.react.Reusability
import scala.reflect.ClassTag
import scalaz.~~>
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.UseCaseStepGD
import shipreq.webapp.member.text.Text
import shipreq.webapp.client.project.feature.RenderFeature
import shipreq.webapp.client.project.lib.DataReusability._

/**
 * ADT representing all types of fields supported by the editor.
 * Meant to be used as a key for some given content (e.g. for requirement FR-1).
 */
sealed trait FieldKey { self =>

  /** Arguments required for every .render call */
  type Args <: AnyRef

  final type CommitValue = Change

  /** Description of changes the user has made in the editor */
  type Change

  type RenderFieldKey <: RenderFeature.FieldKey

  def forRender: RenderFieldKey

  @inline final def cast2[F[_], G[_, _], A, B](f: F[G[A, B]]) = f.asInstanceOf[F[G[Args, Change]]]

  def fold[F[_, _]](f: FieldKey.FoldAll[F]): F[Args, Change]

  final type AndArgs = FieldKey.AndArgs { val key: self.type }

  final def andArgs(a: Args): AndArgs =
    new FieldKey.AndArgs {
      override val key: self.type = self
      override val args = a
    }
}

object FieldKey {

  /** Fields apply to one or more type of reqs */
  sealed trait ForSomeReq extends FieldKey {
    override type RenderFieldKey <: RenderFeature.FieldKey.ForSomeReq
  }

  /** Fields apply to all types of reqs */
  sealed trait ForAllReqs extends ForGenericReq with ForUseCase {
    override type RenderFieldKey <: RenderFeature.FieldKey.ForAllReqs
  }

  sealed trait ForCodeGroup extends FieldKey {
    override type RenderFieldKey <: RenderFeature.FieldKey.ForCodeGroup
    def foldCG[F[_, _]](f: FoldForCodeGroup[F]): F[Args, Change]
  }

  sealed trait ForGenericReq extends ForSomeReq {
    override type RenderFieldKey <: RenderFeature.FieldKey.ForGenericReq
    def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change]
  }

  sealed trait ForUseCase extends ForSomeReq {
    override type RenderFieldKey <: RenderFeature.FieldKey.ForUseCase
    def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change]
  }

  case object Code extends ForCodeGroup {
    override type Args = EditorArgs.ForReqCodeEditor
    override type Change = ReqCode.Value
    override type RenderFieldKey = RenderFeature.FieldKey.Code.type
    override def forRender = RenderFeature.FieldKey.Code
    override def fold[F[_, _]](f: FoldAll[F]): F[Args, Change] = f.code(this)
    override def foldCG[F[_, _]](f: FoldForCodeGroup[F]): F[Args, Change] = f.code(this)
  }

  case object CodeGroupTitle extends ForCodeGroup {
    override type Args = EditorArgs.ForTextEditor
    override type Change = Text.CodeGroupTitle.OptionalText
    override type RenderFieldKey = RenderFeature.FieldKey.CodeGroupTitle.type
    override def forRender = RenderFeature.FieldKey.CodeGroupTitle
    override def fold[F[_, _]](f: FoldAll[F]): F[Args, Change] = f.titleCG(this)
    override def foldCG[F[_, _]](f: FoldForCodeGroup[F]): F[Args, Change] = f.title(this)
  }

  case object Codes extends ForAllReqs {
    override type Args = EditorArgs.ForReqCodeEditor
    override type Change = SetDiff.NE[ReqCode.Value]
    override type RenderFieldKey = RenderFeature.FieldKey.Codes.type
    override def forRender = RenderFeature.FieldKey.Codes
    override def fold[F[_, _]](f: FoldAll[F]): F[Args, Change] = f.codes(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.codes(this)
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change] = f.codes(this)
  }

  final case class CustomTextField(field: CustomField.Text.Id) extends ForAllReqs {
    override type Args = EditorArgs.ForTextEditor
    override type Change = Text.CustomTextField.OptionalText
    override type RenderFieldKey = RenderFeature.FieldKey.CustomTextField
    override def forRender = RenderFeature.FieldKey.CustomTextField(field)
    override def fold[F[_, _]](f: FoldAll[F]): F[Args, Change] = f.customTextField(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.customTextField(this)
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change] = f.customTextField(this)
  }

  case object GenericReqTitle extends ForGenericReq {
    override type Args = EditorArgs.ForTextEditor
    override type Change = Text.GenericReqTitle.OptionalText
    override type RenderFieldKey = RenderFeature.FieldKey.Title.type
    override def forRender = RenderFeature.FieldKey.Title
    override def fold[F[_, _]](f: FoldAll[F]): F[Args, Change] = f.titleGR(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.title(this)
  }

  final case class Implications(scope: ImplicationScope) extends ForAllReqs {
    override type Args = EditorArgs.ForImplicationEditor
    override type Change = SetDiff.NE[ReqId]
    override type RenderFieldKey = RenderFeature.FieldKey.Implications
    override def forRender = RenderFeature.FieldKey.Implications(scope)
    override def fold[F[_, _]](f: FoldAll[F]): F[Args, Change] = f.implications(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.implications(this)
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change] = f.implications(this)
  }

  object Implications {
    val byDir: Direction => Implications =
      Direction.memo(dir => apply(\/-(dir)))
  }

  case object ReqType extends ForGenericReq {
    override type Args = EditorArgs.ForReqTypeEditor
    override type Change = CustomReqType
    override type RenderFieldKey = RenderFeature.FieldKey.ReqType.type
    override def forRender = RenderFeature.FieldKey.ReqType
    override def fold[F[_, _]](f: FoldAll[F]): F[Args, Change] = f.reqType(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.reqType(this)
  }

  case object AllTags extends ForAllReqs {
    override type Args = EditorArgs.ForTagEditor
    override type Change = SetDiff.NE[ApplicableTagId]
    override type RenderFieldKey = RenderFeature.FieldKey.AllTags.type
    override def forRender = RenderFeature.FieldKey.AllTags
    override def fold[F[_, _]](f: FoldAll[F]): F[Args, Change] = f.allTags(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.allTags(this)
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change] = f.allTags(this)
  }

  case object OtherTags extends ForAllReqs {
    override type Args = EditorArgs.ForTagEditor
    override type Change = SetDiff.NE[ApplicableTagId]
    override type RenderFieldKey = RenderFeature.FieldKey.OtherTags.type
    override def forRender = RenderFeature.FieldKey.OtherTags
    override def fold[F[_, _]](f: FoldAll[F]): F[Args, Change] = f.otherTags(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.otherTags(this)
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change] = f.otherTags(this)
  }

  final case class CustomFieldTags(field: CustomField.Tag.Id) extends ForAllReqs {
    override type Args = EditorArgs.ForTagEditor
    override type Change = SetDiff.NE[ApplicableTagId]
    override type RenderFieldKey = RenderFeature.FieldKey.CustomFieldTags
    override def forRender = RenderFeature.FieldKey.CustomFieldTags(field)
    override def fold[F[_, _]](f: FoldAll[F]): F[Args, Change] = f.customFieldTags(this)
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.customFieldTags(this)
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change] = f.customFieldTags(this)
  }

  final case class UseCaseStep(id: UseCaseStepId) extends FieldKey {
    override type Args = EditorArgs.ForUseCaseStepEditor
    override type Change = UseCaseStepGD.NonEmptyValues
    override type RenderFieldKey = RenderFeature.FieldKey.UseCaseStep
    override def forRender = RenderFeature.FieldKey.UseCaseStep(id)
    override def fold[F[_, _]](f: FoldAll[F]): F[Args, Change] = f.useCaseStep(this)
    def foldUCS[F[_, _]](f: FoldForUseCaseSteps[F]): F[Args, Change] = f.step(this)
  }

  case object UseCaseTitle extends ForUseCase {
    override type Args = EditorArgs.ForTextEditor
    override type Change = Text.UseCaseTitle.OptionalText
    override type RenderFieldKey = RenderFeature.FieldKey.Title.type
    override def forRender = RenderFeature.FieldKey.Title
    override def fold[F[_, _]](f: FoldAll[F]): F[Args, Change] = f.titleUC(this)
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change] = f.title(this)
  }

  final case class ManualIssue(id: ManualIssueId) extends FieldKey {
    override type Args = EditorArgs.ForTextEditor
    override type Change = Text.ManualIssue.NonEmptyText
    override type RenderFieldKey = RenderFeature.FieldKey.ManualIssue
    override def forRender = RenderFeature.FieldKey.ManualIssue(id)
    override def fold[F[_, _]](f: FoldAll[F]): F[Args, Change] = f.manualIssue(this)
    def foldMI[F[_, _]](f: FoldForManualIssues[F]): F[Args, Change] = f.text(this)
  }

  @inline implicit def equalityForSomeReq: UnivEq[ForSomeReq] =
    UnivEq.derive

  @inline implicit def equality: UnivEq[FieldKey] =
    UnivEq.derive

  implicit val reusability: Reusability[FieldKey] =
    Reusability.byUnivEq

  def customField(id: CustomFieldId): FieldKey =
    id match {
      case i: CustomField.Text.Id        => CustomTextField(i)
      case i: CustomField.Tag.Id         => CustomFieldTags(i)
      case i: CustomField.Implication.Id => Implications(-\/(i))
    }

  def impliedBy = Implications(\/-(Backwards))

//  def reqTextLoc(reqId: ReqId, loc: LocationOf.Text.InReq): FieldKey =
//    loc match {
//      case Location.Text.Title                    => reqTitle(reqId)
//      case Location.Text.CustomTextField(fieldId) => CustomTextField(fieldId)
//      case Location.Text.UseCaseStep(stepId)      => UseCaseStep(stepId)
//    }

  def reqTitle(id: ReqId): ForSomeReq { type Args = EditorArgs.ForTextEditor } =
    id match {
      case _: GenericReqId => GenericReqTitle
      case _: UseCaseId    => UseCaseTitle
    }

  type Aux[A, C] = FieldKey { type Args = A; type Change = C }
  type Nullary = FieldKey { type Args = Unit }

  // ===================================================================================================================

  /** This shit is required to workaround Scala failing to be check exhaustivity when pattern-matching on Aux */
  trait Fold[-FK <: FieldKey, F[_, _]] {
    def apply(f: FK): F[f.Args, f.Change]
    def map[G[_, _]](t: F ~~> G): Fold[FK, G]
  }

  case class FoldAll[F[_, _]](allTags        : AllTags.type         => F[AllTags        .Args, AllTags        .Change],
                              code           : Code.type            => F[Code           .Args, Code           .Change],
                              codes          : Codes.type           => F[Codes          .Args, Codes          .Change],
                              customFieldTags: CustomFieldTags      => F[CustomFieldTags#Args, CustomFieldTags#Change],
                              customTextField: CustomTextField      => F[CustomTextField#Args, CustomTextField#Change],
                              implications   : Implications         => F[Implications   #Args, Implications   #Change],
                              manualIssue    : ManualIssue          => F[ManualIssue    #Args, ManualIssue    #Change],
                              otherTags      : OtherTags.type       => F[OtherTags      .Args, OtherTags      .Change],
                              reqType        : ReqType.type         => F[ReqType        .Args, ReqType        .Change],
                              titleCG        : CodeGroupTitle.type  => F[CodeGroupTitle .Args, CodeGroupTitle .Change],
                              titleGR        : GenericReqTitle.type => F[GenericReqTitle.Args, GenericReqTitle.Change],
                              titleUC        : UseCaseTitle.type    => F[UseCaseTitle   .Args, UseCaseTitle   .Change],
                              useCaseStep    : UseCaseStep          => F[UseCaseStep    #Args, UseCaseStep    #Change],
                             ) extends Fold[FieldKey, F] {
    override def apply(f: FieldKey): F[f.Args, f.Change] = f.fold(this)
    override def map[G[_, _]](t: F ~~> G): FoldAll[G] =
      FoldAll(
        allTags         = f => t(allTags        (f)),
        code            = f => t(code           (f)),
        codes           = f => t(codes          (f)),
        customFieldTags = f => t(customFieldTags(f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        manualIssue     = f => t(manualIssue    (f)),
        otherTags       = f => t(otherTags      (f)),
        reqType         = f => t(reqType        (f)),
        titleCG         = f => t(titleCG        (f)),
        titleGR         = f => t(titleGR        (f)),
        titleUC         = f => t(titleUC        (f)),
        useCaseStep     = f => t(useCaseStep    (f)),
      )
  }

  case class FoldForCodeGroup[F[_, _]](code : Code          .type => F[Code          .Args, Code          .Change],
                                       title: CodeGroupTitle.type => F[CodeGroupTitle.Args, CodeGroupTitle.Change],
                                      ) extends Fold[ForCodeGroup, F] {
    override def apply(f: ForCodeGroup): F[f.Args, f.Change] = f.foldCG(this)
    override def map[G[_, _]](t: F ~~> G): FoldForCodeGroup[G] =
      FoldForCodeGroup(
        code  = f => t(code (f)),
        title = f => t(title(f)))
  }

  case class FoldForGenericReq[F[_, _]](codes          : Codes.type           => F[Codes          .Args, Codes          .Change],
                                        customTextField: CustomTextField      => F[CustomTextField#Args, CustomTextField#Change],
                                        implications   : Implications         => F[Implications   #Args, Implications   #Change],
                                        reqType        : ReqType.type         => F[ReqType        .Args, ReqType        .Change],
                                        allTags        : AllTags.type         => F[AllTags        .Args, AllTags        .Change],
                                        otherTags      : OtherTags.type       => F[OtherTags      .Args, OtherTags      .Change],
                                        customFieldTags: CustomFieldTags      => F[CustomFieldTags#Args, CustomFieldTags#Change],
                                        title          : GenericReqTitle.type => F[GenericReqTitle.Args, GenericReqTitle.Change],
                                       ) extends Fold[ForGenericReq, F] {
    override def apply(f: ForGenericReq): F[f.Args, f.Change] = f.foldGR(this)
    override def map[G[_, _]](t: F ~~> G): FoldForGenericReq[G] =
      FoldForGenericReq(
        codes           = f => t(codes          (f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        reqType         = f => t(reqType        (f)),
        allTags         = f => t(allTags        (f)),
        otherTags       = f => t(otherTags      (f)),
        customFieldTags = f => t(customFieldTags(f)),
        title           = f => t(title          (f)))
  }

  case class FoldForUseCase[F[_, _]](codes          : Codes.type        => F[Codes          .Args, Codes          .Change],
                                     customTextField: CustomTextField   => F[CustomTextField#Args, CustomTextField#Change],
                                     implications   : Implications      => F[Implications   #Args, Implications   #Change],
                                     allTags        : AllTags.type      => F[AllTags        .Args, AllTags        .Change],
                                     otherTags      : OtherTags.type    => F[OtherTags      .Args, OtherTags      .Change],
                                     customFieldTags: CustomFieldTags   => F[CustomFieldTags#Args, CustomFieldTags#Change],
                                     title          : UseCaseTitle.type => F[UseCaseTitle   .Args, UseCaseTitle   .Change],
                                    ) extends Fold[ForUseCase, F] {
    override def apply(f: ForUseCase): F[f.Args, f.Change] = f.foldUC(this)
    override def map[G[_, _]](t: F ~~> G): FoldForUseCase[G] =
      FoldForUseCase(
        codes           = f => t(codes          (f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        allTags         = f => t(allTags        (f)),
        otherTags       = f => t(otherTags      (f)),
        customFieldTags = f => t(customFieldTags(f)),
        title           = f => t(title          (f)))
  }

  case class FoldForUseCaseSteps[F[_, _]](step: UseCaseStep => F[UseCaseStep#Args, UseCaseStep#Change]) extends Fold[UseCaseStep, F] {
    override def apply(f: UseCaseStep): F[f.Args, f.Change] = f.foldUCS(this)
    override def map[G[_, _]](t: F ~~> G): FoldForUseCaseSteps[G] =
      FoldForUseCaseSteps(f => t(step(f)))
  }

  case class FoldForManualIssues[F[_, _]](text: ManualIssue => F[ManualIssue#Args, ManualIssue#Change]) extends Fold[ManualIssue, F] {
    override def apply(f: ManualIssue): F[f.Args, f.Change] = f.foldMI(this)
    override def map[G[_, _]](t: F ~~> G): FoldForManualIssues[G] =
      FoldForManualIssues(f => t(text(f)))
  }

  // ===================================================================================================================

  final class Type[F <: FieldKey](implicit ct: ClassTag[F]) {
    def widenFn[G >: F <: FieldKey, A](orig: F => A)(fallback: A): G => A =
      g => if (ct.runtimeClass.isInstance(g))
        orig(g.asInstanceOf[F])
      else
        fallback
  }
  implicit val typeCG : Type[ForCodeGroup]  = new Type
  implicit val typeGR : Type[ForGenericReq] = new Type
  implicit val typeUC : Type[ForUseCase]    = new Type
  implicit val typeMI : Type[ManualIssue]   = new Type
  implicit val typeUCS: Type[UseCaseStep]   = new Type


  // ===================================================================================================================

  sealed trait AndArgs {
    val key: FieldKey
    val args: key.Args
  }

  object AndArgs {
    implicit val reusability: Reusability[AndArgs] = {
      type F[A, V] = Reusability[A]
      val fold = FoldAll[F](
        allTags         = f => implicitly[Reusability[f.Args]],
        code            = f => implicitly[Reusability[f.Args]],
        codes           = f => implicitly[Reusability[f.Args]],
        customFieldTags = f => implicitly[Reusability[f.Args]],
        customTextField = f => implicitly[Reusability[f.Args]],
        implications    = f => implicitly[Reusability[f.Args]],
        manualIssue     = f => implicitly[Reusability[f.Args]],
        otherTags       = f => implicitly[Reusability[f.Args]],
        reqType         = f => implicitly[Reusability[f.Args]],
        titleCG         = f => implicitly[Reusability[f.Args]],
        titleGR         = f => implicitly[Reusability[f.Args]],
        titleUC         = f => implicitly[Reusability[f.Args]],
        useCaseStep     = f => implicitly[Reusability[f.Args]],
      )
      Reusability { (x, y) =>
        @inline def sameRef = x eq y
        @inline def sameKey = x.key == y.key
        @inline def sameArgs = x.key.fold(fold).test(x.args, y.args.asInstanceOf[x.key.Args])
        sameRef || (sameKey && sameArgs)
      }
    }
  }
}
