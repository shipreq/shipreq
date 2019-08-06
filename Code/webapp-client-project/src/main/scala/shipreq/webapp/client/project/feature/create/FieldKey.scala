package shipreq.webapp.client.project.feature.create

import japgolly.scalajs.react.Reusability
import monocle.{Iso, Prism}
import scalaz.~~>
import shipreq.base.util.Direction
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.feature.editor.{FieldKey => E}

/**
 * ADT representing all types of fields supported by the editor.
 * Meant to be used as a key for some given content (e.g. for requirement FR-1).
 */
sealed trait FieldKey {

  /** Arguments required for every .render call */
  final type Args = NewEditorArgs

  type Value

  @inline final def cast2[F[_], G[_, _]](f: F[G[Nothing, Any]]) = f.asInstanceOf[F[G[Args, Value]]]

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
    override type Value = ReqCode.Value
    override def foldCG[F[_, _]](f: FoldForCodeGroup[F]): F[Args, Value] = f.code(this)
  }

  case object CodeGroupTitle extends ForCodeGroup {
    override type Value = Text.CodeGroupTitle.OptionalText
    override def foldCG[F[_, _]](f: FoldForCodeGroup[F]): F[Args, Value] = f.title(this)
  }

  case object Codes extends ForAllReqs {
    override type Value = Set[ReqCode.Value]
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Value] = f.codes(this)
    override def foldUC[F[_, _]](f: FoldForUseCase   [F]): F[Args, Value] = f.codes(this)
  }

  final case class CustomTextField(field: CustomField.Text.Id) extends ForAllReqs {
    override type Value = Text.CustomTextField.OptionalText
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Value] = f.customTextField(this)
    override def foldUC[F[_, _]](f: FoldForUseCase   [F]): F[Args, Value] = f.customTextField(this)
  }

  case object GenericReqTitle extends ForGenericReq {
    override type Value = Text.GenericReqTitle.OptionalText
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Value] = f.title(this)
  }

  final case class Implications(scope: ImplicationScope) extends ForAllReqs {
    override type Value = Set[ReqId]
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Value] = f.implications(this)
    override def foldUC[F[_, _]](f: FoldForUseCase   [F]): F[Args, Value] = f.implications(this)
    def dir: Direction = ImplicationScope.dir(scope)
  }

  final case class Tags(field: Option[CustomField.Tag.Id]) extends ForAllReqs {
    override type Value = Set[ApplicableTagId]
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Value] = f.tags(this)
    override def foldUC[F[_, _]](f: FoldForUseCase   [F]): F[Args, Value] = f.tags(this)
  }

  case object UseCaseTitle extends ForUseCase {
    override type Value = Text.UseCaseTitle.OptionalText
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Value] = f.title(this)
  }

  case object ManualIssue extends ForManualIssue {
    override type Value = Text.ManualIssue.NonEmptyText
    override def foldMI[F[_, _]](f: FoldForManualIssue[F]): F[Args, Value] = f.text(this)
  }

  @inline implicit def equalityForSomeReq: UnivEq[ForSomeReq] =
    UnivEq.derive

  @inline implicit def equality: UnivEq[FieldKey] =
    UnivEq.derive

  implicit val reusability: Reusability[FieldKey] =
    Reusability.byUnivEq

  /** This shit is required to workaround Scala failing to be check exhaustivity when pattern-matching on Aux */
  trait Fold[-FK <: FieldKey, F[_, _]] {
    def apply(f: FK): F[f.Args, f.Value]
    def map[G[_, _]](t: F ~~> G): Fold[FK, G]
  }

  case class FoldForCodeGroup[F[_, _]](code : Code          .type => F[Code          .Args, Code          .Value],
                                       title: CodeGroupTitle.type => F[CodeGroupTitle.Args, CodeGroupTitle.Value],
                                      ) extends Fold[ForCodeGroup, F] {
    override def apply(f: ForCodeGroup): F[f.Args, f.Value] = f.foldCG(this)
    override def map[G[_, _]](t: F ~~> G): FoldForCodeGroup[G] =
      FoldForCodeGroup(
        code  = f => t(code (f)),
        title = f => t(title(f)))
  }

  case class FoldForGenericReq[F[_, _]](codes          : Codes.type           => F[Codes          .Args, Codes          .Value],
                                        customTextField: CustomTextField      => F[CustomTextField#Args, CustomTextField#Value],
                                        implications   : Implications         => F[Implications   #Args, Implications   #Value],
                                        tags           : Tags                 => F[Tags           #Args, Tags           #Value],
                                        title          : GenericReqTitle.type => F[GenericReqTitle.Args, GenericReqTitle.Value],
                                       ) extends Fold[ForGenericReq, F] {
    override def apply(f: ForGenericReq): F[f.Args, f.Value] = f.foldGR(this)
    override def map[G[_, _]](t: F ~~> G): FoldForGenericReq[G] =
      FoldForGenericReq(
        codes           = f => t(codes          (f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        tags            = f => t(tags           (f)),
        title           = f => t(title          (f)))
  }

  case class FoldForUseCase[F[_, _]](codes          : Codes.type        => F[Codes          .Args, Codes          .Value],
                                     customTextField: CustomTextField   => F[CustomTextField#Args, CustomTextField#Value],
                                     implications   : Implications      => F[Implications   #Args, Implications   #Value],
                                     tags           : Tags              => F[Tags           #Args, Tags           #Value],
                                     title          : UseCaseTitle.type => F[UseCaseTitle   .Args, UseCaseTitle   .Value],
                                    ) extends Fold[ForUseCase, F] {
    override def apply(f: ForUseCase): F[f.Args, f.Value] = f.foldUC(this)
    override def map[G[_, _]](t: F ~~> G): FoldForUseCase[G] =
      FoldForUseCase(
        codes           = f => t(codes          (f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        tags            = f => t(tags           (f)),
        title           = f => t(title          (f)))
  }

  case class FoldForManualIssue[F[_, _]](text: ManualIssue.type => F[ManualIssue.Args, ManualIssue.Value]) extends Fold[ForManualIssue, F] {
    override def apply(f: ForManualIssue): F[f.Args, f.Value] = f.foldMI(this)
    override def map[G[_, _]](t: F ~~> G): FoldForManualIssue[G] =
      FoldForManualIssue(
        text = f => t(text(f)))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed trait AndValue[+FK <: FieldKey, F[_, _]] {
    val field: FK
    val value: F[field.Args, field.Value]

    final def withValue[G[_, _]](a: G[field.Args, field.Value]): AndValue[field.type, G] =
      AndValue(field)(a)

    final def map[G[_, _]](f: F[field.Args, field.Value] => G[field.Args, field.Value]): AndValue[field.type, G] =
      withValue(f(value))

    final def trans[G[_, _]](f: F ~~> G): AndValue[field.type, G] =
      map(f.apply)

    final def foldValue[A](fold: Fold[FK, λ[(a, v)  => F[a, v] => A]]): A =
      fold(field)(value)
  }

  object AndValue {
    def apply[F[_, _]](f: FieldKey)(a: F[f.Args, f.Value]): AndValue[f.type, F] =
      new AndValue[f.type, F] {
        override val field = f
        override val value = a
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
    case E.Tags(field)            => Some(Tags(field))
  })({
    case Codes                  => E.Codes
    case CustomTextField(field) => E.CustomTextField(field)
    case GenericReqTitle        => E.GenericReqTitle
    case Implications(scope)    => E.Implications(scope)
    case Tags(field)            => E.Tags(field)
  })

  val editorFieldUC = Iso[E.ForUseCase, ForUseCase]({
    case E.Codes                  => Codes
    case E.CustomTextField(field) => CustomTextField(field)
    case E.Implications(scope)    => Implications(scope)
    case E.Tags(field)            => Tags(field)
    case E.UseCaseTitle           => UseCaseTitle
  })({
    case Codes                  => E.Codes
    case CustomTextField(field) => E.CustomTextField(field)
    case UseCaseTitle           => E.UseCaseTitle
    case Implications(scope)    => E.Implications(scope)
    case Tags(field)            => E.Tags(field)
  })
}
