package shipreq.webapp.client.project.feature.create

import japgolly.scalajs.react.extra.Reusability
import monocle.{Iso, Prism}
import scalaz.~>
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
  type Value
  @inline final def cast2[F[_], G[_]](f: F[G[Any]]) = f.asInstanceOf[F[G[Value]]]

  final type AndValue[F[_]] = FieldKey.AndValue[this.type, F]
  @inline final def andValue[F[_]](a: F[Value]): AndValue[F] =
    FieldKey.AndValue(this)(a)
}

object FieldKey {

  sealed trait ForCodeGroup  extends FieldKey   { def foldCG[F[_]](f: FoldForCodeGroup [F]): F[Value] }
  sealed trait ForGenericReq extends ForSomeReq { def foldGR[F[_]](f: FoldForGenericReq[F]): F[Value] }
  sealed trait ForUseCase    extends ForSomeReq { def foldUC[F[_]](f: FoldForUseCase   [F]): F[Value] }

  /** Fields apply to one or more type of reqs */
  sealed trait ForSomeReq extends FieldKey

  /** Fields apply to all types of reqs */
  sealed trait ForAllReqs extends ForGenericReq with ForUseCase

  case object Code extends ForCodeGroup {
    override type Value = ReqCode.Value
    override def foldCG[F[_]](f: FoldForCodeGroup[F]): F[Value] = f.code(this)
  }

  case object CodeGroupTitle extends ForCodeGroup {
    override type Value = Text.CodeGroupTitle.OptionalText
    override def foldCG[F[_]](f: FoldForCodeGroup[F]): F[Value] = f.title(this)
  }

  case object Codes extends ForAllReqs {
    override type Value = Set[ReqCode.Value]
    override def foldGR[F[_]](f: FoldForGenericReq[F]): F[Value] = f.codes(this)
    override def foldUC[F[_]](f: FoldForUseCase[F]): F[Value] = f.codes(this)
  }

  final case class CustomTextField(field: CustomField.Text.Id) extends ForAllReqs {
    override type Value = Text.CustomTextField.OptionalText
    override def foldGR[F[_]](f: FoldForGenericReq[F]): F[Value] = f.customTextField(this)
    override def foldUC[F[_]](f: FoldForUseCase[F]): F[Value] = f.customTextField(this)
  }

  case object GenericReqTitle extends ForGenericReq {
    override type Value = Text.GenericReqTitle.OptionalText
    override def foldGR[F[_]](f: FoldForGenericReq[F]): F[Value] = f.title(this)
  }

  final case class Implications(scope: ImplicationScope) extends ForAllReqs {
    override type Value = Set[ReqId]
    override def foldGR[F[_]](f: FoldForGenericReq[F]): F[Value] = f.implications(this)
    override def foldUC[F[_]](f: FoldForUseCase[F]): F[Value] = f.implications(this)
    def dir: Direction = ImplicationScope.dir(scope)
  }

  final case class Tags(field: Option[CustomField.Tag.Id]) extends ForAllReqs {
    override type Value = Set[ApplicableTagId]
    override def foldGR[F[_]](f: FoldForGenericReq[F]): F[Value] = f.tags(this)
    override def foldUC[F[_]](f: FoldForUseCase[F]): F[Value] = f.tags(this)
  }

  case object UseCaseTitle extends ForUseCase {
    override type Value = Text.UseCaseTitle.OptionalText
    override def foldUC[F[_]](f: FoldForUseCase[F]): F[Value] = f.title(this)
  }

  @inline implicit def equalityForSomeReq: UnivEq[ForSomeReq] =
    UnivEq.derive

  @inline implicit def equality: UnivEq[FieldKey] =
    UnivEq.derive

  implicit val reusability: Reusability[FieldKey] =
    Reusability.byUnivEq

  /** This shit is required to workaround Scala failing to be check exhaustivity when pattern-matching on Aux */
  trait Fold[-FK <: FieldKey, F[_]] {
    def apply(f: FK): F[f.Value]
    def map[G[_]](t: F ~> G): Fold[FK, G]
  }

  case class FoldForCodeGroup[F[_]](code : Code          .type => F[Code          .Value],
                                    title: CodeGroupTitle.type => F[CodeGroupTitle.Value],
                                   ) extends Fold[ForCodeGroup, F] {
    override def apply(f: ForCodeGroup): F[f.Value] = f.foldCG(this)
    override def map[G[_]](t: F ~> G): FoldForCodeGroup[G] =
      FoldForCodeGroup(
        code  = f => t(code (f)),
        title = f => t(title(f)))
  }

  case class FoldForGenericReq[F[_]](codes          : Codes.type           => F[Codes          .Value],
                                     customTextField: CustomTextField      => F[CustomTextField#Value],
                                     implications   : Implications         => F[Implications   #Value],
                                     tags           : Tags                 => F[Tags           #Value],
                                     title          : GenericReqTitle.type => F[GenericReqTitle.Value],
                                    ) extends Fold[ForGenericReq, F] {
    override def apply(f: ForGenericReq): F[f.Value] = f.foldGR(this)
    override def map[G[_]](t: F ~> G): FoldForGenericReq[G] =
      FoldForGenericReq(
        codes           = f => t(codes          (f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        tags            = f => t(tags           (f)),
        title           = f => t(title          (f)))
  }

  case class FoldForUseCase[F[_]](codes          : Codes.type        => F[Codes          .Value],
                                  customTextField: CustomTextField   => F[CustomTextField#Value],
                                  implications   : Implications      => F[Implications   #Value],
                                  tags           : Tags              => F[Tags           #Value],
                                  title          : UseCaseTitle.type => F[UseCaseTitle   .Value],
                                 ) extends Fold[ForUseCase, F] {
    override def apply(f: ForUseCase): F[f.Value] = f.foldUC(this)
    override def map[G[_]](t: F ~> G): FoldForUseCase[G] =
      FoldForUseCase(
        codes           = f => t(codes          (f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        tags            = f => t(tags           (f)),
        title           = f => t(title          (f)))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed trait AndValue[+FK <: FieldKey, F[_]] {
    val field: FK
    val value: F[field.Value]

    final def withValue[G[_]](a: G[field.Value]): AndValue[field.type, G] =
      AndValue(field)(a)

    final def map[G[_]](f: F[field.Value] => G[field.Value]): AndValue[field.type, G] =
      withValue(f(value))

    final def trans[G[_]](f: F ~> G): AndValue[field.type, G] =
      map(f.apply)

    final def foldValue[A](fold: Fold[FK, λ[v => F[v] => A]]): A =
      fold(field)(value)
  }

  object AndValue {
    def apply[F[_]](f: FieldKey)(a: F[f.Value]): AndValue[f.type, F] =
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
