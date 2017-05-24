package shipreq.webapp.client.project.feature.editor2

import japgolly.scalajs.react.extra.Reusability
import scala.reflect.ClassTag
import scalaz.~>
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.UseCaseStepGD
import shipreq.webapp.base.text.Text
import shipreq.webapp.client.project.lib.DataReusability._

/**
 * ADT representing all types of fields supported by the editor.
 * Meant to be used as a key for some given content (e.g. for requirement FR-1).
 */
sealed trait FieldKey {
  type Change
  @inline final def cast2[F[_], G[_]](f: F[G[Any]]) = f.asInstanceOf[F[G[Change]]]
}

object FieldKey {

  sealed trait ForCodeGroup  extends FieldKey   { def foldCG[F[_]](f: FoldForCodeGroup [F]): F[Change] }
  sealed trait ForGenericReq extends ForSomeReq { def foldGR[F[_]](f: FoldForGenericReq[F]): F[Change] }
  sealed trait ForUseCase    extends ForSomeReq { def foldUC[F[_]](f: FoldForUseCase   [F]): F[Change] }

  /** Fields apply to one or more type of reqs */
  sealed trait ForSomeReq extends FieldKey

  /** Fields apply to all types of reqs */
  sealed trait ForAllReqs extends ForGenericReq with ForUseCase

  case object Code extends ForCodeGroup {
    override type Change = ReqCode.Value
    override def foldCG[F[_]](f: FoldForCodeGroup[F]): F[Change] = f.code(this)
  }

  case object CodeGroupTitle extends ForCodeGroup {
    override type Change = Text.CodeGroupTitle.OptionalText
    override def foldCG[F[_]](f: FoldForCodeGroup[F]): F[Change] = f.title(this)
  }

  case object Codes extends ForAllReqs {
    override type Change = SetDiff.NE[ReqCode.Value]
    override def foldGR[F[_]](f: FoldForGenericReq[F]): F[Change] = f.codes(this)
    override def foldUC[F[_]](f: FoldForUseCase[F]): F[Change] = f.codes(this)
  }

  final case class CustomTextField(field: CustomField.Text.Id) extends ForAllReqs {
    override type Change = Text.CustomTextField.OptionalText
    override def foldGR[F[_]](f: FoldForGenericReq[F]): F[Change] = f.customTextField(this)
    override def foldUC[F[_]](f: FoldForUseCase[F]): F[Change] = f.customTextField(this)
  }

  case object GenericReqTitle extends ForGenericReq {
    override type Change = Text.GenericReqTitle.OptionalText
    override def foldGR[F[_]](f: FoldForGenericReq[F]): F[Change] = f.title(this)
  }

  final case class Implications(scope: ImplicationScope) extends ForAllReqs {
    override type Change = SetDiff.NE[ReqId]
    override def foldGR[F[_]](f: FoldForGenericReq[F]): F[Change] = f.implications(this)
    override def foldUC[F[_]](f: FoldForUseCase[F]): F[Change] = f.implications(this)
  }

  case object ReqType extends ForGenericReq {
    override type Change = CustomReqType
    override def foldGR[F[_]](f: FoldForGenericReq[F]): F[Change] = f.reqType(this)
  }

  final case class Tags(field: Option[CustomField.Tag.Id]) extends ForAllReqs {
    override type Change = SetDiff.NE[ApplicableTagId]
    override def foldGR[F[_]](f: FoldForGenericReq[F]): F[Change] = f.tags(this)
    override def foldUC[F[_]](f: FoldForUseCase[F]): F[Change] = f.tags(this)
  }

  final case class UseCaseStep(id: UseCaseStepId) extends FieldKey {
    override type Change = UseCaseStepGD.NonEmptyValues
    def foldUCS[F[_]](f: FoldForUseCaseSteps[F]): F[Change] = f.step(this)
  }

  case object UseCaseTitle extends ForUseCase {
    override type Change = Text.UseCaseTitle.OptionalText
    override def foldUC[F[_]](f: FoldForUseCase[F]): F[Change] = f.title(this)
  }

  @inline implicit def equality: UnivEq[FieldKey] =
    UnivEq.derive

  implicit val reusability: Reusability[FieldKey] =
    Reusability.byUnivEq

  def reqTitle(id: ReqId): ForSomeReq =
    id match {
      case _: GenericReqId => GenericReqTitle
      case _: UseCaseId    => UseCaseTitle
    }

  type Aux[C] = FieldKey { type Change = C }

  /** This shit is required to workaround Scala failing to be check exhaustivity when pattern-matching on Aux */
  trait Fold[-FK <: FieldKey, F[_]] {
    def apply(f: FK): F[f.Change]
    def map[G[_]](t: F ~> G): Fold[FK, G]
  }

  case class FoldForCodeGroup[F[_]](code : Code          .type => F[Code          .Change],
                                    title: CodeGroupTitle.type => F[CodeGroupTitle.Change],
                                   ) extends Fold[ForCodeGroup, F] {
    override def apply(f: ForCodeGroup): F[f.Change] = f.foldCG(this)
    override def map[G[_]](t: F ~> G): FoldForCodeGroup[G] =
      FoldForCodeGroup(
        code  = f => t(code (f)),
        title = f => t(title(f)))
  }

  case class FoldForGenericReq[F[_]](codes          : Codes.type           => F[Codes          .Change],
                                     customTextField: CustomTextField      => F[CustomTextField#Change],
                                     implications   : Implications         => F[Implications   #Change],
                                     reqType        : ReqType.type         => F[ReqType        .Change],
                                     tags           : Tags                 => F[Tags           #Change],
                                     title          : GenericReqTitle.type => F[GenericReqTitle.Change],
                                    ) extends Fold[ForGenericReq, F] {
    override def apply(f: ForGenericReq): F[f.Change] = f.foldGR(this)
    override def map[G[_]](t: F ~> G): FoldForGenericReq[G] =
      FoldForGenericReq(
        codes           = f => t(codes          (f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        reqType         = f => t(reqType        (f)),
        tags            = f => t(tags           (f)),
        title           = f => t(title          (f)))
  }

  case class FoldForUseCase[F[_]](codes          : Codes.type        => F[Codes          .Change],
                                  customTextField: CustomTextField   => F[CustomTextField#Change],
                                  implications   : Implications      => F[Implications   #Change],
                                  tags           : Tags              => F[Tags           #Change],
                                  title          : UseCaseTitle.type => F[UseCaseTitle   .Change],
                                 ) extends Fold[ForUseCase, F] {
    override def apply(f: ForUseCase): F[f.Change] = f.foldUC(this)
    override def map[G[_]](t: F ~> G): FoldForUseCase[G] =
      FoldForUseCase(
        codes           = f => t(codes          (f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        tags            = f => t(tags           (f)),
        title           = f => t(title          (f)))
  }

  case class FoldForUseCaseSteps[F[_]](step: UseCaseStep => F[UseCaseStep#Change]) extends Fold[UseCaseStep, F] {
    override def apply(f: UseCaseStep): F[f.Change] = f.foldUCS(this)
    override def map[G[_]](t: F ~> G): FoldForUseCaseSteps[G] =
      FoldForUseCaseSteps(f => t(step(f)))

  }

  final class Type[F <: FieldKey](implicit ct: ClassTag[F]) {
    def widenFn[G >: F <: FieldKey, A](orig: F => A)(fallback: A): G => A =
      g => if (ct.runtimeClass.isInstance(g))
        orig(g.asInstanceOf[F])
      else
        fallback
  }
  implicit val typeGR: Type[ForGenericReq] = new Type
  implicit val typeUC: Type[ForUseCase]    = new Type
}
