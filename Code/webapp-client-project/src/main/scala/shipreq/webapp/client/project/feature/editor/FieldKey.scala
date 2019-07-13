package shipreq.webapp.client.project.feature.editor

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.Reusability
import scala.reflect.ClassTag
import scalaz.{-\/, \/-, ~~>}
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.UseCaseStepGD
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.Text
import shipreq.webapp.client.project.lib.DataReusability._

/**
 * ADT representing all types of fields supported by the editor.
 * Meant to be used as a key for some given content (e.g. for requirement FR-1).
 */
sealed trait FieldKey {

  /** Arguments required for every .render call */
  type Args

  /** Description of changes the user has made in the editor */
  type Change

  @inline final def cast2[F[_], G[_, _], A, B](f: F[G[A, B]]) = f.asInstanceOf[F[G[Args, Change]]]
}

object FieldKey {

  /** Fields apply to one or more type of reqs */
  sealed trait ForSomeReq extends FieldKey {
    override final type Args = Unit
  }

  /** Fields apply to all types of reqs */
  sealed trait ForAllReqs extends ForGenericReq with ForUseCase

  sealed trait ForCodeGroup extends FieldKey {
    override final type Args = Unit
    def foldCG[F[_, _]](f: FoldForCodeGroup[F]): F[Args, Change]
  }

  sealed trait ForGenericReq extends ForSomeReq {
    def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change]
  }

  sealed trait ForUseCase extends ForSomeReq {
    def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change]
  }

  case object Code extends ForCodeGroup {
    override type Change = ReqCode.Value
    override def foldCG[F[_, _]](f: FoldForCodeGroup[F]): F[Args, Change] = f.code(this)
  }

  case object CodeGroupTitle extends ForCodeGroup {
    override type Change = Text.CodeGroupTitle.OptionalText
    override def foldCG[F[_, _]](f: FoldForCodeGroup[F]): F[Args, Change] = f.title(this)
  }

  case object Codes extends ForAllReqs {
    override type Change = SetDiff.NE[ReqCode.Value]
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.codes(this)
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change] = f.codes(this)
  }

  final case class CustomTextField(field: CustomField.Text.Id) extends ForAllReqs {
    override type Change = Text.CustomTextField.OptionalText
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.customTextField(this)
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change] = f.customTextField(this)
  }

  case object GenericReqTitle extends ForGenericReq {
    override type Change = Text.GenericReqTitle.OptionalText
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.title(this)
  }

  final case class Implications(scope: ImplicationScope) extends ForAllReqs {
    override type Change = SetDiff.NE[ReqId]
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.implications(this)
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change] = f.implications(this)
  }

  case object ReqType extends ForGenericReq {
    override type Change = CustomReqType
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.reqType(this)
  }

  final case class Tags(field: Option[CustomField.Tag.Id]) extends ForAllReqs {
    override type Change = SetDiff.NE[ApplicableTagId]
    override def foldGR[F[_, _]](f: FoldForGenericReq[F]): F[Args, Change] = f.tags(this)
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change] = f.tags(this)
  }

  final case class UseCaseStep(id: UseCaseStepId) extends FieldKey {
    override type Args = UseCaseStep.Args
    override type Change = UseCaseStepGD.NonEmptyValues
    def foldUCS[F[_, _]](f: FoldForUseCaseSteps[F]): F[Args, Change] = f.step(this)
  }

  object UseCaseStep {
    /**
      * @param shiftRunner   so users can shift the step left/right via keyboard shortcuts.
      * @param addStepRunner so users can add a new step via keyboard shortcuts.
      */
    final case class Args(shiftRunner  : AsyncFeature.Runner.D0[UpdateContentCmd.ForUseCaseStep, Any],
                          addStepRunner: AsyncFeature.Runner.D0[UpdateContentCmd.AddUseCaseStep, Any])
  }

  case object UseCaseTitle extends ForUseCase {
    override type Change = Text.UseCaseTitle.OptionalText
    override def foldUC[F[_, _]](f: FoldForUseCase[F]): F[Args, Change] = f.title(this)
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
      case i: CustomField.Tag.Id         => Tags(Some(i))
      case i: CustomField.Implication.Id => Implications(-\/(i))
    }

  def impliedBy = Implications(\/-(Backwards))

  def reqTextLoc(reqId: ReqId, loc: ReqTextLoc): FieldKey =
    loc match {
      case ReqTextLoc.Title                    => reqTitle(reqId)
      case ReqTextLoc.CustomTextField(fieldId) => CustomTextField(fieldId)
      case ReqTextLoc.UseCaseStep(stepId)      => UseCaseStep(stepId)
    }

  def reqTitle(id: ReqId): ForSomeReq =
    id match {
      case _: GenericReqId => GenericReqTitle
      case _: UseCaseId    => UseCaseTitle
    }

  type Aux[A, C] = FieldKey { type Args = A; type Change = C }
  type Nullary = FieldKey { type Args = Unit }

  /** This shit is required to workaround Scala failing to be check exhaustivity when pattern-matching on Aux */
  trait Fold[-FK <: FieldKey, F[_, _]] {
    def apply(f: FK): F[f.Args, f.Change]
    def map[G[_, _]](t: F ~~> G): Fold[FK, G]
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
                                        tags           : Tags                 => F[Tags           #Args, Tags           #Change],
                                        title          : GenericReqTitle.type => F[GenericReqTitle.Args, GenericReqTitle.Change],
                                       ) extends Fold[ForGenericReq, F] {
    override def apply(f: ForGenericReq): F[f.Args, f.Change] = f.foldGR(this)
    override def map[G[_, _]](t: F ~~> G): FoldForGenericReq[G] =
      FoldForGenericReq(
        codes           = f => t(codes          (f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        reqType         = f => t(reqType        (f)),
        tags            = f => t(tags           (f)),
        title           = f => t(title          (f)))
  }

  case class FoldForUseCase[F[_, _]](codes          : Codes.type        => F[Codes          .Args, Codes          .Change],
                                     customTextField: CustomTextField   => F[CustomTextField#Args, CustomTextField#Change],
                                     implications   : Implications      => F[Implications   #Args, Implications   #Change],
                                     tags           : Tags              => F[Tags           #Args, Tags           #Change],
                                     title          : UseCaseTitle.type => F[UseCaseTitle   .Args, UseCaseTitle   .Change],
                                    ) extends Fold[ForUseCase, F] {
    override def apply(f: ForUseCase): F[f.Args, f.Change] = f.foldUC(this)
    override def map[G[_, _]](t: F ~~> G): FoldForUseCase[G] =
      FoldForUseCase(
        codes           = f => t(codes          (f)),
        customTextField = f => t(customTextField(f)),
        implications    = f => t(implications   (f)),
        tags            = f => t(tags           (f)),
        title           = f => t(title          (f)))
  }

  case class FoldForUseCaseSteps[F[_, _]](step: UseCaseStep => F[UseCaseStep#Args, UseCaseStep#Change]) extends Fold[UseCaseStep, F] {
    override def apply(f: UseCaseStep): F[f.Args, f.Change] = f.foldUCS(this)
    override def map[G[_, _]](t: F ~~> G): FoldForUseCaseSteps[G] =
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
