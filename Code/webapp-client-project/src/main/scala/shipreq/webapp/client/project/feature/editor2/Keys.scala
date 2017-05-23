package shipreq.webapp.client.project.feature.editor2

import japgolly.scalajs.react.extra.Reusability
import scalaz.\/
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data
import shipreq.webapp.client.project.lib.DataReusability._

sealed abstract class RowKey {
  type FieldKey <: shipreq.webapp.client.project.feature.editor2.FieldKey

  import RowKey._

  /** This shit is required to workaround Scala failing to be check exhaustivity when pattern-matching on RowKey.Aux */
  def fold[F[_]](codeGroup   : CodeGroup  => F[FieldKey.ForCodeGroup],
                 genericReq  : GenericReq => F[FieldKey.ForGenericReq],
                 useCase     : UseCase    => F[FieldKey.ForUseCase],
                 useCaseSteps:            => F[FieldKey.UseCaseStep]): F[FieldKey]
}

object RowKey {
  type Aux[F <: FieldKey] = RowKey { type FieldKey = F }

  final case class CodeGroup(id: data.ReqCodeId) extends RowKey {
    override type FieldKey = FieldKey.ForCodeGroup
    override def fold[F[_]](codeGroup   : CodeGroup  => F[FieldKey.ForCodeGroup],
                            genericReq  : GenericReq => F[FieldKey.ForGenericReq],
                            useCase     : UseCase    => F[FieldKey.ForUseCase],
                            useCaseSteps:            => F[FieldKey.UseCaseStep]): F[FieldKey] =
      codeGroup(this)
  }

  final case class GenericReq(id: data.GenericReqId) extends RowKey {
    override type FieldKey = FieldKey.ForGenericReq
    override def fold[F[_]](codeGroup   : CodeGroup  => F[FieldKey.ForCodeGroup],
                            genericReq  : GenericReq => F[FieldKey.ForGenericReq],
                            useCase     : UseCase    => F[FieldKey.ForUseCase],
                            useCaseSteps:            => F[FieldKey.UseCaseStep]): F[FieldKey] =
      genericReq(this)
  }

  final case class UseCase(id: data.UseCaseId) extends RowKey {
    override type FieldKey = FieldKey.ForUseCase
    override def fold[F[_]](codeGroup   : CodeGroup  => F[FieldKey.ForCodeGroup],
                            genericReq  : GenericReq => F[FieldKey.ForGenericReq],
                            useCase     : UseCase    => F[FieldKey.ForUseCase],
                            useCaseSteps:            => F[FieldKey.UseCaseStep]): F[FieldKey] =
      useCase(this)
  }

  case object UseCaseSteps extends RowKey {
    override type FieldKey = FieldKey.UseCaseStep
    override def fold[F[_]](codeGroup   : CodeGroup  => F[FieldKey.ForCodeGroup],
                            genericReq  : GenericReq => F[FieldKey.ForGenericReq],
                            useCase     : UseCase    => F[FieldKey.ForUseCase],
                            useCaseSteps:            => F[FieldKey.UseCaseStep]): F[FieldKey] =
      useCaseSteps
  }

  @inline implicit def equality: UnivEq[RowKey] =
    UnivEq.derive

  implicit val reusability: Reusability[RowKey] =
    Reusability.byUnivEq
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * ADT representing all types of fields supported by the editor.
 * Meant to be used as a key for some given content (e.g. for requirement FR-1).
 */
sealed trait FieldKey {
  type Change
}
object FieldKey {
  type Aux[C] = FieldKey { type Change = C }

  sealed trait ForCodeGroup  extends FieldKey
  sealed trait ForGenericReq extends FieldKey
  sealed trait ForUseCase    extends FieldKey
  sealed trait ForReq        extends ForGenericReq with ForUseCase

  case object Code extends ForCodeGroup {
    override type Change = Unit
  }

  case object CodeGroupTitle extends ForCodeGroup {
    override type Change = Unit
  }

  case object Codes extends ForReq {
    override type Change = Unit
  }

  final case class CustomTextField(field: data.CustomField.Text.Id) extends ForReq {
    override type Change = Unit
  }

  case object GenericReqTitle extends ForGenericReq {
    override type Change = Unit
  }

  final case class Implications(scope: data.ImplicationScope) extends ForReq {
    override type Change = Unit
  }

  case object ReqType extends ForGenericReq {
    override type Change = Unit
  }

  final case class Tags(field: Option[data.CustomField.Tag.Id]) extends ForReq {
    override type Change = Unit
  }

  final case class UseCaseStep(id: data.UseCaseStepId) extends FieldKey {
    override type Change = Unit
  }

  case object UseCaseTitle extends ForUseCase {
    override type Change = Unit
  }

  @inline implicit def equality: UnivEq[FieldKey] =
    UnivEq.derive

  implicit val reusability: Reusability[FieldKey] =
    Reusability.byUnivEq
}
