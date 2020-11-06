package shipreq.webapp.client.project.feature.render

import japgolly.scalajs.react.Reusability
import shipreq.webapp.client.project.feature.render.{FieldKey => AnyFieldKey}
import shipreq.webapp.client.project.util.DataReusability._
import shipreq.webapp.member.project.data._

sealed abstract class RowKey {
  type FieldKey <: AnyFieldKey
  def fold[F[_ <: AnyFieldKey]](f: RowKey.Fold[F]): F[FieldKey]
}

object RowKey {

  final case class CodeGroup(id: ReqCodeGroupId) extends RowKey {
    override type FieldKey = FieldKey.ForCodeGroup
    override def fold[F[_ <: AnyFieldKey]](f: Fold[F]): F[FieldKey] = f.codeGroup(this)
  }

  final case class GenericReq(id: GenericReqId) extends RowKey {
    override type FieldKey = FieldKey.ForGenericReq
    override def fold[F[_ <: AnyFieldKey]](f: Fold[F]): F[FieldKey] = f.genericReq(this)
  }

  final case class UseCase(id: UseCaseId) extends RowKey {
    override type FieldKey = FieldKey.ForUseCase
    override def fold[F[_ <: AnyFieldKey]](f: Fold[F]): F[FieldKey] = f.useCase(this)
  }

  case object UseCaseSteps extends RowKey {
    override type FieldKey = FieldKey.UseCaseStep
    override def fold[F[_ <: AnyFieldKey]](f: Fold[F]): F[FieldKey] = f.useCaseSteps()
  }

  case object ManualIssues extends RowKey {
    override type FieldKey = FieldKey.ManualIssue
    override def fold[F[_ <: AnyFieldKey]](f: Fold[F]): F[FieldKey] = f.manualIssues()
  }

  @inline implicit def equality: UnivEq[RowKey] =
    UnivEq.derive

  implicit val reusability: Reusability[RowKey] =
    Reusability.byUnivEq

  def req(id: ReqId): RowKey =
    id.foldReqId(GenericReq, UseCase)

  /** This shit is required to workaround Scala failing to be check exhaustivity when pattern-matching on Aux */
  case class Fold[F[_ <: FieldKey]](codeGroup   : CodeGroup  => F[CodeGroup   #FieldKey],
                                    genericReq  : GenericReq => F[GenericReq  #FieldKey],
                                    useCase     : UseCase    => F[UseCase     #FieldKey],
                                    useCaseSteps: ()         => F[UseCaseSteps.FieldKey],
                                    manualIssues: ()         => F[ManualIssues.FieldKey],
                                    ) {
    @inline def apply(r: RowKey): F[r.FieldKey] = r.fold(this)
  }
}
