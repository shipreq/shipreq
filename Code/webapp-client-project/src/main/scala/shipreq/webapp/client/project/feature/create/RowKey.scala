package shipreq.webapp.client.project.feature.create

import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.feature.create.{FieldKey => AnyFieldKey}

sealed abstract class RowKey {
  type FieldKey <: AnyFieldKey
  def fold[F[_ <: AnyFieldKey]](f: RowKey.Fold[F]): F[FieldKey]
}

object RowKey {

  case object CodeGroup extends RowKey {
    override type FieldKey = FieldKey.ForCodeGroup
    override def fold[F[_ <: AnyFieldKey]](f: Fold[F]): F[FieldKey] = f.codeGroup(this)
  }

  final case class GenericReq(reqTypeId: CustomReqTypeId) extends RowKey {
    override type FieldKey = FieldKey.ForGenericReq
    override def fold[F[_ <: AnyFieldKey]](f: Fold[F]): F[FieldKey] = f.genericReq(this)
  }

  case object UseCase extends RowKey {
    override type FieldKey = FieldKey.ForUseCase
    override def fold[F[_ <: AnyFieldKey]](f: Fold[F]): F[FieldKey] = f.useCase(this)
  }

  @inline implicit def equality: UnivEq[RowKey] =
    UnivEq.derive

  implicit val reusability: Reusability[RowKey] =
    Reusability.byUnivEq

  def req(id: ReqTypeId): RowKey =
    id match {
      case i: CustomReqTypeId    => GenericReq(i)
      case StaticReqType.UseCase => UseCase
    }

  /** This shit is required to workaround Scala failing to be check exhaustivity when pattern-matching on Aux */
  case class Fold[F[_ <: FieldKey]](codeGroup   : CodeGroup .type => F[CodeGroup .FieldKey],
                                    genericReq  : GenericReq      => F[GenericReq#FieldKey],
                                    useCase     : UseCase   .type => F[UseCase   .FieldKey]) {
    @inline def apply(r: RowKey): F[r.FieldKey] = r.fold(this)
  }
}
