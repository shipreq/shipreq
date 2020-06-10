package shipreq.webapp.client.project.feature.create

import japgolly.scalajs.react.Reusability
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.websocket.{CreateContentCmd, ManualIssueCmd}
import shipreq.webapp.client.project.feature.create.{FieldKey => AnyFieldKey}
import shipreq.webapp.client.project.lib.DataReusability._

sealed abstract class RowKey {
  type FieldKey <: AnyFieldKey
  type Cmd
  def foldF[F[_ <: AnyFieldKey]](f: RowKey.Fold[F]): F[FieldKey]
  def foldC[F[_]](f: RowKey.FoldCmd[F]): F[Cmd]
}

object RowKey {

  case object CodeGroup extends RowKey {
    override type FieldKey = FieldKey.ForCodeGroup
    override type Cmd      = CreateContentCmd
    override def foldF[F[_ <: AnyFieldKey]](f: Fold[F]): F[FieldKey] = f.codeGroup(this)
    override def foldC[F[_]](f: FoldCmd[F]): F[Cmd] = f.codeGroup(this)
  }

  final case class GenericReq(reqTypeId: CustomReqTypeId) extends RowKey {
    override type FieldKey = FieldKey.ForGenericReq
    override type Cmd      = CreateContentCmd
    override def foldF[F[_ <: AnyFieldKey]](f: Fold[F]): F[FieldKey] = f.genericReq(this)
    override def foldC[F[_]](f: FoldCmd[F]): F[Cmd] = f.genericReq(this)
  }

  case object UseCase extends RowKey {
    override type FieldKey = FieldKey.ForUseCase
    override type Cmd      = CreateContentCmd
    override def foldF[F[_ <: AnyFieldKey]](f: Fold[F]): F[FieldKey] = f.useCase(this)
    override def foldC[F[_]](f: FoldCmd[F]): F[Cmd] = f.useCase(this)
    @inline def reqTypeId = StaticReqType.UseCase
  }

  case object ManualIssue extends RowKey {
    override type FieldKey = FieldKey.ForManualIssue
    override type Cmd      = ManualIssueCmd
    override def foldF[F[_ <: AnyFieldKey]](f: Fold[F]): F[FieldKey] = f.manualIssue(this)
    override def foldC[F[_]](f: FoldCmd[F]): F[Cmd] = f.manualIssue(this)
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
  final case class Fold[F[_ <: FieldKey]](codeGroup  : CodeGroup  .type => F[CodeGroup  .FieldKey],
                                          genericReq : GenericReq       => F[GenericReq #FieldKey],
                                          useCase    : UseCase    .type => F[UseCase    .FieldKey],
                                          manualIssue: ManualIssue.type => F[ManualIssue.FieldKey],
                                         ) {
    @inline def apply(r: RowKey): F[r.FieldKey] = r.foldF(this)
  }

  /** This shit is required to workaround Scala failing to be check exhaustivity when pattern-matching on Aux */
  final case class FoldCmd[F[_]](codeGroup  : CodeGroup  .type => F[CodeGroup  .Cmd],
                                 genericReq : GenericReq       => F[GenericReq #Cmd],
                                 useCase    : UseCase    .type => F[UseCase    .Cmd],
                                 manualIssue: ManualIssue.type => F[ManualIssue.Cmd],
                                ) {
    @inline def apply(r: RowKey): F[r.Cmd] = r.foldC(this)
  }
}
