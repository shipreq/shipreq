package shipreq.webapp.base.protocol

import scalaz.{NonEmptyList, \&/}
import upickle.{Reader, Writer}
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta.RemoteDelta
import Codec._, DataCodecs._, DeltaCodecs._, RoutineCodecs._
import Routine._

trait Crudable extends Desc {
  type Id
  type V
  override final type I  = CrudAction[Id, V]
  override final type O  = RemoteDelta

  final type Action = CrudAction[Id, V]
  @inline final def create(v: V)                      : Action = CrudAction.Create[V]    (v)
  @inline final def update(id: Id, v: V)              : Action = CrudAction.Update[Id, V](id, v)
  @inline final def delete(id: Id, a: DeletionAction) : Action = CrudAction.Delete[Id]   (id, a)
}

object Crudable {
  type Aux[_I, _V] = Crudable { type Id = _I; type V = _V }

  class CAux[_Id, _V] private[protocol](implicit RI: Reader[_Id], WI: Writer[_Id], RV: Reader[_V], WV: Writer[_V]
                                        , RO: Reader[RemoteDelta], WO: Writer[RemoteDelta]) extends Crudable {
    override final type Id = _Id
    override final type V  = _V

    override implicit final def ri: Reader[I] = crudAction[_Id, _V]
    override implicit final def wi: Writer[I] = crudAction[_Id, _V]
    override implicit final def ro: Reader[O] = RO
    override implicit final def wo: Writer[O] = WO

    implicit def rwd = remoteRoutine(this)
  }
}

sealed trait CrudAction[+Id, +V]
object CrudAction {
  final case class Create[V]    (newValues: V)                   extends CrudAction[Nothing, V]
  final case class Update[Id, V](id: Id, newValues: V)           extends CrudAction[Id     , V]
  final case class Delete[Id]   (id: Id, action: DeletionAction) extends CrudAction[Id     , Nothing]
}


sealed abstract class DeletionAction
object DeletionAction {
  case object HardDel extends DeletionAction
  case object SoftDel extends DeletionAction
  case object Restore extends DeletionAction
  def values = NonEmptyList[DeletionAction](HardDel, SoftDel, Restore)
}

//======================================================================================================================

object Routines {

  object ProjectInit extends DescT[Unit, Project]

  object CustomIssueTypeCrud extends Crudable.CAux[CustomIssueType.Id, (RefKey, Option[String])]
  object CustomReqTypeCrud   extends Crudable.CAux[CustomReqType.Id,   (ReqType.Mnemonic, String, ImplicationRequired)]
  object TagCrud             extends Crudable.CAux[Tag.Id,             TagProtocol.Values \&/ TagProtocol.PovRelations]

  object CustomReqTypeImplicationMod extends DescT[(CustomReqType.Id, ImplicationRequired), RemoteDelta]

  // TODO rename ForCfgReqType
  case class ForCfgReqType(projectInit:   ProjectInit.Remote,
                           issueTypeCrud: CustomIssueTypeCrud.Remote,
                           reqTypeCrud:   CustomReqTypeCrud.Remote,
                           reqTypeImpMod: CustomReqTypeImplicationMod.Remote,
                           tagCrud:       TagCrud.Remote)
    extends Group
}
