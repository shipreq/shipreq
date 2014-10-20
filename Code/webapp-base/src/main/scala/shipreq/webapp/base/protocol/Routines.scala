package shipreq.webapp.base.protocol

import scalaz.NonEmptyList
import upickle.{Reader, Writer}
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta.RemoteDelta
import Codec._, DataCodecs._, DeltaCodecs._
import Routine._

trait Crudable {
  type Id
  type V
}

trait CrudableAux[_I, _V] extends Crudable {
  override final type Id = _I
  override final type V = _V
}

abstract class CrudableCompanion[C <: Crudable](implicit RI: Reader[C#Id], WI: Writer[C#Id], RV: Reader[C#V], WV: Writer[C#V])
    extends DescT[CrudAction[C], RemoteDelta] {

  implicit def rwd = remoteRoutine(this)

  final type Action = CrudAction[C]
  @inline final def create(v: C#V)                      = CrudAction.Create[C](v)
  @inline final def update(id: C#Id, v: C#V)            = CrudAction.Update[C](id, v)
  @inline final def delete(id: C#Id, a: DeletionAction) = CrudAction.Delete[C](id, a)
}

sealed trait CrudAction[C <: Crudable]
object CrudAction {
  case class Create[C <: Crudable](newValues: C#V) extends CrudAction[C]
  case class Update[C <: Crudable](id: C#Id, newValues: C#V) extends CrudAction[C]
  case class Delete[C <: Crudable](id: C#Id, action: DeletionAction) extends CrudAction[C]
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

  sealed trait CustomIncmpTypeCrud extends CrudableAux[CustomIncmpType.Id, (RefKey, Option[String])]
  object CustomIncmpTypeCrud extends CrudableCompanion[CustomIncmpTypeCrud]

  sealed trait CustomReqTypeCrud extends CrudableAux[CustomReqType.Id, (ReqType.Mnemonic, String, ImplicationRequired)]
  object CustomReqTypeCrud extends CrudableCompanion[CustomReqTypeCrud]

  object CustomReqTypeImpUpd extends DescT[(CustomReqType.Id, ImplicationRequired), RemoteDelta]

  // TODO rename ForCfgReqType
  case class ForCfgReqType(projectInit: ProjectInit.Remote,
                           incmpCrud: CustomIncmpTypeCrud.Remote,
                           reqCrud: CustomReqTypeCrud.Remote
                            )
//  , reqImpReq: CustomReqTypeImpUpd.Remote)
    extends Group
}
