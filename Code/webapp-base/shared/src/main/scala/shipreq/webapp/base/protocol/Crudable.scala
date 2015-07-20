package shipreq.webapp.base.protocol

import boopickle.Pickler
import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.base.delta.RemoteDelta
import Routine._

trait Crudable extends Desc {
  type Id
  type V
  override final type I  = CrudAction[Id, V]
  override final type O  = RemoteDelta

  final type Action = CrudAction[Id, V]
  @inline final def create(v: V)                     : Action = CrudAction.Create[Id, V](v)
  @inline final def update(id: Id, v: V)             : Action = CrudAction.Update[Id, V](id, v)
  @inline final def delete(id: Id, a: DeletionAction): Action = CrudAction.Delete[Id, V](id, a)
}

object Crudable {
  type Aux[_I, _V] = Crudable { type Id = _I; type V = _V }

  class CAux[_Id, _V] private[protocol](implicit PI: Pickler[_Id], PV: Pickler[_V], PO: Pickler[RemoteDelta]) extends Crudable {
    override final type Id = _Id
    override final type V  = _V

    override implicit final def pi: Pickler[I] = ProtocolDataCodecs.pickleCrudAction[_Id, _V]
    override implicit final def po: Pickler[O] = PO
  }
}

sealed trait CrudAction[Id, V]
object CrudAction {
  final case class Create[Id, V](newValues: V)                   extends CrudAction[Id, V]
  final case class Update[Id, V](id: Id, newValues: V)           extends CrudAction[Id, V]
  final case class Delete[Id, V](id: Id, action: DeletionAction) extends CrudAction[Id, V]

  @inline implicit def equality[I: UnivEq, V: UnivEq]: UnivEq[CrudAction[I, V]] = UnivEq.force
}

sealed abstract class DeletionAction
object DeletionAction {
  case object HardDel extends DeletionAction
  case object SoftDel extends DeletionAction
  case object Restore extends DeletionAction
  def values = NonEmptyVector[DeletionAction](HardDel, SoftDel, Restore)
  @inline implicit def equality: UnivEq[DeletionAction] = UnivEq.force
}
