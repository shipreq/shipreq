package shipreq.webapp.base.protocol

import boopickle.Pickler
import shipreq.base.util.univeq._
import shipreq.webapp.base.event.{VerifiedEvents, DeletionAction}
import boopickle._, BoopickleMacros._, BinCodecGeneric._, BinCodecData._, BinCodecEvents._

trait CrudFn extends RemoteFn {
  type Id
  type V

  final override type Input   = CrudAction[Id, V]
  final override type Output  = VerifiedEvents
  final override type Failure = GenericFailure

  final override implicit val pickleOutput  : Pickler[Output]   = pickleVerifiedEvents
  final override implicit val pickleFailure : Pickler[Failure]  = GenericFailure.pickleGenericFailure
  final override implicit val pickleResponse: Pickler[Response] = pickleXor(pickleFailure, pickleOutput)

  final type Action = CrudAction[Id, V]
  @inline final def create(v: V)                     : Action = CrudAction.Create[Id, V](v)
  @inline final def update(id: Id, v: V)             : Action = CrudAction.Update[Id, V](id, v)
  @inline final def delete(id: Id, a: DeletionAction): Action = CrudAction.Delete[Id, V](id, a)
}

object CrudFn {
  type Aux[_I, _V] = CrudFn { type Id = _I; type V = _V }

  class CAux[_Id, _V] private[protocol](implicit PI: Pickler[_Id], PV: Pickler[_V]) extends CrudFn {
    override final type Id = _Id
    override final type V  = _V

    override implicit final val pickleInput: Pickler[Input] =
      CrudAction.pickleCrudAction[_Id, _V]
  }
}

sealed trait CrudAction[Id, V]
object CrudAction {
  final case class Create[Id, V](newValues: V)                   extends CrudAction[Id, V]
  final case class Update[Id, V](id: Id, newValues: V)           extends CrudAction[Id, V]
  final case class Delete[Id, V](id: Id, action: DeletionAction) extends CrudAction[Id, V]

  @inline implicit def equality[I: UnivEq, V: UnivEq]: UnivEq[CrudAction[I, V]] = UnivEq.derive

  def pickleCrudAction[I, V](implicit PI: Pickler[I], PV: Pickler[V]): Pickler[CrudAction[I, V]] = {
    implicit val create: Pickler[Create[I, V]] = pickleCaseClass
    implicit val update: Pickler[Update[I, V]] = pickleCaseClass
    implicit val delete: Pickler[Delete[I, V]] = pickleCaseClass
    pickleADT
  }
}
