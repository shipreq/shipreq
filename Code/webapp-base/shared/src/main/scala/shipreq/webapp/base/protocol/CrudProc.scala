package shipreq.webapp.base.protocol

import boopickle._
import shipreq.base.util.univeq._
import shipreq.webapp.base.event.VerifiedEvent
import BoopickleMacros._
import BinCodecEvents._

sealed trait CrudAction[Id, Value]

object CrudAction {
  final case class Create [Id, Value](newValues: Value)         extends CrudAction[Id, Value]
  final case class Update [Id, Value](id: Id, newValues: Value) extends CrudAction[Id, Value]
  final case class Delete [Id, Value](id: Id)                   extends CrudAction[Id, Value]
  final case class Restore[Id, Value](id: Id)                   extends CrudAction[Id, Value]

  @inline implicit def equality[I: UnivEq, V: UnivEq]: UnivEq[CrudAction[I, V]] = UnivEq.derive

  def pickler[I, V](implicit pi: Pickler[I], pv: Pickler[V]): Pickler[CrudAction[I, V]] = {
    implicit val create : Pickler[Create [I, V]] = pickleCaseClass
    implicit val update : Pickler[Update [I, V]] = pickleCaseClass
    implicit val delete : Pickler[Delete [I, V]] = pickleCaseClass
    implicit val restore: Pickler[Restore[I, V]] = pickleCaseClass
    pickleADT
  }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

sealed abstract class CrudProtocol extends ServerSideProc.Protocol {
  type Id
  type Value

  final override type Failure = ErrorMsg
  final override type Input   = CrudAction[Id, Value]
  final override type Output  = VerifiedEvent.Seq

  final override implicit val pickleOutput  : Pickler[Output]   = pickleVerifiedEventSeq
  final override implicit val pickleFailure : Pickler[Failure]  = ErrorMsg.pickleErrorMsg
  final override implicit val pickleResponse: Pickler[Response] = ServerSideProc.Protocol.pickleErrorMsgOrVerifiedEventSeq

  final type Action = CrudAction[Id, Value]
  final def create (v: Value)        : Action = CrudAction.Create [Id, Value](v)
  final def update (id: Id, v: Value): Action = CrudAction.Update [Id, Value](id, v)
  final def delete (id: Id)          : Action = CrudAction.Delete [Id, Value](id)
  final def restore(id: Id)          : Action = CrudAction.Restore[Id, Value](id)
}

object CrudProtocol {
  type Aux[I, V] = CrudProtocol { type Id = I; type Value = V }

  private[protocol] def apply[I, V](implicit pi: Pickler[I], pv: Pickler[V]): Aux[I, V] =
    new CrudProtocol {
      override type Id = I
      override type Value = V
      override implicit val pickleInput: Pickler[Input] = CrudAction.pickler[I, V]
    }

}
