package shipreq.webapp.base.protocol

import boopickle._
import scalaz.\/
import shipreq.base.util.univeq._
import shipreq.base.util.ErrorMsg
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

  implicit def pickler[I, V](implicit pi: Pickler[I], pv: Pickler[V]): Pickler[CrudAction[I, V]] = {
    implicit val create : Pickler[Create [I, V]] = pickleCaseClass
    implicit val update : Pickler[Update [I, V]] = pickleCaseClass
    implicit val delete : Pickler[Delete [I, V]] = pickleCaseClass
    implicit val restore: Pickler[Restore[I, V]] = pickleCaseClass
    pickleADT
  }

  trait Types[I, V] {
    final type Id     = I
    final type Value  = V
    final type Action = CrudAction[Id, Value]
    final def create (v: Value)        : Action = CrudAction.Create [Id, Value](v)
    final def update (id: Id, v: Value): Action = CrudAction.Update [Id, Value](id, v)
    final def delete (id: Id)          : Action = CrudAction.Delete [Id, Value](id)
    final def restore(id: Id)          : Action = CrudAction.Restore[Id, Value](id)
  }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

@deprecated
final class CrudProtocol[I: Pickler, V: Pickler](name: String)
    extends ServerSideProc.Protocol[CrudAction[I, V], ErrorMsg \/ VerifiedEvent.Seq](name)(
      CrudAction.pickler[I, V],
      pickleErrorMsgOrVerifiedEventSeq) {

  type Id     = I
  type Value  = V
  type Action = CrudAction[Id, Value]

  def create (v: Value)        : Action = CrudAction.Create [Id, Value](v)
  def update (id: Id, v: Value): Action = CrudAction.Update [Id, Value](id, v)
  def delete (id: Id)          : Action = CrudAction.Delete [Id, Value](id)
  def restore(id: Id)          : Action = CrudAction.Restore[Id, Value](id)
}

object CrudProtocol {
  private[protocol] def apply[I: Pickler, V: Pickler](name: String): CrudProtocol[I, V] =
    new CrudProtocol(name)
}
