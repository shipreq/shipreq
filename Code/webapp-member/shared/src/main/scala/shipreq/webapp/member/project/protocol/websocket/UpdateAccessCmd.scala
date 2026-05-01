package shipreq.webapp.member.project.protocol.websocket

import cats.Monad
import cats.syntax.all._
import shipreq.webapp.base.data._

sealed trait UpdateAccessCmd

object UpdateAccessCmd {

  final case class Add(user: Username \/ EmailAddr, perm: ProjectPerm) extends UpdateAccessCmd

  final case class Modify(updates: Map[UserId.Public, Option[ProjectPerm]]) extends UpdateAccessCmd

  implicit def univEq: UnivEq[UpdateAccessCmd] = UnivEq.derive

  def resolve[F[_], A](cmd       : UpdateAccessCmd)(
                       getUserId : (Username \/ EmailAddr) => F[Option[UserId.Public]],
                       onNotFound: => A,
                       modify    : UpdateAccessCmd.Modify => F[A])(implicit F: Monad[F]): F[A] =
    cmd match {
      case a: Add =>
        getUserId(a.user).flatMap {
          case Some(u) => modify(Modify(Map(u -> Some(a.perm))))
          case None    => F.pure(onNotFound)
        }
      case m: Modify =>
        modify(m)
    }

  object CodecsV1 {
    import boopickle.DefaultBasic._
    import shipreq.webapp.base.protocol.binary.v1.BaseData._
    import shipreq.webapp.member.project.protocol.binary.v2.Rev0._

    private implicit val picklerUpdateAccessCmdAdd: Pickler[Add] =
      new Pickler[Add] {
        override def pickle(a: Add)(implicit state: PickleState): Unit = {
          state.pickle(a.user)
          state.pickle(a.perm)
        }
        override def unpickle(implicit state: UnpickleState): Add = {
          val user = state.unpickle[Username \/ EmailAddr]
          val perm = state.unpickle[ProjectPerm]
          Add(user, perm)
        }
      }

    private implicit val picklerUpdateAccessCmdModify: Pickler[Modify] =
      pickleMap[UserId.Public, Option[ProjectPerm]].xmap(Modify.apply)(_.updates)

    implicit val picklerUpdateAccessCmd: Pickler[UpdateAccessCmd] =
      new Pickler[UpdateAccessCmd] {
        private[this] final val KeyAdd    = 'a'
        private[this] final val KeyModify = 'm'
        override def pickle(a: UpdateAccessCmd)(implicit state: PickleState): Unit =
          a match {
            case b: Add    => state.enc.writeByte(KeyAdd   ); state.pickle(b)
            case b: Modify => state.enc.writeByte(KeyModify); state.pickle(b)
          }
        override def unpickle(implicit state: UnpickleState): UpdateAccessCmd =
          state.dec.readByte match {
            case KeyAdd    => state.unpickle[Add   ]
            case KeyModify => state.unpickle[Modify]
          }
      }
  }
}
