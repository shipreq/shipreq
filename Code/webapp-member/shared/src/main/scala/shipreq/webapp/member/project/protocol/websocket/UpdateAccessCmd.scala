package shipreq.webapp.member.project.protocol.websocket

import cats.Monad
import cats.syntax.all._
import shipreq.webapp.base.data._

sealed trait UpdateAccessCmd

object UpdateAccessCmd {

  final case class Add(user: Username \/ EmailAddr, role: ProjectRole) extends UpdateAccessCmd
  final case class Modify(updates: Map[UserId.Public, Option[ProjectRole]]) extends UpdateAccessCmd
  final case object RemoveSelf extends UpdateAccessCmd

  implicit def univEq: UnivEq[UpdateAccessCmd] = UnivEq.derive

  /** @param modify The ProjectRole argument is what is required of the current user to make the change */
  def resolve[F[_], A](cmd       : UpdateAccessCmd)(
                       userId    : UserId.Public,
                       getUserId : (Username \/ EmailAddr) => F[Option[UserId.Public]],
                       onNotFound: => A,
                       modify    : (UpdateAccessCmd.Modify, ProjectRole) => F[A])(implicit F: Monad[F]): F[A] =
    cmd match {
      case a: Add =>
        getUserId(a.user).flatMap {
          case Some(u) => modify(Modify(Map(u -> Some(a.role))), ProjectRole.Admin)
          case None    => F.pure(onNotFound)
        }

      case RemoveSelf =>
        modify(Modify(Map(userId -> None)), ProjectRole.min)

      case m: Modify =>
        modify(m, ProjectRole.Admin)
    }

  object CodecsV1 {
    import boopickle.DefaultBasic._
    import shipreq.webapp.base.protocol.binary.v1.BaseData._
    import shipreq.webapp.member.project.protocol.binary.v2.Rev0._

    private implicit val picklerUpdateAccessCmdAdd: Pickler[Add] =
      new Pickler[Add] {
        override def pickle(a: Add)(implicit state: PickleState): Unit = {
          state.pickle(a.user)
          state.pickle(a.role)
        }
        override def unpickle(implicit state: UnpickleState): Add = {
          val user = state.unpickle[Username \/ EmailAddr]
          val role = state.unpickle[ProjectRole]
          Add(user, role)
        }
      }

    private implicit val picklerUpdateAccessCmdModify: Pickler[Modify] =
      pickleMap[UserId.Public, Option[ProjectRole]].xmap(Modify.apply)(_.updates)

    implicit val picklerUpdateAccessCmd: Pickler[UpdateAccessCmd] =
      new Pickler[UpdateAccessCmd] {
        private[this] final val KeyAdd        = 'a'
        private[this] final val KeyModify     = 'm'
        private[this] final val KeyRemoveSelf = 'r'
        override def pickle(a: UpdateAccessCmd)(implicit state: PickleState): Unit =
          a match {
            case b: Add     => state.enc.writeByte(KeyAdd       ); state.pickle(b)
            case b: Modify  => state.enc.writeByte(KeyModify    ); state.pickle(b)
            case RemoveSelf => state.enc.writeByte(KeyRemoveSelf)
          }
        override def unpickle(implicit state: UnpickleState): UpdateAccessCmd =
          state.dec.readByte match {
            case KeyAdd        => state.unpickle[Add   ]
            case KeyModify     => state.unpickle[Modify]
            case KeyRemoveSelf => RemoveSelf
          }
      }
  }
}
