package shipreq.webapp.member.project.protocol.binary.v1

import boopickle.DefaultBasic._
import scala.reflect.ClassTag
import shipreq.base.util.{Digraph, SetDiff}
import shipreq.webapp.base.data.Username
import shipreq.webapp.member.social._

object Social {
  import shipreq.webapp.base.protocol.binary.v1.BaseData.{
    pickleDigraphBiDir,
    pickleMultimap,
    pickleNES,
    pickleObfuscated,
    picklerUsername,
    pickleSetDiff,
  }

  implicit val picklerUserGroupId: Pickler[UserGroup.Id.Public] =
    pickleObfuscated

  implicit val picklerUserGroupName: Pickler[UserGroup.Name] =
    new Pickler[UserGroup.Name] {
      override def pickle(a: UserGroup.Name)(implicit state: PickleState): Unit = {
        state.pickle(a.value)
      }
      override def unpickle(implicit state: UnpickleState): UserGroup.Name = {
        val value = state.unpickle[String]
        UserGroup.Name(value)
      }
    }

  implicit val picklerUserGroupHandle: Pickler[UserGroup.Handle] =
    new Pickler[UserGroup.Handle] {
      override def pickle(a: UserGroup.Handle)(implicit state: PickleState): Unit = {
        state.pickle(a.value)
      }
      override def unpickle(implicit state: UnpickleState): UserGroup.Handle = {
        val value = state.unpickle[String]
        UserGroup.Handle(value)
      }
    }

  implicit val picklerUserGroupPerm: Pickler[UserGroup.Perm] =
    new Pickler[UserGroup.Perm] {
      private[this] final val KeyMember = 0
      private[this] final val KeyAdmin  = 1
      override def pickle(a: UserGroup.Perm)(implicit state: PickleState): Unit =
        a match {
          case UserGroup.Perm.Admin  => state.enc.writeByte(KeyAdmin )
          case UserGroup.Perm.Member => state.enc.writeByte(KeyMember)
        }
      override def unpickle(implicit state: UnpickleState): UserGroup.Perm =
        state.dec.readByte match {
          case KeyAdmin  => UserGroup.Perm.Admin
          case KeyMember => UserGroup.Perm.Member
        }
    }

  private def pickleUserGroupARel[A: Pickler]: Pickler[UserGroup.ARel[A]] =
    new Pickler[UserGroup.ARel[A]] {
      override def pickle(a: UserGroup.ARel[A])(implicit state: PickleState): Unit = {
        state.pickle(a.to)
        state.pickle(a.perm)
      }
      override def unpickle(implicit state: UnpickleState): UserGroup.ARel[A] = {
        val to   = state.unpickle[A]
        val perm = state.unpickle[UserGroup.Perm]
        UserGroup.ARel(to, perm)
      }
    }

  implicit val picklerUserGroupARelUserGroupIdPublic: Pickler[UserGroup.ARel[UserGroup.Id.Public]] =
    pickleUserGroupARel

  implicit val picklerUserGroupARelUsername: Pickler[UserGroup.ARel[Username]] =
    pickleUserGroupARel

  implicit val picklerSetDiffUserGroupARelUserGroupIdPublic: Pickler[SetDiff[UserGroup.ARel[UserGroup.Id.Public]]] =
    pickleSetDiff

  implicit val picklerSetDiffUserGroupARelUsername: Pickler[SetDiff[UserGroup.ARel[Username]]] =
    pickleSetDiff

  private def pickleUserGroupARels[F[_], G, U](implicit g: Pickler[F[UserGroup.ARel[G]]], u: Pickler[F[UserGroup.ARel[U]]]): Pickler[UserGroup.ARels[F, G, U]] =
    new Pickler[UserGroup.ARels[F, G, U]] {
      override def pickle(a: UserGroup.ARels[F, G, U])(implicit state: PickleState): Unit = {
        state.pickle(a.parents)
        state.pickle(a.children)
        state.pickle(a.users)
      }
      override def unpickle(implicit state: UnpickleState): UserGroup.ARels[F, G, U] = {
        val parents  = state.unpickle[F[UserGroup.ARel[G]]]
        val children = state.unpickle[F[UserGroup.ARel[G]]]
        val users    = state.unpickle[F[UserGroup.ARel[U]]]
        UserGroup.ARels(parents, children, users)
      }
    }

  implicit val picklerUserGroupARelsSet: Pickler[UserGroup.ARels[Set, UserGroup.Id.Public, Username]] =
    pickleUserGroupARels

  implicit val picklerUserGroupARelsSetDiff: Pickler[UserGroup.ARels[SetDiff, UserGroup.Id.Public, Username]] =
    pickleUserGroupARels

  private implicit val picklerUserGroupValidationErrorGraphCycle: Pickler[UserGroup.ValidationError.GraphCycle[UserGroup.Id.Public]] =
    new Pickler[UserGroup.ValidationError.GraphCycle[UserGroup.Id.Public]] {
      override def pickle(a: UserGroup.ValidationError.GraphCycle[UserGroup.Id.Public])(implicit state: PickleState): Unit = {
        state.pickle(a.from)
        state.pickle(a.to)
      }
      override def unpickle(implicit state: UnpickleState): UserGroup.ValidationError.GraphCycle[UserGroup.Id.Public] = {
        val from = state.unpickle[UserGroup.Id.Public]
        val to   = state.unpickle[UserGroup.Id.Public]
        UserGroup.ValidationError.GraphCycle(from, to)
      }
    }

  private implicit val picklerUserGroupValidationErrorNoAdminUsers: Pickler[UserGroup.ValidationError.NoAdminUsers[UserGroup.Id.Public]] =
    new Pickler[UserGroup.ValidationError.NoAdminUsers[UserGroup.Id.Public]] {
      override def pickle(a: UserGroup.ValidationError.NoAdminUsers[UserGroup.Id.Public])(implicit state: PickleState): Unit = {
        state.pickle(a.group)
      }
      override def unpickle(implicit state: UnpickleState): UserGroup.ValidationError.NoAdminUsers[UserGroup.Id.Public] = {
        val group = state.unpickle[UserGroup.Id.Public]
        UserGroup.ValidationError.NoAdminUsers(group)
      }
    }

  private implicit val picklerUserGroupValidationErrorGroupNotFound: Pickler[UserGroup.ValidationError.GroupNotFound[UserGroup.Id.Public]] =
    new Pickler[UserGroup.ValidationError.GroupNotFound[UserGroup.Id.Public]] {
      override def pickle(a: UserGroup.ValidationError.GroupNotFound[UserGroup.Id.Public])(implicit state: PickleState): Unit = {
        state.pickle(a.group)
      }
      override def unpickle(implicit state: UnpickleState): UserGroup.ValidationError.GroupNotFound[UserGroup.Id.Public] = {
        val group = state.unpickle[UserGroup.Id.Public]
        UserGroup.ValidationError.GroupNotFound(group)
      }
    }

  implicit val picklerUserGroupValidationError: Pickler[UserGroup.ValidationError[UserGroup.Id.Public]] =
    new Pickler[UserGroup.ValidationError[UserGroup.Id.Public]] {
      private[this] final val KeyGraphCycle    = 0
      private[this] final val KeyNoAdminUsers  = 1
      private[this] final val KeyGroupNotFound = 2
      override def pickle(a: UserGroup.ValidationError[UserGroup.Id.Public])(implicit state: PickleState): Unit =
        a match {
          case b: UserGroup.ValidationError.GraphCycle   [UserGroup.Id.Public] => state.enc.writeByte(KeyGraphCycle   ); state.pickle(b)
          case b: UserGroup.ValidationError.NoAdminUsers [UserGroup.Id.Public] => state.enc.writeByte(KeyNoAdminUsers ); state.pickle(b)
          case b: UserGroup.ValidationError.GroupNotFound[UserGroup.Id.Public] => state.enc.writeByte(KeyGroupNotFound); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): UserGroup.ValidationError[UserGroup.Id.Public] =
        state.dec.readByte match {
          case KeyGraphCycle    => state.unpickle[UserGroup.ValidationError.GraphCycle   [UserGroup.Id.Public]]
          case KeyNoAdminUsers  => state.unpickle[UserGroup.ValidationError.NoAdminUsers [UserGroup.Id.Public]]
          case KeyGroupNotFound => state.unpickle[UserGroup.ValidationError.GroupNotFound[UserGroup.Id.Public]]
        }
    }

  implicit val picklerNonEmptySetUserGroupValidationError: Pickler[NonEmptySet[UserGroup.ValidationError[UserGroup.Id.Public]]] =
    pickleNES

  private def pickleUserGroupSaveErrorInvalid[GI](implicit p: Pickler[NonEmptySet[UserGroup.ValidationError[GI]]]): Pickler[UserGroup.SaveError.Invalid[GI]] =
    new Pickler[UserGroup.SaveError.Invalid[GI]] {
      override def pickle(a: UserGroup.SaveError.Invalid[GI])(implicit state: PickleState): Unit = {
        state.pickle(a.errors)
      }
      override def unpickle(implicit state: UnpickleState): UserGroup.SaveError.Invalid[GI] = {
        val errors = state.unpickle[NonEmptySet[UserGroup.ValidationError[GI]]]
        UserGroup.SaveError.Invalid(errors)
      }
    }

  private def pickleUserGroupSaveError[GI](implicit p1: Pickler[UserGroup.SaveError.Invalid[GI]]): Pickler[UserGroup.SaveError[GI]] =
    new Pickler[UserGroup.SaveError[GI]] {
      private[this] final val KeyHandleAlreadyTaken = 1
      private[this] final val KeyInvalid            = 0
      override def pickle(a: UserGroup.SaveError[GI])(implicit state: PickleState): Unit =
        a match {
          case UserGroup.SaveError.HandleAlreadyTaken    => state.enc.writeByte(KeyHandleAlreadyTaken)
          case b: UserGroup.SaveError.Invalid[GI]        => state.enc.writeByte(KeyInvalid           ); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): UserGroup.SaveError[GI] =
        state.dec.readByte match {
          case KeyHandleAlreadyTaken => UserGroup.SaveError.HandleAlreadyTaken
          case KeyInvalid            => state.unpickle[UserGroup.SaveError.Invalid[GI]]
        }
    }

  implicit val picklerUserGroupSaveErrorId: Pickler[UserGroup.SaveError[UserGroup.Id.Public]] = {
    implicit val a = pickleUserGroupSaveErrorInvalid[UserGroup.Id.Public]
    pickleUserGroupSaveError
  }

  def pickleUserGroupUniverse[UI: UnivEq: Pickler, U: Pickler, GI: ClassTag: UnivEq: Pickler, G: Pickler]: Pickler[UserGroup.Universe[UI, U, GI, G]] = {
    implicit val giGraph = pickleDigraphBiDir[GI]
    implicit val giToUis = pickleMultimap[GI, Set, UI]
    new Pickler[UserGroup.Universe[UI, U, GI, G]] {
      override def pickle(a: UserGroup.Universe[UI, U, GI, G])(implicit state: PickleState): Unit = {
        state.pickle(a.groupGraphMap)
        state.pickle(a.groupsToUsersMap)
        state.pickle(a.groups)
        state.pickle(a.users)
      }
      override def unpickle(implicit state: UnpickleState): UserGroup.Universe[UI, U, GI, G] = {
        val groupGraphMap    = state.unpickle[Map[UserGroup.Perm, Digraph.BiDir[GI]]]
        val groupsToUsersMap = state.unpickle[Map[UserGroup.Perm, Multimap[GI, Set, UI]]]
        val groups           = state.unpickle[Map[GI, G]]
        val users            = state.unpickle[Map[UI, U]]
        UserGroup.Universe(groupGraphMap, groupsToUsersMap, groups, users)
      }
    }
  }

def pickleUserGroup[Id: Pickler]: Pickler[UserGroup[Id]] =
  new Pickler[UserGroup[Id]] {
    override def pickle(a: UserGroup[Id])(implicit state: PickleState): Unit = {
      state.pickle(a.id)
      state.pickle(a.name)
      state.pickle(a.handle)
    }
    override def unpickle(implicit state: UnpickleState): UserGroup[Id] = {
      val id     = state.unpickle[Id]
      val name   = state.unpickle[UserGroup.Name]
      val handle = state.unpickle[UserGroup.Handle]
      UserGroup(id, name, handle)
    }
  }

}
