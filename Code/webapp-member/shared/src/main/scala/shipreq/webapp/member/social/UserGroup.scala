package shipreq.webapp.member.social

import cats.Functor
import japgolly.microlibs.adt_macros.AdtMacros
import scala.reflect.ClassTag
import shipreq.base.util._
import shipreq.webapp.base.util.Obfuscated

final case class UserGroup[+Id](id    : Id,
                                name  : UserGroup.Name,
                                handle: UserGroup.Handle)

object UserGroup {

  final case class Id(value: Long)

  object Id {
    /** The real UserGroup.Id is never directly exposed to users. Publicly it has a different ID. */
    type Public = Obfuscated[Id]
  }

  final case class Name(value: String)

  /** Like a Username for the [[UserGroup]]. Eg `@@blah` */
  final case class Handle(value: String)

  sealed abstract class Perm(final val ord: Int)
  object Perm {
    case object Admin  extends Perm(0)
    case object Member extends Perm(1)

    val values = AdtMacros.adtValues[Perm]
  }

  final case class Rel[+A, +B](from: A, to: B, perm: Perm)  {
    def fromTo               : (A, B)    = (from, to)
    def mapFrom[C](f: A => C): Rel[C, B] = Rel(f(from), to, perm)
    def mapTo  [C](f: B => C): Rel[A, C] = Rel(from, f(to), perm)
  }

  /** ARel for Anonymous Rel because the from field is omitted. */
  final case class ARel[+A](to: A, perm: Perm) {
    def map    [B](f: A => B): ARel[B]   = ARel(f(to), perm)
    def from   [B](from: B)  : Rel[B, A] = Rel(from, to, perm)
    def reverse[B](newTo: B) : Rel[A, B] = Rel(to, newTo, perm)
  }

  final case class ARels[F[_], G, U](parents : F[ARel[G]],
                                     children: F[ARel[G]],
                                     users   : F[ARel[U]]) {

    def xmap[G2, U2](g: G => G2, u: U => U2)(implicit F: Functor[F]): ARels[F, G2, U2] =
      ARels(
        parents  = F.map(parents )(_.map(g)),
        children = F.map(children)(_.map(g)),
        users    = F.map(users   )(_.map(u)),
      )

    def modParents (f: F[ARel[G]] => F[ARel[G]]): ARels[F, G, U] = copy(parents  = f(parents ))
    def modChildren(f: F[ARel[G]] => F[ARel[G]]): ARels[F, G, U] = copy(children = f(children))
    def modUsers   (f: F[ARel[U]] => F[ARel[U]]): ARels[F, G, U] = copy(users    = f(users   ))

    def fullParents (g: G)(implicit F: Functor[F]): F[Rel[G, G]] = F.map(parents )(_.reverse(g))
    def fullChildren(g: G)(implicit F: Functor[F]): F[Rel[G, G]] = F.map(children)(_.from(g))
    def fullUsers   (g: G)(implicit F: Functor[F]): F[Rel[G, U]] = F.map(users   )(_.from(g))

    def fullTreeRelsIterator(g: G)(f: F[ARel[G]] => Iterator[ARel[G]]): Iterator[Rel[G, G]] =
      f(parents).map(_.reverse(g)) ++ f(children).map(_.from(g))

    def fullUserRelsIterator(g: G)(f: F[ARel[U]] => Iterator[ARel[U]]): Iterator[Rel[G, U]] =
      f(users).map(_.from(g))
  }

  object ARels {
    def emptySet[G: UnivEq, U: UnivEq]: ARels[Set, G, U] =
      new ARels(Set.empty, Set.empty, Set.empty)

    def emptySetDiff[G: UnivEq, U: UnivEq]: ARels[SetDiff, G, U] =
      new ARels(SetDiff.empty, SetDiff.empty, SetDiff.empty)
  }

  @inline implicit def univEq      [A: UnivEq]           : UnivEq[UserGroup[A]] = UnivEq.derive
  @inline implicit def univEqHandle                      : UnivEq[Handle      ] = UnivEq.derive
  @inline implicit def univEqId                          : UnivEq[Id          ] = UnivEq.derive
  @inline implicit def univEqName                        : UnivEq[Name        ] = UnivEq.derive
  @inline implicit def univEqPerm                        : UnivEq[Perm        ] = UnivEq.derive
  @inline implicit def univEqRel   [A: UnivEq, B: UnivEq]: UnivEq[Rel[A, B]   ] = UnivEq.derive
  @inline implicit def univEqARel  [A: UnivEq]           : UnivEq[ARel[A]     ] = UnivEq.derive

  @nowarn("cat=unused")
  @inline
  implicit def univEqARels[F[_], G, U](implicit g: UnivEq[F[ARel[G]]], u: UnivEq[F[ARel[U]]]): UnivEq[ARels[F, G, U]] =
    UnivEq.derive

  // ===================================================================================================================

  /**
    * "X ->ₚ Y" means "Y is a P of X" (because X pulls in Y as its P)
    *
    * @tparam UI user id
    * @tparam U  user
    * @tparam GI user group id
    * @tparam G  user group
    */
  final case class Universe[UI: UnivEq, U, GI: ClassTag: UnivEq, G](
        groupGraphMap   : Map[Perm, Digraph.BiDir[GI]],
        groupsToUsersMap: Map[Perm, Multimap[GI, Set, UI]],
        groups          : Map[GI, G],
        users           : Map[UI, U],
      ) {

    assert {
      Perm.values.iterator
        .flatMap(groupsToUsers(_).valueIterator)
        .find(!users.contains(_))
        .map(i => s"User #$i found in groupsToUsers but not users map.")
    }

    def isEmpty =
      groups.isEmpty && users.isEmpty

    @elidable(elidable.ASSERTION)
    override def toString = {
      def fmtMap[A, B](m: Map[A, B]): String =
        m.iterator
          .map { case (k, v) => s"$k → $v"}
          .toArray
          .sortInPlace()
          .mkString("Map(", ", ", ")")

      s"""|UserGroup.Universe(
          |  groupGraph    = ${fmtMap(groupGraphMap)},
          |  groupsToUsers = ${fmtMap(groupsToUsersMap)},
          |  groups        = ${fmtMap(groups)},
          |  users         = ${fmtMap(users)},
          |)
          |""".stripMargin
    }

    def groupGraph(p: Perm): Digraph.BiDir[GI] =
      groupGraphMap.getOrElse(p, Digraph.emptyBiDir[GI])

    def groupsToUsers(p: Perm): Multimap[GI, Set, UI] =
      groupsToUsersMap.getOrElse(p, Multimap.empty[GI, Set, UI])

    /** @param checkForCycles Validates that there are no cycles in the user group graph. The reason that this is
                              optional is that there's a DB trigger that prevents cycles, and by disabling this
                              this check on the server, we avoid unnecessary work.
      */
    def validate(checkForCycles: Boolean): Set[ValidationError[GI]] = {
      var result = Set.empty[ValidationError[GI]]

      // no group cycles
      if (checkForCycles) {
        val cycleDetector = Digraph.cycleDetector[GI]
        val graph = Perm.values.iterator.map(groupGraph(_).forwards).reduce(_ ++ _.m)
        for (c <- cycleDetector.findCycle(graph.m))
          result += ValidationError.GraphCycle(c._1, c._2)
      }
      val acyclic = result.isEmpty

      // ensure user groups always have a valid owner
      if (acyclic) {
        val adminGroups = groupGraph(Perm.Admin).transitiveClosure(Backwards, groups.keysIterator)
        val adminUsers  = groupsToUsers(Perm.Admin)
        for (id <- groups.keys)
          if (adminGroups(id).forall(adminUsers(_).isEmpty))
            result += ValidationError.NoAdminUsers(id)
      }

      // ensure all group ids have a corresponding group
      {
        val validateGroupId: GI => Unit = id =>
          if (!groups.contains(id))
            result += ValidationError.GroupNotFound(id)

        for (p <- Perm.values) {
          for (e <- groupGraph(p).forwards.m) {
            validateGroupId(e._1)
            e._2.foreach(validateGroupId)
          }

          groupsToUsers(p).keys.foreach(validateGroupId)
        }
      }

      result
    }

    def allUsersInGroup(groupdId: GI, perm: Perm): Set[UI] = {
      @tailrec
      def go(id: GI, q: Set[GI], seen: Set[GI], r: Set[UI]): Set[UI] = {
        if (seen.contains(id)) {
          if (q.isEmpty) r else go(q.head, q.tail, seen, r)
        } else {
          val r2 = Util.mergeSets(r, groupsToUsers(perm)(id))
          val q2 = Util.mergeSets(q, groupGraph(perm).forwards(id))
          if (q2.isEmpty) r2 else go(q2.head, q2.tail, seen + id, r2)
        }
      }
      go(groupdId, Set.empty, Set.empty, Set.empty)
    }
  }

  object Universe {
    implicit def univEq[UI: UnivEq, U: UnivEq, GI: UnivEq, G: UnivEq]: UnivEq[Universe[UI, U, GI, G]] = UnivEq.derive

    def empty[UI: UnivEq, U, GI: ClassTag: UnivEq, G]: Universe[UI, U, GI, G] =
      apply(Map.empty, Map.empty, Map.empty, Map.empty)

    def fromRels[UI: UnivEq, GI: ClassTag: UnivEq](groupRels : Iterable[Rel[GI, GI]],
                                                   groupUsers: Iterable[Rel[GI, UI]]): Universe[UI, Unit, GI, Unit] = {

      val groups = Map.newBuilder[GI, Unit]
      val users = Map.newBuilder[UI, Unit]
      for (r <- groupRels)
        groups += ((r.from, ())) += ((r.to, ()))
      for (r <- groupUsers) {
        groups += ((r.from, ()))
        users += ((r.to, ()))
      }
      fromRels(groupRels, groupUsers, groups.result(), users.result())
    }

    def fromRels[UI: UnivEq, U, GI: ClassTag: UnivEq, G](groupRels : Iterable[Rel[GI, GI]],
                                                         groupUsers: Iterable[Rel[GI, UI]],
                                                         groups    : Map[GI, G],
                                                         users     : Map[UI, U]): Universe[UI, U, GI, G] = {

      def permMap[A, B](rels: Iterable[Rel[GI, A]])(f: Multimap[GI, Set, A] => B): Map[Perm, B] = {
        val empty = Multimap.empty[GI, Set, A]
        val as = Array.fill(Perm.values.length)(empty)
        for (r <- rels) {
          val i = r.perm.ord
          as(i) = as(i).add(r.from, r.to)
        }
        Perm.values.iterator
          .filter(p => as(p.ord) ne empty)
          .map(p => (p, f(as(p.ord))))
          .toMap
      }

      apply[UI, U, GI, G](
        permMap(groupRels)(Digraph.BiDir(_)),
        permMap(groupUsers)(identity),
        groups,
        users
      )
    }
  }

  sealed trait ValidationError[+GI] {
    def map[A](f: GI => A): ValidationError[A]
  }

  object ValidationError {
    final case class GraphCycle[+GI](from: GI, to: GI) extends ValidationError[GI] {
      override def map[A](f: GI => A): GraphCycle[A] = GraphCycle(f(from), f(to))
    }

    final case class NoAdminUsers[+GI](group: GI) extends ValidationError[GI] {
      override def map[A](f: GI => A): NoAdminUsers[A] = NoAdminUsers(f(group))
    }

    final case class GroupNotFound[+GI](group: GI) extends ValidationError[GI] {
      override def map[A](f: GI => A): GroupNotFound[A] = GroupNotFound(f(group))
    }

    implicit def univEq[A: UnivEq]: UnivEq[ValidationError[A]] = UnivEq.derive
  }

  sealed trait SaveError[+GI] {
    def map[A: UnivEq](f: GI => A): SaveError[A]
  }

  object SaveError {
    case object HandleAlreadyTaken extends SaveError[Nothing] {
      override def map[A: UnivEq](f: Nothing => A) = this
    }

    final case class Invalid[GI](errors: NonEmptySet[UserGroup.ValidationError[GI]]) extends SaveError[GI] {
      override def map[A: UnivEq](f: GI => A): Invalid[A] = Invalid(errors.map(_.map(f)))
    }

    implicit def univEq[A: UnivEq]: UnivEq[SaveError[A]] = UnivEq.derive
  }
}
