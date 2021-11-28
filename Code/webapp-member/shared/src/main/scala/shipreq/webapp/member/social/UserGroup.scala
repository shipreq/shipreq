package shipreq.webapp.member.social

import cats.Functor
import japgolly.microlibs.adt_macros.AdtMacros
import scala.reflect.ClassTag
import shipreq.base.util._
import shipreq.webapp.base.data.Live
import shipreq.webapp.base.util.Obfuscated

final case class UserGroup[+Id](id    : Id,
                                name  : UserGroup.Name,
                                handle: UserGroup.Handle,
                                live  : Live)

object UserGroup {

  final case class Id(value: Long)

  object Id {
    /** The real UserGroup.Id is never directly exposed to users. Publicly it has a different ID. */
    type Public = Obfuscated[Id]
  }


  final case class Name(value: String)

  /** Like a Username for the [[UserGroup]]. Eg `@@blah` */
  final case class Handle(value: String)

  sealed trait Perm
  object Perm {
    case object Admin  extends Perm
    case object Member extends Perm

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
  }

  object ARels {
    def emptySet[G, U]: ARels[Set, G, U] =
      new ARels(Set.empty, Set.empty, Set.empty)
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
        .flatMap(groupGraph(_).forwards.kvIterator.flatMap(x => x._1 :: x._2 :: Nil))
        .find(!groups.contains(_))
        .map(i => s"Group #$i found in graph but not groups map.")
    }

    assert {
      Perm.values.iterator
        .flatMap(groupsToUsers(_).keyIterator)
        .find(!groups.contains(_))
        .map(i => s"Group #$i found in groupsToUsers but not groups map.")
    }

    assert {
      Perm.values.iterator
        .flatMap(groupsToUsers(_).valueIterator)
        .find(!users.contains(_))
        .map(i => s"User #$i found in groupsToUsers but not users map.")
    }

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
        for {
          p <- Perm.values
          c <- cycleDetector.findCycle(groupGraph(p).forwards.m)
        } result += ValidationError.GraphCycle(c._1, c._2, p)
      }
      val acyclic = result.isEmpty

      // ensure user groups always have a valid owner
      if (acyclic) {
        val adminGroups = groupGraph(Perm.Admin).transitiveClosure(Backwards, groups.keysIterator)
        val adminUsers  = groupsToUsers(Perm.Admin)
        for (gi <- groups.keys)
          if (adminGroups(gi).forall(adminUsers(_).isEmpty))
            result += ValidationError.NoAdminUsers(gi)
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
  }

  sealed trait ValidationError[+GI]
  object ValidationError {
    final case class GraphCycle  [+GI](from: GI, to: GI, perm: Perm) extends ValidationError[GI]
    final case class NoAdminUsers[+GI](group: GI)                    extends ValidationError[GI]

    implicit def univEq[A: UnivEq]: UnivEq[ValidationError[A]] = UnivEq.derive
  }
}
