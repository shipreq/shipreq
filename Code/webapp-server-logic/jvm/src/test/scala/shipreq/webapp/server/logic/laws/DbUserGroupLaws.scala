package shipreq.webapp.server.logic.laws

import cats.implicits._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.util.UUID
import scala.util.Try
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.member.social.UserGroup.Perm._
import shipreq.webapp.member.social.UserGroup.{Handle, Id, Name, Perm, SaveError, Universe, ValidationError}
import shipreq.webapp.member.social._
import shipreq.webapp.member.test.project.UnsafeTypes._
import shipreq.webapp.server.logic.test.WebappServerLogicTestUtil._
import sourcecode.Line
import utest._

object DbUserGroupLaws {
  import UserGroup._
  import UserGroup.Perm._

  val uuid: () => Long = {
    var prev = System.nanoTime()
    () => {
      prev += 1
      prev
    }
  }

  def uuid(prefix: String): String =
    prefix + uuid()

  def newUniverse = new UniverseBuider(
    Map.empty[Perm, Digraph.UniDir[Id]].withDefaultValue(Digraph.emptyUniDir),
    Map.empty[Perm, Multimap[Id, Set, UserId]].withDefaultValue(Multimap.empty),
    Set.empty,
    Set.empty
  )

  final case class UniverseBuider(groupGraph   : Map[Perm, Digraph.UniDir[Id]],
                                  groupsToUsers: Map[Perm, Multimap[Id, Set, UserId]],
                                  groups       : Set[Id],
                                  users        : Set[UserId]) {

    import UniverseBuider._

    private def modGroupGraph(p: Perm)(f: Digraph.UniDir[Id] => Digraph.UniDir[Id]): Map[Perm, Digraph.UniDir[Id]] =
      groupGraph.updated(p, f(groupGraph(p)))

    private def modGroupsToUsers(p: Perm)(f: Multimap[Id, Set, UserId] => Multimap[Id, Set, UserId]): Map[Perm, Multimap[Id, Set, UserId]] =
      groupsToUsers.updated(p, f(groupsToUsers(p)))

    def users(g: Id, p: Perm)(us: UserId*): UniverseBuider = copy(
      groupsToUsers = modGroupsToUsers(p)(_.addvs(g, us.toSet)),
      groups        = groups + g,
      users         = users ++ us,
    )
    def admins (g: Id)(us: UserId*): UniverseBuider = users(g, Admin)(us: _*)
    def members(g: Id)(us: UserId*): UniverseBuider = users(g, Member)(us: _*)

    def children(g: Id, p: Perm)(cs: Id*): UniverseBuider = copy(
      groupGraph = modGroupGraph(p)(_.addvs(g, cs.toSet)),
      groups     = groups + g ++ cs,
    )
    def adminChildren (g: Id)(cs: Id*): UniverseBuider = children(g, Admin)(cs: _*)
    def memberChildren(g: Id)(cs: Id*): UniverseBuider = children(g, Member)(cs: _*)

    def parents(g: Id, p: Perm)(ps: Id*): UniverseBuider = copy(
      groupGraph = modGroupGraph(p)(_.addks(ps.toSet, g)),
      groups     = groups + g ++ ps,
    )
    def adminParents (g: Id)(ps: Id*): UniverseBuider = parents(g, Admin)(ps: _*)
    def memberParents(g: Id)(ps: Id*): UniverseBuider = parents(g, Member)(ps: _*)

    def result: UniverseU = {
      implicit def set[A](s: Set[A]): Map[A, Unit] = Map.empty[A, Unit] ++ s.iterator.map((_, ()))
      Universe(groupGraph.mapValuesNow(Digraph.BiDir(_)), groupsToUsers, groups, users)
    }

    def apply(g: Id)(f: Focus => Focus): UniverseBuider = {
      val f1 = new Focus(g, this)
      val f2 = f(f1)
      f2.result
    }
  }

  object UniverseBuider {
    final class Focus(g: Id, private[UniverseBuider] val result: UniverseBuider) {
      private def mod(f: UniverseBuider => UniverseBuider) = new Focus(g, f(result))
      def admins        (ids: UserId*) = mod(_.admins        (g)(ids: _*))
      def members       (ids: UserId*) = mod(_.members       (g)(ids: _*))
      def adminChildren (ids: Id*)     = mod(_.adminChildren (g)(ids: _*))
      def memberChildren(ids: Id*)     = mod(_.memberChildren(g)(ids: _*))
      def adminParents  (ids: Id*)     = mod(_.adminParents  (g)(ids: _*))
      def memberParents (ids: Id*)     = mod(_.memberParents (g)(ids: _*))
    }
  }

  type UniverseU = Universe[UserId, Unit, Id, Unit]

  type ARels = UserGroup.ARels[Set, Id, UserId]
  type ARelDiffs = UserGroup.ARels[SetDiff, Id, UserId]

  trait ARelsDsl[+A] {
    protected def mod(f: ARels => ARels): A
    def admins        (ids: UserId*): A = mod(_.modUsers(_ ++ ids.map(ARel(_, Admin))))
    def members       (ids: UserId*): A = mod(_.modUsers(_ ++ ids.map(ARel(_, Member))))
    def adminChildren (ids: Id*)    : A = mod(_.modChildren(_ ++ ids.map(ARel(_, Admin))))
    def memberChildren(ids: Id*)    : A = mod(_.modChildren(_ ++ ids.map(ARel(_, Member))))
    def adminParents  (ids: Id*)    : A = mod(_.modParents(_ ++ ids.map(ARel(_, Admin))))
    def memberParents (ids: Id*)    : A = mod(_.modParents(_ ++ ids.map(ARel(_, Member))))
  }

  trait ARelDiffsDsl[+A] {
    protected def mod(f: ARelDiffs => ARelDiffs): A

    def addAdmins        (ids: UserId*): A = mod(_.modUsers(_ ++ ids.map(ARel(_, Admin))))
    def addMembers       (ids: UserId*): A = mod(_.modUsers(_ ++ ids.map(ARel(_, Member))))
    def addAdminChildren (ids: Id*)    : A = mod(_.modChildren(_ ++ ids.map(ARel(_, Admin))))
    def addMemberChildren(ids: Id*)    : A = mod(_.modChildren(_ ++ ids.map(ARel(_, Member))))
    def addAdminParents  (ids: Id*)    : A = mod(_.modParents(_ ++ ids.map(ARel(_, Admin))))
    def addMemberParents (ids: Id*)    : A = mod(_.modParents(_ ++ ids.map(ARel(_, Member))))

    def delAdmins        (ids: UserId*): A = mod(_.modUsers(_ -- ids.map(ARel(_, Admin))))
    def delMembers       (ids: UserId*): A = mod(_.modUsers(_ -- ids.map(ARel(_, Member))))
    def delAdminChildren (ids: Id*)    : A = mod(_.modChildren(_ -- ids.map(ARel(_, Admin))))
    def delMemberChildren(ids: Id*)    : A = mod(_.modChildren(_ -- ids.map(ARel(_, Member))))
    def delAdminParents  (ids: Id*)    : A = mod(_.modParents(_ -- ids.map(ARel(_, Admin))))
    def delMemberParents (ids: Id*)    : A = mod(_.modParents(_ -- ids.map(ARel(_, Member))))
  }

}

// =====================================================================================================================

abstract class DbUserGroupLaws extends TestSuite {
  import DbUserGroupLaws._

  private type SaveError = UserGroup.SaveError[Id]

  protected def beforeTest(): Unit =
    ()

  protected def newDbApi[A](f: DbApi => A): A

  protected trait DbApi {
    def createUser(): User
    def createUserGroup: (Name, Handle, ARels) => SaveError \/ Id
    def updateUserGroup: (Id, Option[Name], Option[Handle], ARelDiffs) => SaveError \/ Unit
    def getUserGroupUniverseU: Id => UniverseU
    def getUserGroupUniverseForUser: UserId => Universe[UserId, Username, Id, UserGroup[Id]]
    def getUserIdsByUsername: Set[Username] => NonEmptySet[Username] \/ Map[Username, UserId]
  }

  // ===================================================================================================================

  private class CreateUserGroupDsl(rels: ARels)(implicit db: DbApi) extends ARelsDsl[CreateUserGroupDsl] {
    override protected def mod(f: ARels => ARels) = new CreateUserGroupDsl(f(rels))

    def create(handle: String = null) = {
      val id = uuid("ug")
      val h = Option(handle).getOrElse(id)
      db.createUserGroup(id, h, rels)
    }
  }

  private class UpdateUserGroupDsl(id: Id, rels: ARelDiffs)(implicit db: DbApi) extends ARelDiffsDsl[UpdateUserGroupDsl] {
    override protected def mod(f: ARelDiffs => ARelDiffs) = new UpdateUserGroupDsl(id, f(rels))

    def update(name: Name = null, handle: Handle = null) =
      db.updateUserGroup(id, Option(name), Option(handle), rels)
  }

  private final class Tester()(implicit val db: DbApi) {
    def createUser() = db.createUser()
    def createUserGroup = new CreateUserGroupDsl(UserGroup.ARels.emptySet)
    def updateUserGroup(id: Id) = new UpdateUserGroupDsl(id, UserGroup.ARels.emptySetDiff)

    def assertUniverse(g: Id, expect: UniverseU)(implicit q: Line): Unit = {
      val actual = db.getUserGroupUniverseU(g)
      if (actual !=* expect) {
        for (p <- Perm.values) {
          Try(assertMap(s"groupGraph($p)", actual.groupGraph(p).forwards.m, expect.groupGraph(p).forwards.m))
          Try(assertMap(s"groupsToUsers($p)", actual.groupsToUsers(p).m, expect.groupsToUsers(p).m))
        }
        Try(assertMap("groups", actual.groups, expect.groups))
        Try(assertMap("users", actual.users, expect.users))
        assertEq(actual, expect)
      }

      for {
        p <- Perm.values
        m <- actual.groupsToUsersMap.get(p)
        u <- m(g)
      } {
        def subset[A, B, E](m: Map[Perm, A], e: Map[Perm, E])(f: (A, E) => B): Map[Perm, B] =
          m.iterator.flatMap { case (p, a) => e.get(p).map(e => (p, f(a, e))) }.toMap

        val a = db.getUserGroupUniverseForUser(u)

        val actual2 = Universe[UserId, Unit, Id, Unit](
          groupGraphMap    = subset(a.groupGraphMap, expect.groupGraphMap)(_.asSubsetOf(_)),
          groupsToUsersMap = subset(a.groupsToUsersMap, expect.groupsToUsersMap)(_.asSubsetOf(_)),
          groups           = expect.groups.filter(x => a.groups.contains(x._1)),
          users            = expect.users.filter(x => a.users.contains(x._1)),
        )
        assertEq(s"getUserGroupUniverseForUser($u)", actual2, expect)
      }
    }

    def assertErrors(actual: SaveError)(expect: UserGroup.ValidationError[Unit]*)(implicit q: Line): Unit = {
      val a = actual.map(_ => ())
      val e = SaveError.Invalid(NonEmptySet.force(expect.toSet))
      assertEq(a, e)
    }
  }

  private implicit def autoUserId(u: User): UserId = u.id

  protected def test[A](f: (Tester, User) => A): A =
    newDbApi { implicit db =>
      val t = new Tester

      // Create some irrelevant noise
      val u = db.createUser()
      t.createUserGroup.admins(u).create()

      f(t, db.createUser())
    }

  // ===================================================================================================================

  /** group with a single admin user */
  private def createSole() = test { (t, u) =>
    val g      = t.createUserGroup.admins(u).create().getOrThrow()
    val expect = newUniverse.admins(g)(u).result
    t.assertUniverse(g, expect)
  }

  /**       A
    *    a/  \m
    *   B     C
    * a/ \m a/ \m
    * D   E F   G
    *
    * "X ->ₚ Y" means "Y is a P of X" (because X pulls in Y as its P)
    *
    * [B -> D] D is an admin of B (because B is pulling in D as an admin)
    * [B -> E] E is a member of B (because B is pulling in E as a member)
    */
  private def createTree(testSubject: Char) = test { (t, uaa) =>
    val uab = t.createUser()
    val uac = t.createUser()
    val uad = t.createUser()
    val uae = t.createUser()
    val uaf = t.createUser()
    val uag = t.createUser()

    val uma = t.createUser()
    val umb = t.createUser()
    val umc = t.createUser()
    val umd = t.createUser()
    val ume = t.createUser()
    val umf = t.createUser()
    val umg = t.createUser()

    val ga = t.createUserGroup.admins(uaa).members(uma)                  .create().getOrThrow()
    val gb = t.createUserGroup.admins(uab).members(umb).adminParents (ga).create().getOrThrow()
    val gc = t.createUserGroup.admins(uac).members(umc).memberParents(ga).create().getOrThrow()
    val gd = t.createUserGroup.admins(uad).members(umd).adminParents (gb).create().getOrThrow()
    val ge = t.createUserGroup.admins(uae).members(ume).memberParents(gb).create().getOrThrow()
    val gf = t.createUserGroup.admins(uaf).members(umf).adminParents (gc).create().getOrThrow()
    val gg = t.createUserGroup.admins(uag).members(umg).memberParents(gc).create().getOrThrow()

    val e: UniverseU = {
      var u = newUniverse

      u = u(ga)(_.admins(uaa).members(uma).adminChildren(gb).memberChildren(gc))
      u = u(gb)(_.admins(uab).members(umb).adminChildren(gd).memberChildren(ge))
      u = u(gc)(_.admins(uac).members(umc).adminChildren(gf).memberChildren(gg))
      u = u(gd)(_.admins(uad).members(umd))
      u = u(ge)(_.admins(uae).members(ume))
      u = u(gf)(_.admins(uaf).members(umf))
      u = u(gg)(_.admins(uag).members(umg))

      u.result
    }

    def test(g: Id)(admins: UserId*)(members: UserId*)(implicit l: Line) =
      try {
        t.assertUniverse(g, e)
        assertEq(s"Admin users of group $g", e.allUsersInGroup(g, Admin), admins.toSet)
        assertEq(s"Member users of group $g", e.allUsersInGroup(g, Member), members.toSet)
      } catch {
        case t: Throwable =>
          println()
          println(s"Group IDs: a=${ga.value}, b=${gb.value}, c=${gc.value}, d=${gd.value}, e=${ge.value}, f=${gf.value}, g=${gg.value}")
          println(s"Expecting $e")
          throw t
      }

    testSubject match {
      case 'a' => test(ga)(uaa, uab, uad)(uma, umc, umg)
      case 'b' => test(gb)(uab, uad)(umb, ume)
      case 'c' => test(gc)(uac, uaf)(umc, umg)
      case 'd' => test(gd)(uad)(umd)
      case 'e' => test(ge)(uae)(ume)
      case 'f' => test(gf)(uaf)(umf)
      case 'g' => test(gg)(uag)(umg)
    }
  }

  private def createCycle() = test { (t, u) =>
    val g1 = t.createUserGroup.admins(u).create().getOrThrow()
    val g2 = t.createUserGroup.admins(u).memberParents(g1).create().getOrThrow()
    val es = t.createUserGroup.admins(u).adminParents(g2).adminChildren(g1).create().getLeftOrThrow()
    t.assertErrors(es)(ValidationError.GraphCycle((), ()))
    t.assertUniverse(Id(g2.value + 1), Universe.empty)
  }

  // No point testing IDs that don't exist; we know we don't create them, we know they'll fail
  // private def createBadGroup() = test { (t, u) =>
  //   val g1 = t.createUserGroup.admins(u).create().getOrThrow()
  //   val g2 = Id(g1.value + 9999)
  //   val r  = t.createUserGroup.admins(u).adminChildren(g1, g2).create().getLeftOrThrow()
  //   assertSeq(r, Set[ValidationError](UserGroup.ValidationError.GroupNotFound(g2)))
  // }

  private def createNoAdmin() = test { (t, u) =>
    val es = t.createUserGroup.members(u).create().getLeftOrThrow()
    t.assertErrors(es)(ValidationError.NoAdminUsers(()))
  }

  private def createParentAdmin() = test { (t, u) =>
    val g = t.createUserGroup.admins(u).create().getOrThrow()
    t.createUserGroup.adminParents(g).create().getOrThrow()
    ()
  }

  private def createChildAdmin() = test { (t, u) =>
    val g  = t.createUserGroup.admins(u).create().getOrThrow()
    val es = t.createUserGroup.adminChildren(g).create().getLeftOrThrow()
    t.assertErrors(es)(ValidationError.NoAdminUsers(()))
    t.assertUniverse(Id(g.value + 1), Universe.empty)
  }

  private def updateOk() = test { (t, u) =>
    val g    = t.createUserGroup.admins(u).create().getOrThrow()
    val guca = t.createUserGroup.admins(u).adminParents(g).create().getOrThrow()
    val gupa = t.createUserGroup.admins(u).adminChildren(g).create().getOrThrow()
    val gucm = t.createUserGroup.admins(u).memberParents(g).create().getOrThrow()
    val gupm = t.createUserGroup.admins(u).memberChildren(g).create().getOrThrow()
    val gxca = t.createUserGroup.admins(u).adminParents(g).create().getOrThrow()
    val gxpa = t.createUserGroup.admins(u).adminChildren(g).create().getOrThrow()
    val gxcm = t.createUserGroup.admins(u).memberParents(g).create().getOrThrow()
    val gxpm = t.createUserGroup.admins(u).memberChildren(g).create().getOrThrow()

    try {

      t.assertUniverse(g, newUniverse
        .admins(g)(u)
        .admins(guca)(u)
        .admins(gupa)(u)
        .admins(gucm)(u)
        .admins(gupm)(u)
        .admins(gxca)(u)
        .admins(gxpa)(u)
        .admins(gxcm)(u)
        .admins(gxpm)(u)
        .adminParents(g)(gxpa, gupa)
        .adminChildren(g)(gxca, guca)
        .memberParents(g)(gxpm, gupm)
        .memberChildren(g)(gxcm, gucm)
        .result
      )

      val g2 = t.createUserGroup.admins(u).create().getOrThrow()

      t.updateUserGroup(g)
        .delAdminParents(gupa)
        .delAdminChildren(guca)
        .delMemberParents(gupm)
        .delMemberChildren(gucm)
        .addAdminParents(gucm)
        .addAdminChildren(gupm)
        .addMemberParents(guca)
        .addMemberChildren(gupa, g2)
        .update("name!", "updateOk")
        .getOrThrow()

      t.assertUniverse(g, newUniverse
        .admins(g)(u)
        .admins(guca)(u)
        .admins(gupa)(u)
        .admins(gucm)(u)
        .admins(gupm)(u)
        .admins(gxca)(u)
        .admins(gxpa)(u)
        .admins(gxcm)(u)
        .admins(gxpm)(u)
        .admins(g2)(u)
        .adminParents(g)(gxpa, gucm)
        .adminChildren(g)(gxca, gupm)
        .memberParents(g)(gxpm, guca)
        .memberChildren(g)(gxcm, gupa, g2)
        .result
      )

      val ug = t.db.getUserGroupUniverseForUser(u).groups(g)
      assertEq("new name", ug.name, Name("name!"))
      assertEq("new handle", ug.handle, Handle("updateOk"))

    } catch {
      case t: Throwable =>
        println(s"g=${g.value}, guca=${guca.value}, gupa=${gupa.value}, gucm=${gucm.value}, gupm=${gupm.value}, gxca=${gxca.value}, gxpa=${gxpa.value}, gxcm=${gxcm.value}, gxpm=${gxpm.value}")
        println()
        throw t
    }
  }

  private def updateCycle() = test { (t, u) =>
    val g1 = t.createUserGroup.admins(u).create().getOrThrow()
    val g2 = t.createUserGroup.admins(u).memberParents(g1).create().getOrThrow()
    val g3 = t.createUserGroup.admins(u).adminParents(g2).create().getOrThrow()
    val un = t.db.getUserGroupUniverseU(g1)
    val es = t.updateUserGroup(g3).addAdminChildren(g1).update().getLeftOrThrow()
    t.assertErrors(es)(ValidationError.GraphCycle((), ()))
    t.assertUniverse(g1, un)
  }
  // No point testing IDs that don't exist; we know we don't create them, we know they'll fail
  // private def updateBadGroupDel() = test { (t, u) =>
  //   val g  = t.createUserGroup.admins(u).create().getOrThrow()
  //   val g2 = Id(g.value + 9999)
  //   val es = t.updateUserGroup(g).delAdminChildren(g2).update()
  //   assertSeq(es, Set[ValidationError](UserGroup.ValidationError.GroupNotFound(g2)))
  // }

  // No point testing IDs that don't exist; we know we don't create them, we know they'll fail
  // private def updateBadGroupAdd() = test { (t, u) =>
  //   val g  = t.createUserGroup.admins(u).create().getOrThrow()
  //   val g2 = Id(g.value + 9999)
  //   val es = t.updateUserGroup(g).addAdminChildren(g2).update()
  //   assertSeq(es, Set[ValidationError](UserGroup.ValidationError.GroupNotFound(g2)))
  // }

  private def updateNoAdmin() = test { (t, u) =>
    val g  = t.createUserGroup.admins(u).create().getOrThrow()
    val un = t.db.getUserGroupUniverseU(g)
    val es = t.updateUserGroup(g).delAdmins(u).addMembers(u).update().getLeftOrThrow()
    t.assertErrors(es)(ValidationError.NoAdminUsers(()))
    t.assertUniverse(g, un)
  }

  private def createDupHandle() = test { (t, u) =>
    val h  = "createDupHandle"
    val g  = t.createUserGroup.admins(u).create(h).getOrThrow()
    val un = t.db.getUserGroupUniverseU(g)
    val es = t.createUserGroup.admins(u).create(h).getLeftOrThrow()
    assertEq(es, SaveError.HandleAlreadyTaken)
    t.assertUniverse(g, un)
  }

  private def updateDupHandle() = test { (t, u) =>
    val h  = "updateDupHandle"
    val _  = t.createUserGroup.admins(u).create(h).getOrThrow()
    val g  = t.createUserGroup.admins(u).create().getOrThrow()
    val es = t.updateUserGroup(g).update(handle = h).getLeftOrThrow()
    assertEq(es, SaveError.HandleAlreadyTaken)
  }

  private def testUsernamesEmpty() = test { (t, _) =>
    val m = t.db.getUserIdsByUsername(Set.empty).getOrThrow()
    assertEq(m, Map.empty[Username, UserId])
  }

  private def testUsernamesAll() = test { (t, u) =>
    val u2 = t.createUser()
    val m = t.db.getUserIdsByUsername(Set(u.username, u2.username)).getOrThrow()
    assertEq(m, Map(u.username -> u.id, u2.username -> u2.id))
  }

  private def testUsernamesMissing() = test { (t, u) =>
    def newBadUsername() = Username("x" + UUID.randomUUID().toString().replace("-", " "))
    val u2 = newBadUsername()
    val u3 = newBadUsername()
    val e = t.db.getUserIdsByUsername(Set(u.username, u2, u3)).getLeftOrThrow()
    assertEq(e, NonEmptySet(u2, u3))
  }

  // ===================================================================================================================

  override def tests = Tests {
    beforeTest()

    "getUserIdsByUsername" - {
      "empty" - testUsernamesEmpty()
      "all" - testUsernamesAll()
      "missing" - testUsernamesMissing()
    }

    "create" - {
      "sole" - createSole()
      "cycle" - createCycle()
      // "badGroup" - createBadGroup() // No point testing IDs that don't exist; we know we don't create them, we know they'll fail
      "noAdmin" - createNoAdmin()
      "parentAdmin" - createParentAdmin()
      "childAdmin" - createChildAdmin()
      "dupHandle" - createDupHandle()
      "tree" - {
        "a" - createTree('a')
        "b" - createTree('b')
        "c" - createTree('c')
        "d" - createTree('d')
        "e" - createTree('e')
        "f" - createTree('f')
        "g" - createTree('g')
      }
    }

    "update" - {
      "ok" - updateOk()
      "cycle" - updateCycle()
      // "badGroupDel" - updateBadGroupDel() // No point testing IDs that don't exist; we know we don't create them, we know they'll fail
      // "badGroupAdd" - updateBadGroupAdd() // No point testing IDs that don't exist; we know we don't create them, we know they'll fail
      "noAdmin" - updateNoAdmin()
      "dupHandle" - updateDupHandle()
    }
  }
}
