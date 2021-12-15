package shipreq.webapp.server.logic.laws

import cats.implicits._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.util.UUID
import nyaya.gen.Gen
import shipreq.webapp.base.data._
import shipreq.webapp.member.project.data.{Project, ProjectAccess}
import shipreq.webapp.member.project.event.ActiveEvent
import shipreq.webapp.member.test.project.RandomData
import shipreq.webapp.server.logic.algebra.DB.{ProjectSpaInitPage, UpdateProjectAccessError}
import shipreq.webapp.server.logic.data.ProjectEncryptionKey
import shipreq.webapp.server.logic.test.WebappServerLogicTestUtil._
import shipreq.webapp.server.logic.util.Obfuscators
import sourcecode.Line
import utest._

object DbLaws {
  val genProjectEncryptionKey: Gen[ProjectEncryptionKey] =
    RandomData.genBinaryData(32).map(ProjectEncryptionKey.apply)
}

abstract class DbLaws extends TestSuite {
  import DbLaws._

  protected def beforeTest(): Unit =
    ()

  protected def newDbApi[A](f: DbApi => A): A

  protected trait DbApi {
    def createUser(): User
    def createProject: (UserId, Vector[ActiveEvent], Project, ProjectEncryptionKey) => ProjectId
    def getUserIdsByUsername: Set[Username] => NonEmptySet[Username] \/ Map[Username, UserId]
    def updateProjectAccess: (ProjectId, Set[UserId], Map[UserId, ProjectPerm]) => UpdateProjectAccessError \/ ProjectAccess
    def getProjectAccess: ProjectId => ProjectAccess
    def projectSpaInitPage: (ProjectId, UserId) => Option[ProjectSpaInitPage]
  }

  // ===================================================================================================================

  private final class Tester()(implicit val db: DbApi) {
    def createUser() =
      db.createUser()

    def createProject(uid: UserId): ProjectId =
      db.createProject(uid, Vector.empty, Project.empty, genProjectEncryptionKey.sample())

    def assertProjectAccess(pid: ProjectId)(entries: (UserId, ProjectPerm)*)(implicit q: Line): Unit = {
      val expect = entries.toMap
      assert(expect.size == entries.size)
      val actual = getProjectAccessByIds(pid)
      assertMap(actual, expect)
    }

    def needUserId(u: Username): UserId =
      db.getUserIdsByUsername(Set(u)).getOrThrow()(u)

    def getProjectAccessByIds(pid: ProjectId): Map[UserId, ProjectPerm] =
      db.getProjectAccess(pid).value.mapKeysNow(Obfuscators.userId.deobfuscateOrThrow)
  }

  private implicit def autoUserId(u: User): UserId = u.id

  protected def test[A](f: (Tester, User) => A): A =
    newDbApi { implicit db =>
      val t = new Tester

      // Create some irrelevant noise
      val u = db.createUser()
      t.createProject(u)

      f(t, db.createUser())
    }

  // ===================================================================================================================

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

  private def testUpdateProjectAccess(t: Tester, pid: ProjectId)(remove: UserId*)(add: (UserId, ProjectPerm)*)
                                     (expect: UpdateProjectAccessError \/ Map[UserId, ProjectPerm])(implicit q: Line): Unit = {
    val before = t.getProjectAccessByIds(pid)
    val actual = t.db.updateProjectAccess(pid, remove.toSet, add.toMap)
    assertEq(actual.void, expect.void)

    val expectAfter = expect.getOrElse(before)
    assertMap(expectAfter, t.getProjectAccessByIds(pid))
  }

  private def addProjectMember(t: Tester, pid: ProjectId, perm: ProjectPerm): UserId = {
    val u = t.db.createUser().id
    t.db.updateProjectAccess(pid, Set.empty, Map(u -> perm)).getOrThrow()
    u
  }

  private def testUpdateProjectAccessNoUsers() = test { (t, u) =>
    val p = t.createProject(u)
    testUpdateProjectAccess(t, p)(u)()(-\/(UpdateProjectAccessError.CantRemoveLastAdmin))
  }

  private def testUpdateProjectAccessOnlyCollab() = test { (t, u) =>
    val p = t.createProject(u)
    testUpdateProjectAccess(t, p)(u)(u.id -> ProjectPerm.Collaborator)(-\/(UpdateProjectAccessError.CantRemoveLastAdmin))
  }

  private def testUpdateProjectAccessAdd() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = t.db.createUser().id
    testUpdateProjectAccess(t, p)()(u2 -> ProjectPerm.Collaborator)(\/-(Map(u.id -> ProjectPerm.Admin, u2 -> ProjectPerm.Collaborator)))
  }

  private def testUpdateProjectAccessDel() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = addProjectMember(t, p, ProjectPerm.Admin)
    testUpdateProjectAccess(t, p)(u)()(\/-(Map(u2 -> ProjectPerm.Admin)))
  }

  private def testUpdateProjectAccessUpd() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = addProjectMember(t, p, ProjectPerm.Admin)
    testUpdateProjectAccess(t, p)()(u.id -> ProjectPerm.Collaborator)(\/-(Map(u.id -> ProjectPerm.Collaborator, u2 -> ProjectPerm.Admin)))
  }

  private def testUpdateProjectAccessBulk() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = addProjectMember(t, p, ProjectPerm.Collaborator)
    val u3 = addProjectMember(t, p, ProjectPerm.Admin)
    val u4 = t.db.createUser().id
    testUpdateProjectAccess(t, p)(u)(u3 -> ProjectPerm.Collaborator, u4 -> ProjectPerm.Admin)(\/-(Map(
      u2 -> ProjectPerm.Collaborator,
      u3 -> ProjectPerm.Collaborator,
      u4 -> ProjectPerm.Admin,
    )))
  }

  // ===================================================================================================================

  override def tests = Tests {
    beforeTest()

    "getUserIdsByUsername" - {
      "empty" - testUsernamesEmpty()
      "all" - testUsernamesAll()
      "missing" - testUsernamesMissing()
    }

    "updateProjectAccess" - {
      "noUsers" - testUpdateProjectAccessNoUsers()
      "onlyCollab" - testUpdateProjectAccessOnlyCollab()
      "add" - testUpdateProjectAccessAdd()
      "del" - testUpdateProjectAccessDel()
      "upd" - testUpdateProjectAccessUpd()
      "bulk" - testUpdateProjectAccessBulk()
    }

  }
}
