package shipreq.webapp.server.logic.laws

import cats.implicits._
import java.util.UUID
import nyaya.gen.Gen
import shipreq.webapp.base.data._
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.ActiveEvent
import shipreq.webapp.member.test.project.RandomData
import shipreq.webapp.server.logic.algebra.DB.{ProjectSpaInitPage, UpdateProjectAccessError}
import shipreq.webapp.server.logic.data.ProjectEncryptionKey
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.webapp.server.logic.test.WebappServerLogicTestUtil._
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
    def updateProjectAccess: (ProjectId, Set[UserId], Map[UserId, ProjectPerm]) => UpdateProjectAccessError \/ Unit
    def getProjectAccess: ProjectId => Map[UserId, ProjectPerm]
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
      val actual = db.getProjectAccess(pid)
      assertMap(actual, expect)
    }

    def needUserId(u: Username): UserId =
      db.getUserIdsByUsername(Set(u)).getOrThrow()(u)
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
                                     (expect: UpdateProjectAccessError \/ Unit, expectedDbState: (UserId, ProjectPerm)*)(implicit q: Line): Unit = {
    val before = t.db.getProjectAccess(pid)
    val actual = t.db.updateProjectAccess(pid, remove.toSet, add.toMap)
    assertEq(actual, expect)

    val expectAfter: Map[UserId, ProjectPerm] =
      if (expect.isLeft) before else expectedDbState.toMap

    assertMap("getProjectAccess", expectAfter, t.db.getProjectAccess(pid))

    for (u <- expectAfter.keys) {
      val access = t.db
        .projectSpaInitPage(pid, u)
        .getOrThrow("projectSpaInitPage is empty").access
        .mapKeysNow(t.needUserId)
      assertMap("projectSpaInitPage", access, expectAfter)
    }
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
    testUpdateProjectAccess(t, p)()(u2 -> ProjectPerm.Collaborator)(\/-(()), u.id -> ProjectPerm.Admin, u2 -> ProjectPerm.Collaborator)
  }

  private def testUpdateProjectAccessDel() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = addProjectMember(t, p, ProjectPerm.Admin)
    testUpdateProjectAccess(t, p)(u)()(\/-(()), u2 -> ProjectPerm.Admin)
  }

  private def testUpdateProjectAccessUpd() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = addProjectMember(t, p, ProjectPerm.Admin)
    testUpdateProjectAccess(t, p)()(u.id -> ProjectPerm.Collaborator)(\/-(()), u.id -> ProjectPerm.Collaborator, u2 -> ProjectPerm.Admin)
  }

  private def testUpdateProjectAccessBulk() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = addProjectMember(t, p, ProjectPerm.Collaborator)
    val u3 = addProjectMember(t, p, ProjectPerm.Admin)
    val u4 = t.db.createUser().id
    testUpdateProjectAccess(t, p)(u)(u3 -> ProjectPerm.Collaborator, u4 -> ProjectPerm.Admin)(\/-(()),
      u2 -> ProjectPerm.Collaborator,
      u3 -> ProjectPerm.Collaborator,
      u4 -> ProjectPerm.Admin,
    )
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
