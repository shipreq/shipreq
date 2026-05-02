package shipreq.webapp.server.logic.laws

import cats.implicits._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Instant
import java.util.UUID
import nyaya.gen.Gen
import shipreq.webapp.base.data._
import shipreq.webapp.member.project.data.{Project, ProjectAccess}
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.test.project.RandomData
import shipreq.webapp.member.test.project.UnsafeTypes.projectCreatorFromUserId
import shipreq.webapp.server.logic.algebra.DB.{ProjectSpaInitPage, ReadProjectEventError, SaveProjectEventError}
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
    def getUsernamesByUserId: Set[UserId] => NonEmptySet[UserId] \/ Map[UserId, Username]
    def getUserIdsByUsername: Set[Username] => NonEmptySet[Username] \/ Map[Username, UserId]
    def getProjectAccess: ProjectId => ProjectAccess
    def projectSpaInitPage: (ProjectId, UserId) => Option[ProjectSpaInitPage]
    def getProjectRolodex: ProjectId => Rolodex

    def needProjectCreator: ProjectId => UserId
    def getProjectEvents: ProjectId => ReadProjectEventError \/ VerifiedEvent.Seq
    def saveProjectEvent: (ProjectId, EventOrd, ActiveEvent, Project, UserId) => SaveProjectEventError \/ VerifiedEvent

    final def addEvent(pid: ProjectId, e: ActiveEvent): SaveProjectEventError \/ Unit = {
      val creator = needProjectCreator(pid)
      val events  = getProjectEvents(pid).getOrThrow()
      val p1      = applyVerifiedEventsSuccessfully(Project.init(creator), events)
      val ve      = VerifiedEvent(p1.history.nextOrd, e, Instant.now())
      val p2      = ApplyEvent.trusted(ve)(p1).fold(_.throwException(), identity)
      val uid     = Obfuscators.userId.deobfuscateOrThrow(p1.access.adminIterator().next())
      saveProjectEvent(pid, ve.ord, e, p2, uid).void
    }

    final def updateProjectAccess(pid: ProjectId, updates: Map[UserId, Option[ProjectPerm]]): SaveProjectEventError \/ Unit =
      addEvent(pid, Event.AccessUpdate(updates.mapKeysNow(Obfuscators.userId.obfuscate)))
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

      val actualRolodex = db.getProjectRolodex(pid)
      val expectRolodex = Rolodex(db.getUsernamesByUserId(expect.keySet).getOrThrow().mapKeysNow(Obfuscators.userId.obfuscate))
      assertEq(actualRolodex, expectRolodex)
    }

    def needUserId(u: Username): UserId =
      db.getUserIdsByUsername(Set(u)).getOrThrow()(u)

    def getProjectAccessByIds(pid: ProjectId): Map[UserId, ProjectPerm] =
      db.getProjectAccess(pid).asMap.mapKeysNow(Obfuscators.userId.deobfuscateOrThrow)

    def getUserIdsByUsername(ids: Set[Username]): NonEmptySet[Username] \/ Map[Username, UserId] =
      db.getUserIdsByUsername(ids).flatMap { users =>
        assertEq(db.getUsernamesByUserId(users.values.toSet), \/-(users.map(_.swap)))
        \/-(users)
      }

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
    val m = t.getUserIdsByUsername(Set.empty).getOrThrow()
    assertEq(m, Map.empty[Username, UserId])
  }

  private def testUsernamesAll() = test { (t, u) =>
    val u2 = t.createUser()
    val m = t.getUserIdsByUsername(Set(u.username, u2.username)).getOrThrow()
    assertEq(m, Map(u.username -> u.id, u2.username -> u2.id))
  }

  private def testUsernamesMissing() = test { (t, u) =>
    def newBadUsername() = Username("x" + UUID.randomUUID().toString().replace("-", " "))
    val u2 = newBadUsername()
    val u3 = newBadUsername()
    val e = t.getUserIdsByUsername(Set(u.username, u2, u3)).getLeftOrThrow()
    assertEq(e, NonEmptySet(u2, u3))
  }

  private def testUpdateProjectAccess(t: Tester, pid: ProjectId)(access: (UserId, Option[ProjectPerm])*)
                                     (expect: SaveProjectEventError \/ Map[UserId, ProjectPerm])(implicit q: Line): Unit = {

    val before = t.getProjectAccessByIds(pid)
    val actual = t.db.updateProjectAccess(pid, access.toMap)
    assertEq(actual.void, expect.void)

    val expectAfter = expect.getOrElse(before)
    assertMap(expectAfter, t.getProjectAccessByIds(pid))
  }

  private def addProjectMember(t: Tester, pid: ProjectId, perm: ProjectPerm): UserId = {
    val u = t.db.createUser().id
    t.db.updateProjectAccess(pid, Map(u -> perm.some)).getOrThrow()
    u
  }

  private def testUpdateProjectAccessNoUsers() = test { (t, u) =>
    val p = t.createProject(u)
    testUpdateProjectAccess(t, p)(u.id -> None)(-\/(SaveProjectEventError.OnAccess.CantRemoveLastAdmin))
  }

  private def testUpdateProjectAccessOnlyCollab() = test { (t, u) =>
    val p = t.createProject(u)
    testUpdateProjectAccess(t, p)(u.id -> ProjectPerm.Collaborator.some)(-\/(SaveProjectEventError.OnAccess.CantRemoveLastAdmin))
  }

  private def testUpdateProjectAccessAdd() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = t.db.createUser().id
    testUpdateProjectAccess(t, p)(u2 -> ProjectPerm.Collaborator.some)(\/-(Map(u.id -> ProjectPerm.Admin, u2 -> ProjectPerm.Collaborator)))
  }

  private def testUpdateProjectAccessDel() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = addProjectMember(t, p, ProjectPerm.Admin)
    testUpdateProjectAccess(t, p)(u.id -> None)(\/-(Map(u2 -> ProjectPerm.Admin)))
  }

  private def testUpdateProjectAccessUpd() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = addProjectMember(t, p, ProjectPerm.Admin)
    testUpdateProjectAccess(t, p)(u.id -> ProjectPerm.Collaborator.some)(\/-(Map(u.id -> ProjectPerm.Collaborator, u2 -> ProjectPerm.Admin)))
  }

  private def testUpdateProjectAccessBulk() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = addProjectMember(t, p, ProjectPerm.Collaborator)
    val u3 = addProjectMember(t, p, ProjectPerm.Admin)
    val u4 = t.db.createUser().id
    testUpdateProjectAccess(t, p)(
      u.id -> None,
      u3 -> ProjectPerm.Collaborator.some,
      u4 -> ProjectPerm.Admin.some,
    )(\/-(Map(
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
