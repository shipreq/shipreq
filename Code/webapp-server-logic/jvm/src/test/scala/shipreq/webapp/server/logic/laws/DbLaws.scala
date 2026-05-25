package shipreq.webapp.server.logic.laws

import cats.implicits._
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

    final def loadProject(pid: ProjectId): Project = {
      val creator = needProjectCreator(pid)
      val events  = getProjectEvents(pid).getOrThrow()
      applyVerifiedEventsSuccessfully(Project.init(creator), events)
    }

    final def addEvent(pid: ProjectId, e: ActiveEvent): SaveProjectEventError \/ Unit = {
      val p1  = loadProject(pid)
      val uid = p1.access.adminIterator().next()
      val ve  = VerifiedEvent(p1.history.nextOrd, e, uid, Instant.now())
      val p2  = ApplyEvent.trusted(ve)(p1).fold(_.throwException(), identity)
      saveProjectEvent(pid, ve.ord, e, p2, uid).void
    }

    final def updateProjectAccess(pid: ProjectId, updates: Map[UserId, Option[ProjectRole]]): SaveProjectEventError \/ Unit =
      updates.toList
        .sortBy(e => if (e._2.nonEmpty) -e._1.value else e._1.value) // do adds before removes
        .traverseVoid(e => addEvent(pid, Event.AccessUpdate(e._1, e._2)))
  }

  // ===================================================================================================================

  private final class Tester()(implicit val db: DbApi) {
    def createUser() =
      db.createUser()

    def createProject(uid: UserId): ProjectId =
      db.createProject(uid, Vector.empty, Project.empty, genProjectEncryptionKey.sample())

    def needUserId(u: Username): UserId =
      db.getUserIdsByUsername(Set(u)).getOrThrow()(u)

    def getProjectAccessAsIds(pid: ProjectId): Map[UserId, ProjectRole] =
      db.getProjectAccess(pid).asMap

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
      t.createProject(db.createUser())

      // Create a user for tests
      val u = db.createUser()

      f(t, u)
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

  private def testUpdateProjectAccess(t: Tester, pid: ProjectId)(access: (UserId, Option[ProjectRole])*)
                                     (expect: SaveProjectEventError \/ Map[UserId, ProjectRole])(implicit q: Line): Unit = {

    // Apply access events
    val before = t.getProjectAccessAsIds(pid)
    val actual = t.db.updateProjectAccess(pid, access.toMap)
    assertEq(actual, expect.void)

    // Test db.getProjectAccess
    val expectAfter = expect.getOrElse(before)
    assertMap(expectAfter, t.getProjectAccessAsIds(pid))

    // Test that all users with access are in the rolodex
    val p            = t.db.loadProject(pid)
    val allAuthors   = p.access.asMap.keySet
    val rolodex      = t.db.getProjectRolodex(pid)
    val rolodexUsers = rolodex.asMap.keySet
    assertEq(rolodexUsers, allAuthors)
  }

  private def addProjectMember(t: Tester, pid: ProjectId, role: ProjectRole): UserId = {
    val u = t.db.createUser().id
    t.db.updateProjectAccess(pid, Map(u -> role.some)).getOrThrow()
    u
  }

  private def testUpdateProjectAccessNoUsers() = test { (t, u) =>
    val p = t.createProject(u)
    testUpdateProjectAccess(t, p)(u.id -> None)(-\/(SaveProjectEventError.OnAccess.CantRemoveLastAdmin))
  }

  private def testUpdateProjectAccessOnlyCollab() = test { (t, u) =>
    val p = t.createProject(u)
    testUpdateProjectAccess(t, p)(u.id -> ProjectRole.Collaborator.some)(-\/(SaveProjectEventError.OnAccess.CantRemoveLastAdmin))
  }

  private def testUpdateProjectAccessAdd() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = t.db.createUser().id
    testUpdateProjectAccess(t, p)(u2 -> ProjectRole.Collaborator.some)(\/-(Map(u.id -> ProjectRole.Admin, u2 -> ProjectRole.Collaborator)))
  }

  private def testUpdateProjectAccessDel() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = addProjectMember(t, p, ProjectRole.Admin)
    testUpdateProjectAccess(t, p)(u.id -> None)(\/-(Map(u2 -> ProjectRole.Admin)))
  }

  private def testUpdateProjectAccessUpd() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = addProjectMember(t, p, ProjectRole.Admin)
    testUpdateProjectAccess(t, p)(u.id -> ProjectRole.Collaborator.some)(\/-(Map(u.id -> ProjectRole.Collaborator, u2 -> ProjectRole.Admin)))
  }

  private def testUpdateProjectAccessBulk() = test { (t, u) =>
    val p = t.createProject(u)
    val u2 = addProjectMember(t, p, ProjectRole.Collaborator)
    val u3 = addProjectMember(t, p, ProjectRole.Admin)
    val u4 = t.db.createUser().id
    testUpdateProjectAccess(t, p)(
      u.id -> None,
      u3 -> ProjectRole.Collaborator.some,
      u4 -> ProjectRole.Admin.some,
    )(\/-(Map(
      u2 -> ProjectRole.Collaborator,
      u3 -> ProjectRole.Collaborator,
      u4 -> ProjectRole.Admin,
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

    "projectSpaInitPage" - {
      "ok" - test { (t, u) =>
        val p = t.createProject(u)
        assert(t.db.projectSpaInitPage(p, u).isDefined)
      }
      "noAccess" - test { (t, u) =>
        val p = t.createProject(u)
        val u2 = t.createUser()
        assertEq(t.db.projectSpaInitPage(p, u2), None)
      }
      "revokedAccess" - test { (t, u) =>
        val p = t.createProject(u)
        val u2 = addProjectMember(t, p, ProjectRole.Collaborator)
        assert(t.db.projectSpaInitPage(p, u2).isDefined)
        t.db.updateProjectAccess(p, Map(u2 -> None)).getOrThrow()
        assertEq(t.db.projectSpaInitPage(p, u2), None)
      }
    }

  }
}
