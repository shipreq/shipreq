package shipreq.webapp.server.logic.laws

import cats.implicits._
import java.util.UUID
import shipreq.webapp.base.data._
import shipreq.webapp.server.logic.test.WebappServerLogicTestUtil._
import utest._

abstract class DbLaws extends TestSuite {

  protected def beforeTest(): Unit =
    ()

  protected def newDbApi[A](f: DbApi => A): A

  protected trait DbApi {
    def createUser(): User
    def getUserIdsByUsername: Set[Username] => NonEmptySet[Username] \/ Map[Username, UserId]
  }

  // ===================================================================================================================

  private final class Tester()(implicit val db: DbApi) {
    def createUser() = db.createUser()
  }

  protected def test[A](f: (Tester, User) => A): A =
    newDbApi { implicit db =>
      val t = new Tester

      // Create some irrelevant noise
      db.createUser()

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

  // ===================================================================================================================

  override def tests = Tests {
    beforeTest()

    "getUserIdsByUsername" - {
      "empty" - testUsernamesEmpty()
      "all" - testUsernamesAll()
      "missing" - testUsernamesMissing()
    }

  }
}
