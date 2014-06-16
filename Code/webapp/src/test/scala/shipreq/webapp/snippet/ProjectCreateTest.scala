package shipreq.webapp.snippet

import org.scalatest.{BeforeAndAfterEach, FunSuite}
import net.liftweb.http.js.JsCmd
import shipreq.webapp.test.TestDatabaseSupport
import shipreq.webapp.test.fixture.UserFixture

class ProjectCreateTest extends FunSuite with TestDatabaseSupport with UserFixture with BeforeAndAfterEach {

  override def beforeEachWithDao {
    super.beforeEachWithDao
    initUserFixture
    login(user1)
  }

  def create(n: String): JsCmd = ProjectCreate.onSubmit(n)

  def testSuccess(): Unit =
    assertRedirect(
      assertTableDiffs(Tables.Project -> 1)(
        create("hehe")))

  test("Creates new project") {testSuccess}

  test("Creation fails with empty name") {
    for (n <- Seq("", "  ")) {
      assertJsErrorNotice(create(n), Some("blank"))
    }
  }

  test("Creation fails when project exists") {
    dao.createProject(user1.id, "hehe")
    assertJsErrorNotice(create("hehe"), Some("already"))
  }

  test("Allows same project name between different users") {
    dao.createProject(user2.id, "hehe")
    testSuccess
  }
}