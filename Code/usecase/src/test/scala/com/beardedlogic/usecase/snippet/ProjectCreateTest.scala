package com.beardedlogic.usecase.snippet

import org.scalatest.{BeforeAndAfterEach, FunSuite}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.ResponseShortcutException
import com.beardedlogic.usecase.test.TestDatabaseSupport
import com.beardedlogic.usecase.test.fixture.UserFixture

class ProjectCreateTest extends FunSuite with TestDatabaseSupport with UserFixture with BeforeAndAfterEach {

  override def beforeEachWithDao {
    super.beforeEachWithDao
    initUserFixture
    login(user1)
  }

  def create(n: String): JsCmd = {
    val s = new ProjectCreate
    s.projectName = n
    s.onSubmit
  }

  def testSuccess(): Unit =
    assertRedirect(
      assertTableDiffs(Tables.Project -> 1)(
        create("hehe")))

  test("Creates new project") {testSuccess}

  test("Creation fails with empty name") {
    for (n <- Seq("", "  ")) {
      assertJsErrorNotice(create(n), Some("Invalid"))
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