package com.beardedlogic.usecase.snippet

import org.mockito.Mockito.when
import org.scalatest.FunSuite
import com.beardedlogic.usecase.db.{Project, UserDescriptor, UpdateProjectResult}
import com.beardedlogic.usecase.test.{MockDaoProvider, TestHelpers}
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.util.NonEmptyTemplate
import UpdateProjectResult._
import ProjectHeaderConsts._

class ProjectHeaderTest extends FunSuite with TestHelpers {

  lazy val html = NonEmptyTemplate.load("loggedin/project").get

  def run[R](
    loggedInUser: Option[UserDescriptor] = Some(UD1),
    project: Option[Project] = Some(Project("Grrr", UD1.id)),
    updateResult: UpdateProjectResult = Success("YAY")
    )(fn: ProjectHeader => R = identity[ProjectHeader] _): R = {

    val pid = 123456.tag[ProjectIdTag]
    val uid: UserId = loggedInUser.map(_.id).getOrElse((-1).tag[UserIdTag])
    MockDaoProvider(dao => {
      when(dao.findProject(pid)).thenReturn(project)
      when(dao.updateProject(meq(pid), meq(uid), any)).thenReturn(updateResult)
    }).install {
      withUserLoggedIn(loggedInUser) {
        inMockSession {
          val h = new ProjectHeader(pid)
          fn(h)
        }
      }
    }
  }

  test("Fails when project not found") {
    assertRedirect(run(project = None)())
  }

  test("Fails when user doesnt own project") {
    assertRedirect(run(project = Some(Project("Proj", 123.tag[UserId])))())
  }

  test("Render with valid project & user") {
    val output = run()(_.render(html))
    output.toString should include("Grrr")
  }

  test("Successful rename") {
    val js = run()(_.onRename())
    js.toString should (include("YAY") and include(TriggerProjectUpdated.triggerName))
  }
}
