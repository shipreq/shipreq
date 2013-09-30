package com.beardedlogic.usecase.snippet.project

import com.beardedlogic.usecase.db.{Project, UserDescriptor}
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.test.{MockDaoProvider, TestHelpers}
import org.mockito.Mockito.when
import org.scalatest.FunSuite

class HeaderTest extends FunSuite with TestHelpers {

  lazy val html = loadTemplate("loggedin/project")

  def run(loggedInUser: Option[UserDescriptor], project: Option[Project]): header = {
    val pid = 123456.tag[ProjectIdTag]
    withUserLoggedIn(loggedInUser) {
      MockDaoProvider(dao => when(dao.findProject(pid)).thenReturn(project)).install {
        new header(pid)
      }
    }
  }

  test("Fails when project not found") {
    assertRedirect(run(Some(UD1), None))
  }

  test("Fails when user doesnt own project") {
    assertRedirect(run(Some(UD1), Some(Project("Proj", 123.tag[UserId]))))
  }

  test("Render with valid project & user") {
    val h = run(Some(UD1), Some(Project("Grrr", UD1.id)))
    h.render(html).toString should include("Grrr")
  }
}
