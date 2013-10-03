package com.beardedlogic.usecase.snippet

import org.scalatest.FunSuite
import com.beardedlogic.usecase.test.TestHelpers
import com.beardedlogic.usecase.db.ProjectSummary
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.util.NonEmptyTemplate

class ProjectListTest extends FunSuite with TestHelpers {
  lazy val html = NonEmptyTemplate.load("loggedin/index").extract("#project-list").get

  import ProjectList.renderProjectList

  def includeNone = include("none")
  def includeProjects = include regex ("<ol[ >]")

  test("No projects") {
    renderProjectList(Nil)(html).toString should (includeNone and (not(includeProjects)))
  }

  test("Projects") {
    val p1 = ProjectSummary(1.tag[ProjectIdTag], "Empty", 0, None)
    val p2 = ProjectSummary(2.tag[ProjectIdTag], "Hello", 2, Some("2013-05-20T15:57:35.773674+10:00"))
    val r = renderProjectList(p1 :: p2 :: Nil)(html).toString
    r should (includeProjects and (not(includeNone))
      and include("Empty") and include("0 Use Cases")
      and include("Hello") and include("2 Use Cases"))
    r.occurrences("Last modified") shouldBe 1
  }
}
