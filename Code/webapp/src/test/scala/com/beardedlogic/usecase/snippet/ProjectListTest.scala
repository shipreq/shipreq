package com.beardedlogic.usecase.snippet

import org.scalatest.FunSuite
import com.beardedlogic.usecase.test.TestHelpers
import com.beardedlogic.usecase.db.ProjectSummary
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.util.NonEmptyTemplate
import com.beardedlogic.usecase.lib.Misc

class ProjectListTest extends FunSuite with TestHelpers {
  lazy val html = NonEmptyTemplate.load("loggedin/index").extract("#project-list").get

  import ProjectList.renderProjectList

  def includeNone = include("none")
  def includeProjects = include regex ("<ol[ >]")

  test("No projects") {
    renderProjectList(Nil)(html).toString should (includeNone and (not(includeProjects)))
  }

  test("Projects") {
    val p1 = ProjectSummary(1.tag[IsProjectId], "Empty", 0, None, 0, 0, None)
    val p2 = ProjectSummary(2.tag[IsProjectId], "Hello", 2, Some(Misc.currentTimeAsIso8601Str), 0, 0, None)
    val p3 = ProjectSummary(2.tag[IsProjectId], "Wait!", 1, Some(Misc.currentTimeAsIso8601Str), 1, 20, Some(Misc.currentTimeAsIso8601Str))
    val r = renderProjectList(p1 :: p2 :: p3 :: Nil)(html).toString
    r should (includeProjects and (not(includeNone))
      and include("Empty") and include("0 use cases")
      and include("Hello") and include("2 use cases") and include("0 shares.")
      and include("Wait!") and include("1 use case.") and include("1 share with 20 views.")
    )
    r.occurrences("Last modified") shouldBe 2
    r.occurrences("Last viewed") shouldBe 1
  }
}
