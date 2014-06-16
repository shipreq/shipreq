package shipreq.webapp.snippet

import org.scalatest.FunSuite
import shipreq.webapp.test.TestHelpers
import shipreq.webapp.db.ProjectSummary
import shipreq.webapp.lib.Types._
import shipreq.webapp.util.NonEmptyTemplate
import shipreq.webapp.lib.Misc

class ProjectListTest extends FunSuite with TestHelpers {
  lazy val html = NonEmptyTemplate.load("loggedin/index").extract("#project-list").get

  import ProjectList.renderProjectList

  def includeNone = include("none")
  def includeProjects = include regex "<ol[ >]"

  test("No projects") {
    renderProjectList(Nil)(html).toString should (includeNone and (not(includeProjects)))
  }

  test("Projects") {
    val p1 = ProjectSummary(ProjectId(1), "Empty", 0, None, 0, 0, None)
    val p2 = ProjectSummary(ProjectId(2), "Hello", 2, Some(Misc.currentTimeAsIso8601Str), 0, 0, None)
    val p3 = ProjectSummary(ProjectId(2), "Wait!", 1, Some(Misc.currentTimeAsIso8601Str), 1, 20, Some(Misc.currentTimeAsIso8601Str))
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
