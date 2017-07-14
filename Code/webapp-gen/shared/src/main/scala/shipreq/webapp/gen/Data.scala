package shipreq.webapp.gen

import shipreq.webapp.base.data._
import shipreq.webapp.base.user._

object Data {

  def username: MainAndTest[Username] =
    MainAndTest(Username("USERNAME"), Vector(Username("YXXusernameXYX")))

  def projectName: Vector[Project.Name] =
    Vector("Proj<ect/ \"nam>e\"!")

  type ProjectSpaLoader = (Username, Project.Name)

  def projectSpaLoaderData: MainAndTest[ProjectSpaLoader] = {
    val main = (username.main, "XXX projectname XXX")
    val tests = for {u <- username.tests; p <- projectName} yield (u, p)
    MainAndTest(main, tests)
  }
}

