package shipreq.webapp.base.hash

import utest._
import shipreq.base.util.UnivEq.Implicits._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.test.BaseTestUtil._
import shipreq.webapp.base.test.SampleProject3

/**
 * These tests ensure the stability of [[HashScheme]]s.
 *
 * Once a [[HashScheme]] has been used (i.e. an Event exists in the database which refers to it), it must never change.
 * Its results should always stay the same. Thus, once used, a [[HashScheme]]s values should be calculated and
 * hard-coded below.
 */
object HashSchemeTest extends TestSuite {

  lazy val projectData: List[Project] =
    Project.empty          ::
    SampleProject3.project :: Nil

  def calc(hs: HashScheme): List[Int] =
    projectData map hs.value.hashProject.hash

  override def tests = TestSuite {
    'latest - assertEq(calc(HashScheme.latest), List(-313536696, 1780548659))
  }
}
