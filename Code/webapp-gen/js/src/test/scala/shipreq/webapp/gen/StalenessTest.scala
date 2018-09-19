package shipreq.webapp.gen

import utest._
import shipreq.base.test.BaseTestUtil._

/**
  * Ensures that generated templates don't go stale.
  * (stale meaning out-of-sync with what the origin React component generates now)
  */
object StalenessTest extends TestSuite {

  def assertGen[A](g: Generator[A])(actual: Html, data: A): Unit = {
    val expect = g(data)
    def split(h: Html): String =
      h.value.replace("<", "\n<").split('\n').toList.flatMap(_.grouped(80)).mkString("\n")
    assertMultiline(split(actual), split(expect))
    assertEq(s"${g.name} * $data", actual, expect)
  }

  def test[A](g: Generator[A])(actual: MainAndTest[Html]): Unit = {
    assertGen(g)(actual.main, g.data.main)
    assertEq(s"${g.name} test data size", actual.tests.length, g.data.tests.length)
    for (i <- g.data.tests.indices)
      assertGen(g)(actual.tests(i), g.data.tests(i))
  }

  override def tests = Tests {

    'size -
      assertEq("JS template count", Manifest.All.length, ExpectedTemplateCount)

    'content -
      Manifest.All.foreach {
        case g@ Manifest.ProjectSpaLoader => test(g.gen)(output.ProjectSpaLoader.templates)
      }
  }
}
