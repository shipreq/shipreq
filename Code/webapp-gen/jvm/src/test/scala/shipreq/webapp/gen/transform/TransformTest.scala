package shipreq.webapp.gen.transform

import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.gen._

object TransformTest extends TestSuite {

  def assertGen[A](t: Transformer[A])(actual: Html, data: A): Unit =
    assertEq(s"$t * $data", actual, t(data))

  def test[A](t: Transformer[A]): Unit = {
    for (i <- t.data.tests.indices)
      assertGen(t)(t.templates.tests(i), t.data.tests(i))
  }

  override def tests = Tests {

    'size -
      assertEq("JVM template count", Transformer.All.length, ExpectedTemplateCount)

    'content -
      Transformer.All.foreach(test(_))
  }
}
