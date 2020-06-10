package shipreq.base.test

import japgolly.microlibs.utils.Memo
import org.scalacheck.Test.Parameters
import org.scalacheck.{Prop, Test}
import utest._

object MTestScalaCheck {

  val defaultParams: Parameters =
    Parameters.default
      .withMaxSize(4)
      .withWorkers(1)
      .withMinSuccessfulTests(30)

  val resultText: Int => String =
    Memo.int(n => s"OK, passed $n tests.")
}

trait MTestScalaCheck { self: TestSuite =>

  protected def scalaCheckParameters: Parameters =
    MTestScalaCheck.defaultParams

  protected final def scalaCheck(f: Prop.type => Prop): String = {
    val prop = f(Prop)
    val params = scalaCheckParameters
    val result = Test.check(params, prop)
    assert(result.passed)
    MTestScalaCheck.resultText(result.succeeded)
  }

}
