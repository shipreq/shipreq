package shipreq.fix

import org.scalatest.FunSuiteLike
import scalafix.testkit.AbstractSemanticRuleSuite

class FixShipReqTests extends AbstractSemanticRuleSuite with FunSuiteLike {

  val testFiles = Set(
    "RuleTest1.scala"
  )

  for (t <- testsToRun)
    if (testFiles.contains(t.path.testName))
      runOn(t)
}
