package shipreq.fix

import scalafix.testkit._
import scala.annotation.nowarn

@nowarn("cat=deprecation")
class FixShipReqTests extends SemanticRuleSuite {

  val testFiles = Set(
    "RuleTest1.scala"
  )

  for (t <- testsToRun)
    if (testFiles.contains(t.path.testName))
      runOn(t)

}
