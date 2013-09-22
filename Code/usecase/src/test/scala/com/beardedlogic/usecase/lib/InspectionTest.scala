package com.beardedlogic.usecase.lib

import com.twitter.util.Eval
import org.scalatest.FunSuite
import scalaz.syntax.show._
import com.beardedlogic.usecase.test.TestData
import Inspection._

class InspectionTest extends FunSuite with TestData {

  val imports = "import scalaz.{Name,Need,Value}, com.beardedlogic.usecase, usecase.db._, usecase.lib, lib._, field._, tree._, text._, usecase.util._, Types._;"

  def eval[T](code: String): T = new Eval(None).apply(imports + code)

  test("UC inspection result should match the UC when evaluated") {
    val ucs: List[UseCase] = MockUc1.sampleUC :: MockUc4.UC :: Nil
    for (uc <- ucs) {
      val code = uc.shows
      val parsedUc: UseCase = eval(code)
      assertUseCasesMatch(parsedUc, uc)
      parsedUc.toPrettyString ==== uc.toPrettyString
    }
  }
}
