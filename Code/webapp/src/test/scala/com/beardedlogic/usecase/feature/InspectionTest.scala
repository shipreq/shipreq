package com.beardedlogic.usecase.feature

import com.twitter.util.Eval
import org.scalatest.FunSuite
import scalaz.syntax.show._
import com.beardedlogic.usecase.test.TestData
import com.beardedlogic.usecase.feature.uc.UseCase
import Inspection._
import uc.text.FreeTextTerms.MathTexTerm

class InspectionTest extends FunSuite with TestData {

  val imports = "import scalaz.{Name,Need,Value}, com.beardedlogic.usecase, usecase.db._, usecase.lib.Types._, usecase.feature.uc, uc._, uc.field._, uc.step._, uc.text._, FreeTextTerms._, usecase.util._;"

  def eval[T](code: String): T = new Eval(None).apply(imports + code)

  test("UC inspection result should match the UC when evaluated") {
    val ucs: List[UseCase] = MockUc1.sampleUC :: MockUc4.UC :: Nil
    for (uc <- ucs) {
      val code = uc.shows
      val parsedUc: UseCase = eval(code)
      assertUseCasesMatch(parsedUc, uc)
      parsedUc.devView ==== uc.devView
      parsedUc.inspect ==== uc.inspect
    }
  }

  test("MathTexTerm") {
    val x = MathTexTerm("YAY{}()\"!")
    val y: MathTexTerm = eval(x.shows)
    x ==== y
  }
}
