package shipreq.webapp.feature

// TODO Re-enable InspectionTest. (Pending util-eval for 2.11)
//import com.twitter.util.Eval
//import org.scalatest.FunSuite
//import scalaz.syntax.show._
//import shipreq.webapp.test.TestData
//import shipreq.webapp.feature.uc.UseCase
//import Inspection._
//import uc.text.FreeTextTerms.MathTexTerm
//
//class InspectionTest extends FunSuite with TestData {
//
//  val imports = "import scalaz.{Name,Need,Value}, shipreq.base.util._, shipreq.webapp._, db._, lib.Types._, feature.uc, uc._, uc.field._, uc.step._, uc.text._, FreeTextTerms._, util._;"
//
//  def eval[T](code: String): T = new Eval(None).apply(imports + code)
//
//  test("UC inspection result should match the UC when evaluated") {
//    val ucs: List[UseCase] = MockUc1.sampleUC :: MockUc4.UC :: Nil
//    for (uc <- ucs) {
//      val code = uc.shows
//      val parsedUc: UseCase = eval(code)
//      assertUseCasesMatch(parsedUc, uc)
//      parsedUc.devView ==== uc.devView
//      parsedUc.inspect ==== uc.inspect
//    }
//  }
//
//  test("MathTexTerm") {
//    val x = MathTexTerm("YAY{}()\"!")
//    val y: MathTexTerm = eval(x.shows)
//    x ==== y
//  }
//}
