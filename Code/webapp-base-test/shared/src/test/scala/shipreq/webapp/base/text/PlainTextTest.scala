package shipreq.webapp.base.text

import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.test.SampleProject6._
import shipreq.webapp.base.test.UnsafeTypes._
import utest._
import Values._

object PlainTextTest extends TestSuite {

  def ctx0 = ProjectText.Context.UseCase(0.UC)
  def ctx1 = ProjectText.Context.UseCase(uc1 )

  override def tests = TestSuite {
    'useCaseStepRefs {
      val full = s"[UC-$step16_label] and [UC-$step17_label] are dead. [UC-$step19_label] and [UC-$step18_label] are not."

      'noCtx - assertEq(plainText              .reqTitleById(uc1), full)
      'uc1   - assertEq(plainText.withCtx(ctx1).reqTitleById(uc1), full.replaceAll("UC-", ""))
      'uc2   - assertEq(plainText.withCtx(ctx0).reqTitleById(uc1), full)
    }
  }
}
