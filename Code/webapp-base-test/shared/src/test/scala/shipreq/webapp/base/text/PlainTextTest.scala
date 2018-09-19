package shipreq.webapp.base.text

import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.test.SampleProject6._
import shipreq.webapp.base.test.UnsafeTypes._
import utest._
import Values._

object PlainTextTest extends TestSuite {

  def ctxUc1 = ProjectText.Context.Req(uc1 )
  def ctxUc0 = ProjectText.Context.Req(0.UC)

  override def tests = Tests {
    'useCaseStepRefs {
      val full = s"[UC-$step16_label] and [UC-$step17_label] are dead. [UC-$step19_label] and [UC-$step18_label] are not."

      'noCtx - assertEq(plainText                .reqTitleById(uc1), full)
      'uc1   - assertEq(plainText.withCtx(ctxUc1).reqTitleById(uc1), full.replaceAll("UC-", ""))
      'ucN   - assertEq(plainText.withCtx(ctxUc0).reqTitleById(uc1), full.replaceAll("UC-", ""))
    }
  }
}
