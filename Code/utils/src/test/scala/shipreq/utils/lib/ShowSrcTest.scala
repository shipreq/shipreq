package shipreq.utils.lib

import com.twitter.util.Eval
import japgolly.nyaya._
import japgolly.nyaya.test._
import japgolly.nyaya.test.PropTestOps._
import scalaz.Equal
import scalaz.syntax.equal._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data.{Project => P}
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.test.BaseTestUtil._
import utest._
import ShowSrcDataImp._

object ShowSrcTest extends TestSuite {

  def eval[T](code: String): T = new Eval(None)(code)

  val showSrcIso = Prop.test[P]("showSrcIso", p => {
    val code = ShowSrc.generateBlock(p)
    val p2 = eval[P](code)

    if (p ≠ p2) {
      def check[A: Equal: ShowSrc](f: P => A): Unit = {
        val expect = f(p)
        val actual = f(p2)
        assertEq(ShowSrc.generateBlock(expect), actual, expect)
      }
      check(_.config.customIssueTypes)
      check(_.config.customReqTypes)
      check(_.config.tags)
      check(_.config.fields)
      check(_.reqs)
      check(_.reqCodes)
      check(_.implications)
      check(_.reqTags)
      check(_.reqText)
      check(_.config)
      check(identity)
    }

    true
  })

  implicit val settings = DefaultSettings.propSettings.setSampleSize(8*10).setGenSize(10) //.setSeed(5)

  override def tests = TestSuite {
    RandomData.project mustSatisfy showSrcIso

//    * - { RandomData.project.mustSatisfy(showSrcIso)(settings.setSeed(3)) }
//    * - { RandomData.project.mustSatisfy(showSrcIso)(settings.setSeed(4)) }
//    * - { RandomData.project.mustSatisfy(showSrcIso)(settings.setSeed(5)) }
//    * - { RandomData.project.mustSatisfy(showSrcIso)(settings.setSeed(6)) }
//    * - { RandomData.project.mustSatisfy(showSrcIso)(settings.setSeed(7)) }
//    * - { RandomData.project.mustSatisfy(showSrcIso)(settings.setSeed(8)) }
  }
}
