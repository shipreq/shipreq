package shipreq.utils.lib

import com.twitter.util.Eval
import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTestOps._
import scala.util.{Failure, Success, Try}
import scalaz.Equal
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data.{Project => P}
import shipreq.webapp.base.test.WebappTestUtil._
import utest._
import ShowSrcDataImp._

/*
object ShowSrcTest extends TestSuite {

  def eval[T](code: String): Try[T] =
    Try(new Eval(None).apply[T](code))

  val showSrcIso = Prop.test[P]("showSrcIso", p => {
    val code = ShowSrc.generateExpr(p)
    eval[P](code) match {
      case Success(p2) =>
        if (p ≠ p2) {
          def check[A: Equal: ShowSrc](f: P => A): Unit = {
            val expect = f(p)
            val actual = f(p2)
            assertEq(ShowSrc.generateExpr(expect), actual, expect)
          }
          check(_.config.customIssueTypes)
          check(_.config.reqTypes)
          check(_.config.tags)
          check(_.config.fields)
          check(_.content.reqs)
          check(_.content.reqCodes)
          check(_.content.implications)
          check(_.content.reqTags)
          check(_.content.reqText)
          check(_.config)
          check(identity)
        }

      case Failure(t) =>
        println("="*120)
        println(t.getMessage)
        println()
        println(code)
        println("="*120)
        throw t
    }

    true
  })

  val Threads = sys.runtime.availableProcessors() - 1

  implicit val settings = DefaultSettings.propSettings
    .setSampleSize(Threads * 2)
    .setGenSize(10)
    //.setDebug
    //.setSeed(5)

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
*/
