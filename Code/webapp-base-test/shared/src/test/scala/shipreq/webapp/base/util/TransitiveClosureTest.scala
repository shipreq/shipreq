package shipreq.webapp.base.util

import japgolly.nyaya._
import japgolly.nyaya.test.PropTest._
import shipreq.base.util.UnivEq.Implicits._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import utest._

import scalaz.std.stream._

object TransitiveClosureTest extends TestSuite {

  case class Tester(tt: TagTree) {
    val E  = EvalOver(this)
    val tc = TransitiveClosure.auto(tt.vstream(_.id))(tt.need(_).children, _ => true)

    def test =
      E.forall(tt.values.toStream) { t =>
        val r = tc(t.id)
        E.equal("Same results", r, t.transitiveChildren(tt)) ∧
        E.equal("Non-reflexive", tc.nonRefl(t.id), r - t.id)
      }
  }

  def gen = RandomData.tagTree.map(Tester)

  override def tests = TestSuite {
    gen.mustSatisfyE(_.test)
  }
}
