package shipreq.webapp.base.util

import nyaya.prop._
import nyaya.test.PropTest._
import scalaz.std.list._
import shipreq.base.util.TransitiveClosure
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import utest._

object TransitiveClosureTest extends TestSuite {

  case class Tester(tt: TagTree) {
    val E  = EvalOver(this)
    val tc = TransitiveClosure.auto(tt.valuesIterator.map(_.id))(tt.need(_).children)

    def test =
      E.forall(tt.values.toList) { t =>
        val r = tc(t.id)
        E.equal("Same results", r, t.transitiveChildren(tt)) ∧
        E.equal("Non-reflexive", tc.nonRefl(t.id), r - t.id)
      }
  }

  def gen = RandomData.tagTree(RandomData.reqTypeId.set(0 to 4)).map(Tester)

  override def tests = Tests {
    gen.mustSatisfyE(_.test)
  }
}
