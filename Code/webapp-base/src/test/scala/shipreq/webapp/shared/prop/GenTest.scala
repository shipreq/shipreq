package shipreq.webapp.shared.prop

import shipreq.webapp.shared.TestUtil._
import shipreq.base.prop._
import utest._

object GenTest extends TestSuite {

  val prop = Prop[List[Int]]("distinct ints", is => is.distinct == is)
  val intGen = Gen.chooseint(0,5).list.lim(10).distinct(Distinct.int)

  override def tests = TestSuite {

    'distinct {
      prop mustBeSatisfiedBy intGen
    }
  }
}
